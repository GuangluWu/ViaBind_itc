#!/usr/bin/env node

import { spawn, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import process from "node:process";

function parseArgs(argv) {
  const out = { exe: "", workDir: "" };
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (token === "--exe") {
      out.exe = argv[i + 1] || "";
      i += 1;
      continue;
    }
    if (token === "--work-dir") {
      out.workDir = argv[i + 1] || "";
      i += 1;
      continue;
    }
    throw new Error(`Unknown argument: ${token}`);
  }
  return out;
}

function readTextIfExists(filePath) {
  try {
    if (fs.existsSync(filePath)) {
      return fs.readFileSync(filePath, "utf8");
    }
  } catch (_) {
    // Best effort only.
  }
  return "";
}

function tailText(text, maxChars = 4000) {
  if (text.length <= maxChars) return text;
  return text.slice(text.length - maxChars);
}

function writeJson(filePath, payload) {
  fs.writeFileSync(filePath, JSON.stringify(payload, null, 2));
}

function resolveDefaultLogsDir() {
  if (process.platform !== "win32") {
    return path.join(os.tmpdir(), "ViaBind", "logs");
  }
  const appData = process.env.APPDATA || path.join(os.homedir(), "AppData", "Roaming");
  return path.join(appData, "ViaBind", "logs");
}

function copyFileBestEffort(src, dst) {
  try {
    if (!fs.existsSync(src)) return false;
    fs.mkdirSync(path.dirname(dst), { recursive: true });
    fs.copyFileSync(src, dst);
    return true;
  } catch (_) {
    return false;
  }
}

async function runAttempt({ exePath, cwd, smokeArgs, logsDir, timeoutMs }) {
  let stdout = "";
  let stderr = "";
  let sawMarker = false;
  let sawMarkerAt = 0;
  let settled = false;
  let successExitTimer = null;

  const mainLogPath = path.join(logsDir, "main.log");
  const backendLogPath = path.join(logsDir, "backend.log");

  function terminateChildBestEffort(child) {
    if (process.platform === "win32" && Number.isFinite(child.pid) && child.pid > 0) {
      const result = spawnSync("taskkill", ["/pid", String(child.pid), "/t", "/f"], {
        stdio: "ignore",
        windowsHide: true
      });
      if (result.error) {
        try {
          child.kill("SIGKILL");
        } catch (_) {
          // Best effort only.
        }
      }
      return;
    }

    try {
      child.kill("SIGKILL");
    } catch (_) {
      // Best effort only.
    }
  }

  const result = await new Promise((resolve, reject) => {
    const child = spawn(exePath, smokeArgs, {
      cwd,
      env: {
        ...process.env,
        ITCSUITE_SMOKE_TEST: "1",
        ELECTRON_ENABLE_LOGGING: "1",
        ELECTRON_NO_ATTACH_CONSOLE: "1"
      },
      stdio: ["ignore", "pipe", "pipe"],
      windowsHide: true
    });

    const startedAt = Date.now();
    const finish = (payload) => {
      if (settled) return;
      settled = true;
      clearTimeout(timeout);
      if (successExitTimer) {
        clearTimeout(successExitTimer);
        successExitTimer = null;
      }
      resolve(payload);
    };

    const timeout = setTimeout(() => {
      terminateChildBestEffort(child);
      finish({
        ok: false,
        code: null,
        reason: "timeout",
        durationMs: Date.now() - startedAt,
        sawMarker,
        sawMarkerAt,
        stdout,
        stderr
      });
    }, timeoutMs);

    child.stdout.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      stdout += text;
      process.stdout.write(text);
      if (!sawMarker && text.includes("ITCSUITE_ELECTRON_SMOKE")) {
        sawMarker = true;
        sawMarkerAt = Date.now();
        successExitTimer = setTimeout(() => {
          terminateChildBestEffort(child);
          finish({
            ok: true,
            code: null,
            reason: "marker_detected",
            durationMs: Date.now() - startedAt,
            sawMarker,
            sawMarkerAt,
            stdout,
            stderr
          });
        }, 1500);
      }
    });

    child.stderr.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      stderr += text;
      process.stderr.write(text);
    });

    child.on("error", (error) => {
      if (settled) return;
      if (successExitTimer) {
        clearTimeout(successExitTimer);
        successExitTimer = null;
      }
      clearTimeout(timeout);
      reject(error);
    });

    child.on("close", (code) => {
      finish({
        ok: code === 0,
        code: Number.isFinite(code) ? code : null,
        reason: code === 0 ? "exit_0" : "nonzero_exit",
        durationMs: Date.now() - startedAt,
        sawMarker,
        sawMarkerAt,
        stdout,
        stderr
      });
    });
  });

  const mainLog = readTextIfExists(mainLogPath);
  const backendLog = readTextIfExists(backendLogPath);
  const sawStdoutMarker = result.stdout.includes("ITCSUITE_ELECTRON_SMOKE");
  const sawBackendPageLoad = mainLog.includes("\"event\":\"backend_page_load_success\"");

  return {
    ...result,
    smokeArgs,
    markerDetectedAt: result.sawMarkerAt ? new Date(result.sawMarkerAt).toISOString() : null,
    sawStdoutMarker,
    sawBackendPageLoad,
    mainLogTail: tailText(mainLog),
    backendLogTail: tailText(backendLog),
    stderrTail: tailText(result.stderr),
    stdoutTail: tailText(result.stdout)
  };
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  if (!args.exe) {
    throw new Error("Missing required --exe argument");
  }

  const exePath = path.resolve(args.exe);
  if (!fs.existsSync(exePath)) {
    throw new Error(`Packaged app missing: ${exePath}`);
  }

  const requestedWorkDir = args.workDir || process.env.ITCSUITE_SMOKE_ROOT || "";
  const smokeRoot = requestedWorkDir
    ? path.resolve(requestedWorkDir)
    : fs.mkdtempSync(path.join(os.tmpdir(), "itcsuite-packaged-smoke-"));
  fs.rmSync(smokeRoot, { recursive: true, force: true });
  fs.mkdirSync(smokeRoot, { recursive: true });

  const logsDir = resolveDefaultLogsDir();
  const diagDir = path.join(smokeRoot, "diag");
  fs.mkdirSync(diagDir, { recursive: true });
  process.stdout.write(`ITCSUITE_PACKAGED_SMOKE_ROOT ${smokeRoot}\n`);
  process.stdout.write(`ITCSUITE_PACKAGED_LOGS_DIR ${logsDir}\n`);

  const attempts = [];
  const launchProfiles = process.platform === "win32"
    ? [
        {
          name: "safe-flags",
          args: [
            "--smoke-test",
            "--no-sandbox",
            "--disable-gpu",
            "--disable-gpu-compositing",
            "--disable-features=RendererCodeIntegrity,CalculateNativeWinOcclusion"
          ]
        },
        {
          name: "minimal-flags",
          args: ["--smoke-test"]
        }
      ]
    : [
        { name: "default", args: ["--smoke-test"] }
      ];

  for (const profile of launchProfiles) {
    const attempt = await runAttempt({
      exePath,
      cwd: path.dirname(exePath),
      smokeArgs: profile.args,
      logsDir,
      timeoutMs: 180000
    });
    attempts.push({
      profile: profile.name,
      ...attempt
    });

    if (attempt.ok && (attempt.sawStdoutMarker || attempt.sawBackendPageLoad)) {
      const summary = {
        status: "success",
        exe: exePath,
        logs_dir: logsDir,
        chosen_profile: profile.name,
        attempts: attempts.map((x) => ({
          profile: x.profile,
          code: x.code,
          reason: x.reason,
          duration_ms: x.durationMs,
          saw_stdout_marker: x.sawStdoutMarker,
          saw_backend_page_load: x.sawBackendPageLoad
        }))
      };
      copyFileBestEffort(path.join(logsDir, "main.log"), path.join(diagDir, "main.log"));
      copyFileBestEffort(path.join(logsDir, "backend.log"), path.join(diagDir, "backend.log"));
      writeJson(path.join(diagDir, "packaged-smoke-summary.json"), summary);
      process.stdout.write(`ITCSUITE_PACKAGED_SMOKE ${JSON.stringify(summary)}\n`);
      process.exit(0);
    }
  }

  copyFileBestEffort(path.join(logsDir, "main.log"), path.join(diagDir, "main.log"));
  copyFileBestEffort(path.join(logsDir, "backend.log"), path.join(diagDir, "backend.log"));

  const failureSummary = {
    status: "failed",
    exe: exePath,
    logs_dir: logsDir,
    attempts: attempts.map((x) => ({
      profile: x.profile,
      code: x.code,
      reason: x.reason,
      duration_ms: x.durationMs,
      saw_stdout_marker: x.sawStdoutMarker,
      saw_backend_page_load: x.sawBackendPageLoad,
      stdout_tail: x.stdoutTail,
      stderr_tail: x.stderrTail,
      main_log_tail: x.mainLogTail,
      backend_log_tail: x.backendLogTail
    }))
  };
  writeJson(path.join(diagDir, "packaged-smoke-summary.json"), failureSummary);

  const concise = attempts.map((x) => `${x.profile}: code=${x.code} reason=${x.reason}`).join("; ");
  throw new Error(`Packaged smoke failed after ${attempts.length} attempt(s). ${concise}`);
}

main().catch((error) => {
  console.error(`Packaged smoke failed: ${error.message}`);
  process.exit(1);
});
