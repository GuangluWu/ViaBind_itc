#!/usr/bin/env node

import { spawn } from "node:child_process";
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
  const tmpRoot = requestedWorkDir
    ? path.resolve(requestedWorkDir)
    : fs.mkdtempSync(path.join(os.tmpdir(), "itcsuite-packaged-smoke-"));
  const roamingRoot = path.join(tmpRoot, "appdata-roaming");
  const localRoot = path.join(tmpRoot, "appdata-local");
  const homeRoot = path.join(tmpRoot, "home");
  fs.rmSync(tmpRoot, { recursive: true, force: true });
  fs.mkdirSync(tmpRoot, { recursive: true });
  fs.mkdirSync(roamingRoot, { recursive: true });
  fs.mkdirSync(localRoot, { recursive: true });
  fs.mkdirSync(homeRoot, { recursive: true });

  const logsDir = path.join(roamingRoot, "ViaBind", "logs");
  const mainLogPath = path.join(logsDir, "main.log");
  const backendLogPath = path.join(logsDir, "backend.log");

  process.stdout.write(`ITCSUITE_PACKAGED_SMOKE_ROOT ${tmpRoot}\n`);

  let stdout = "";
  let stderr = "";

  await new Promise((resolve, reject) => {
    const smokeArgs = process.platform === "win32"
      ? ["--disable-gpu", "--disable-gpu-compositing", "--disable-software-rasterizer", "--smoke-test"]
      : ["--smoke-test"];

    const child = spawn(exePath, smokeArgs, {
      cwd: path.dirname(exePath),
      env: {
        ...process.env,
        APPDATA: roamingRoot,
        LOCALAPPDATA: localRoot,
        USERPROFILE: homeRoot,
        HOME: homeRoot,
        ITCSUITE_SMOKE_TEST: "1",
        ELECTRON_ENABLE_LOGGING: "1"
      },
      stdio: ["ignore", "pipe", "pipe"],
      windowsHide: true
    });

    const timeout = setTimeout(() => {
      try {
        child.kill("SIGKILL");
      } catch (_) {
        // Ignore signal failures.
      }
      reject(new Error(`Packaged smoke timed out. main.log tail:\n${tailText(readTextIfExists(mainLogPath))}`));
    }, 180000);

    child.stdout.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      stdout += text;
      process.stdout.write(text);
    });

    child.stderr.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      stderr += text;
      process.stderr.write(text);
    });

    child.on("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });

    child.on("close", (code) => {
      clearTimeout(timeout);
      if (code === 0) {
        resolve();
      } else {
        const mainLogTail = tailText(readTextIfExists(mainLogPath));
        const backendLogTail = tailText(readTextIfExists(backendLogPath));
        reject(
          new Error(
            [
              `Packaged smoke exited with code ${code}.`,
              mainLogTail ? `main.log tail:\n${mainLogTail}` : "",
              backendLogTail ? `backend.log tail:\n${backendLogTail}` : "",
              stderr ? `stderr tail:\n${tailText(stderr)}` : ""
            ].filter(Boolean).join("\n\n")
          )
        );
      }
    });
  });

  const mainLog = readTextIfExists(mainLogPath);
  const sawStdoutMarker = stdout.includes("ITCSUITE_ELECTRON_SMOKE");
  const sawBackendPageLoad = mainLog.includes("\"event\":\"backend_page_load_success\"");

  if (!sawStdoutMarker && !sawBackendPageLoad) {
    throw new Error(
      [
        "Packaged smoke completed without expected success signals.",
        `Expected stdout marker or backend_page_load_success in ${mainLogPath}.`,
        mainLog ? `main.log tail:\n${tailText(mainLog)}` : "main.log missing or empty."
      ].join("\n\n")
    );
  }

  const payload = JSON.stringify({
    exe: exePath,
    logs_dir: logsDir,
    stdout_marker: sawStdoutMarker,
    backend_page_load_success: sawBackendPageLoad
  });
  process.stdout.write(`ITCSUITE_PACKAGED_SMOKE ${payload}\n`);
}

main().catch((error) => {
  console.error(`Packaged smoke failed: ${error.message}`);
  process.exit(1);
});
