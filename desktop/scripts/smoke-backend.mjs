#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const READY_PREFIX = "ITCSUITE_READY ";
const ERROR_PREFIX = "ITCSUITE_ERROR ";
const HOST = "127.0.0.1";

const __filename = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(__filename);
const desktopDir = path.resolve(scriptDir, "..");
const repoRoot = path.resolve(desktopDir, "..");
const runtimeRoot = process.env.ITCSUITE_RUNTIME_ROOT || path.join(desktopDir, "resources", "r-runtime");
const launchScript = path.join(repoRoot, "ITCSuiteWeb", "scripts", "launch_shiny.R");

function resolveRscript(runtimeDir) {
  const candidates = [
    path.join(runtimeDir, "bin", "itcsuite-rscript"),
    path.join(runtimeDir, "bin", "Rscript.exe"),
    path.join(runtimeDir, "bin", "x64", "Rscript.exe"),
    path.join(runtimeDir, "bin", "Rscript"),
    path.join(runtimeDir, "Resources", "bin", "Rscript")
  ];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  return "";
}

function probeBackend(port) {
  return new Promise((resolve) => {
    const req = http.get(
      {
        host: HOST,
        port,
        path: "/",
        timeout: 2000
      },
      (res) => {
        res.resume();
        resolve(true);
      }
    );
    req.on("timeout", () => {
      req.destroy();
      resolve(false);
    });
    req.on("error", () => resolve(false));
  });
}

async function waitForBackendReady(port, timeoutMs = 30000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const ok = await probeBackend(port);
    if (ok) return true;
    await new Promise((resolve) => setTimeout(resolve, 500));
  }
  return false;
}

function delay(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function runTaskkill(pid) {
  await new Promise((resolve) => {
    const killer = spawn("taskkill", ["/PID", String(pid), "/T", "/F"], {
      stdio: "ignore"
    });
    killer.on("error", () => resolve());
    killer.on("close", () => resolve());
  });
}

async function stopChild(child) {
  if (!child) return;
  const pid = child.pid;

  const waitForClose = new Promise((resolve) => {
    child.once("close", () => resolve(true));
  });

  try {
    child.kill("SIGTERM");
  } catch (_) {
    // Ignore signal failures.
  }

  let exited = await Promise.race([
    waitForClose,
    delay(5000).then(() => false)
  ]);
  if (exited) return;

  if (process.platform === "win32" && Number.isInteger(pid) && pid > 0) {
    await runTaskkill(pid);
  } else {
    try {
      child.kill("SIGKILL");
    } catch (_) {
      // Ignore signal failures.
    }
  }

  exited = await Promise.race([
    waitForClose,
    delay(5000).then(() => false)
  ]);

  if (!exited) {
    // Avoid hanging forever if OS refuses to terminate the process tree.
    if (child.stdout && typeof child.stdout.destroy === "function") {
      child.stdout.destroy();
    }
    if (child.stderr && typeof child.stderr.destroy === "function") {
      child.stderr.destroy();
    }
  }
}

async function terminateOnSignal(child, signalName) {
  process.once(signalName, () => {
    void stopChild(child).finally(() => {
      try {
        process.exit(130);
      } catch (_) {
        process.exit(1);
      }
    });
  });
}

async function main() {
  if (!fs.existsSync(launchScript)) {
    throw new Error(`launch_shiny.R missing: ${launchScript}`);
  }
  if (!fs.existsSync(runtimeRoot)) {
    throw new Error(`Bundled runtime missing: ${runtimeRoot}. Run node scripts/build-r-runtime.mjs first.`);
  }

  const rscript = resolveRscript(runtimeRoot);
  if (!rscript) {
    throw new Error(`Bundled Rscript missing under ${runtimeRoot}`);
  }

  const smokeLogDir = path.join(os.tmpdir(), `itcsuite-backend-smoke-${Date.now()}`);
  fs.mkdirSync(smokeLogDir, { recursive: true });
  const isolatedUserLib = path.join(smokeLogDir, "r-library-empty");
  fs.mkdirSync(isolatedUserLib, { recursive: true });
  const bundledLib = path.join(runtimeRoot, "library");

  const args = [
    launchScript,
    "--repo-root", repoRoot,
    "--app-dir", "ITCSuiteWeb",
    "--host", HOST,
    "--port", "0",
    "--log-dir", smokeLogDir
  ];

  const env = {
    ...process.env,
    ITCSUITE_DESKTOP: "1",
    ITCSUITE_RSCRIPT: rscript,
    ITCSUITE_RUNTIME_ROOT: runtimeRoot,
    R_HOME: runtimeRoot,
    R_LIBS: bundledLib,
    R_LIBS_SITE: bundledLib,
    R_LIBS_USER: isolatedUserLib,
    PATH: [
      path.join(runtimeRoot, "bin", "x64"),
      path.join(runtimeRoot, "bin"),
      process.env.PATH || ""
    ].filter(Boolean).join(path.delimiter)
  };

  const child = spawn(rscript, args, {
    cwd: repoRoot,
    env,
    stdio: ["ignore", "pipe", "pipe"]
  });

  await terminateOnSignal(child, "SIGINT");
  await terminateOnSignal(child, "SIGTERM");

  let resolved = false;
  let readyPort = null;
  let stdoutBuffer = "";

  const readyPromise = new Promise((resolve, reject) => {
    const startupTimeout = setTimeout(() => {
      reject(new Error("Backend startup timed out (120s)."));
    }, 120000);

    child.stdout.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      process.stdout.write(text);
      stdoutBuffer += text;

      let idx = stdoutBuffer.indexOf("\n");
      while (idx >= 0) {
        const line = stdoutBuffer.slice(0, idx).trim();
        stdoutBuffer = stdoutBuffer.slice(idx + 1);

        if (line.startsWith(READY_PREFIX)) {
          try {
            const payload = JSON.parse(line.slice(READY_PREFIX.length));
            const port = Number(payload.port);
            if (!Number.isFinite(port) || port <= 0) {
              throw new Error("invalid port in READY payload");
            }
            readyPort = port;
            resolved = true;
            clearTimeout(startupTimeout);
            resolve(port);
          } catch (error) {
            clearTimeout(startupTimeout);
            reject(new Error(`Malformed READY payload: ${error.message}`));
          }
          return;
        }

        if (line.startsWith(ERROR_PREFIX)) {
          clearTimeout(startupTimeout);
          reject(new Error(line));
          return;
        }

        idx = stdoutBuffer.indexOf("\n");
      }
    });

    child.stderr.on("data", (chunk) => {
      process.stderr.write(chunk.toString("utf8"));
    });

    child.on("error", (error) => {
      clearTimeout(startupTimeout);
      reject(error);
    });

    child.on("close", (code) => {
      if (resolved) return;
      clearTimeout(startupTimeout);
      reject(new Error(`Backend exited before READY with code ${code}`));
    });
  });

  try {
    const port = await readyPromise;
    const reachable = await waitForBackendReady(port, 30000);
    if (!reachable) {
      throw new Error(`Backend did not become reachable on ${HOST}:${port}`);
    }
    console.log(`ITCSUITE_BACKEND_SMOKE {"port":${port},"log_dir":"${smokeLogDir.replace(/\\/g, "\\\\")}"}`);
  } finally {
    await stopChild(child);
  }

  if (!readyPort) {
    throw new Error("Backend never reported ready port.");
  }
}

main().catch((error) => {
  console.error(`Smoke failed: ${error.message}`);
  process.exit(1);
});
