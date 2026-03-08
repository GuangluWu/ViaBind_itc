import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
import process from "node:process";

const scriptPath = fileURLToPath(import.meta.url);
const desktopDir = path.resolve(path.dirname(scriptPath), "..");

function resolveElectronLaunch() {
  const windowsExe = path.join(desktopDir, "node_modules", "electron", "dist", "electron.exe");
  const windowsCmd = path.join(desktopDir, "node_modules", ".bin", "electron.cmd");
  const unixBin = path.join(desktopDir, "node_modules", ".bin", "electron");

  if (process.platform === "win32") {
    if (existsSync(windowsExe)) {
      return { command: windowsExe, useShell: false, label: "electron.exe" };
    }
    if (existsSync(windowsCmd)) {
      return { command: windowsCmd, useShell: true, label: "electron.cmd" };
    }
    return { command: windowsExe, useShell: false, label: "electron.exe" };
  }

  return { command: unixBin, useShell: false, label: "electron" };
}

const electronLaunch = resolveElectronLaunch();
const electronBin = electronLaunch.command;

if (!existsSync(electronBin)) {
  console.error(`Smoke failed: electron binary missing (${electronBin}). Run npm install first.`);
  process.exit(1);
}

const child = spawn(electronBin, [".", "--smoke-test"], {
  cwd: desktopDir,
  env: {
    ...process.env,
    ITCSUITE_SMOKE_TEST: "1",
    ITCSUITE_USE_BUNDLED_R: process.env.ITCSUITE_USE_BUNDLED_R || "0"
  },
  stdio: ["ignore", "pipe", "pipe"],
  shell: electronLaunch.useShell,
  windowsHide: true
});

let stdout = "";
let stderr = "";
let sawSmokeMarker = false;
let forcedSuccess = false;
let successShutdownTimer = null;

const timeout = setTimeout(() => {
  child.kill("SIGKILL");
}, 120000);

function clearSuccessShutdownTimer() {
  if (successShutdownTimer) {
    clearTimeout(successShutdownTimer);
    successShutdownTimer = null;
  }
}

function forceTerminateChildTree() {
  if (child.killed) return;
  forcedSuccess = true;

  if (process.platform === "win32" && Number.isFinite(child.pid) && child.pid > 0) {
    const killer = spawn("taskkill", ["/pid", String(child.pid), "/t", "/f"], {
      stdio: "ignore",
      windowsHide: true
    });
    killer.on("error", () => {
      try {
        child.kill("SIGKILL");
      } catch (_) {
        // Ignore signal failures.
      }
    });
    return;
  }

  try {
    child.kill("SIGKILL");
  } catch (_) {
    // Ignore signal failures.
  }
}

child.stdout.on("data", (chunk) => {
  const text = chunk.toString("utf8");
  stdout += text;
  process.stdout.write(text);
  if (text.includes("ITCSUITE_ELECTRON_SMOKE")) {
    sawSmokeMarker = true;
    if (!successShutdownTimer) {
      successShutdownTimer = setTimeout(() => {
        forceTerminateChildTree();
      }, 5000);
    }
  }
});

child.stderr.on("data", (chunk) => {
  const text = chunk.toString("utf8");
  stderr += text;
  process.stderr.write(text);
});

child.on("close", (code) => {
  clearSuccessShutdownTimer();
  clearTimeout(timeout);
  if (forcedSuccess && sawSmokeMarker) {
    process.exit(0);
  }
  if (code !== 0) {
    console.error(`Smoke failed: electron exited with code ${code}.`);
    process.exit(code ?? 1);
  }
  if (!sawSmokeMarker) {
    console.error("Smoke failed: marker ITCSUITE_ELECTRON_SMOKE not found in stdout.");
    process.exit(1);
  }
  process.exit(0);
});

child.on("error", (err) => {
  clearTimeout(timeout);
  console.error(`Smoke failed: ${err.message}`);
  process.exit(1);
});
