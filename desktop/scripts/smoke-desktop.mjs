import { spawn, spawnSync } from "node:child_process";
import fs from "node:fs";
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
    if (fs.existsSync(windowsExe)) {
      return { command: windowsExe, useShell: false, label: "electron.exe" };
    }
    if (fs.existsSync(windowsCmd)) {
      return { command: windowsCmd, useShell: true, label: "electron.cmd" };
    }
    return { command: windowsExe, useShell: false, label: "electron.exe" };
  }

  return { command: unixBin, useShell: false, label: "electron" };
}

function resolveSmokeRoot() {
  const fromEnv = process.env.ITCSUITE_SMOKE_ROOT;
  if (fromEnv && fromEnv.trim()) return path.resolve(fromEnv);
  return path.join(desktopDir, "dist", "smoke-logs", "desktop-smoke");
}

const smokeRoot = resolveSmokeRoot();
const smokeLogPath = path.join(smokeRoot, "desktop-smoke.log");
const smokeSummaryPath = path.join(smokeRoot, "desktop-smoke-summary.json");

fs.rmSync(smokeRoot, { recursive: true, force: true });
fs.mkdirSync(smokeRoot, { recursive: true });
process.stdout.write(`ITCSUITE_DESKTOP_SMOKE_ROOT ${smokeRoot}\n`);

const startedAt = Date.now();

function writeLogLine(payload) {
  const line = `${JSON.stringify(payload)}\n`;
  fs.appendFileSync(smokeLogPath, line);
}

function logEvent(event, details = {}) {
  writeLogLine({
    ts: new Date().toISOString(),
    event,
    ...details
  });
}

function buildSummary(status, details = {}) {
  return {
    status,
    smoke_root: smokeRoot,
    duration_ms: Date.now() - startedAt,
    ...details
  };
}

const electronLaunch = resolveElectronLaunch();
const electronBin = electronLaunch.command;

if (!fs.existsSync(electronBin)) {
  const message = `Smoke failed: electron binary missing (${electronBin}). Run npm install first.`;
  logEvent("launch_missing", { command: electronBin });
  fs.writeFileSync(smokeSummaryPath, JSON.stringify(buildSummary("failed", { reason: "missing_electron" }), null, 2));
  console.error(message);
  process.exit(1);
}

let child;
try {
  child = spawn(electronBin, [".", "--smoke-test"], {
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
} catch (error) {
  const message = `Smoke failed to spawn electron: ${error.message}`;
  logEvent("spawn_error", { message });
  fs.writeFileSync(smokeSummaryPath, JSON.stringify(buildSummary("failed", { reason: "spawn_error", message: error.message }), null, 2));
  console.error(message);
  process.exit(1);
}

logEvent("launched", {
  command: electronBin,
  command_label: electronLaunch.label,
  shell: electronLaunch.useShell,
  pid: Number.isFinite(child.pid) ? child.pid : null
});

let stdout = "";
let stderr = "";
let sawSmokeMarker = false;
let markerAt = 0;
let markerPayloadRaw = "";
let forcedTerminateTriggered = false;
let closeCode = null;
let didTimeout = false;
let finalized = false;
let successExitTimer = null;

function clearTimers(timeoutTimer) {
  clearTimeout(timeoutTimer);
  if (successExitTimer) {
    clearTimeout(successExitTimer);
    successExitTimer = null;
  }
}

function terminateChildBestEffort() {
  if (closeCode !== null) return false;
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
    return true;
  }

  try {
    child.kill("SIGKILL");
    return true;
  } catch (_) {
    return false;
  }
}

function finalizeAndExit(timeoutTimer, status, details = {}) {
  if (finalized) return;
  finalized = true;
  clearTimers(timeoutTimer);
  const summary = buildSummary(status, {
    command: electronBin,
    command_label: electronLaunch.label,
    shell: electronLaunch.useShell,
    pid: Number.isFinite(child.pid) ? child.pid : null,
    saw_marker: sawSmokeMarker,
    marker_at: markerAt ? new Date(markerAt).toISOString() : null,
    marker_payload: markerPayloadRaw || null,
    forced_terminate: forcedTerminateTriggered,
    timeout_triggered: didTimeout,
    close_code: closeCode,
    ...details
  });
  fs.writeFileSync(smokeSummaryPath, JSON.stringify(summary, null, 2));
  process.stdout.write(`ITCSUITE_DESKTOP_SMOKE_RESULT ${JSON.stringify(summary)}\n`);
  process.exit(status === "success" ? 0 : 1);
}

const timeoutTimer = setTimeout(() => {
  didTimeout = true;
  forcedTerminateTriggered = terminateChildBestEffort() || forcedTerminateTriggered;
  logEvent("timeout", {
    timeout_ms: 120000,
    forced_terminate: forcedTerminateTriggered
  });
  finalizeAndExit(timeoutTimer, "failed", { reason: "timeout" });
}, 120000);

function scheduleDeterministicSuccessExit() {
  if (successExitTimer) return;
  successExitTimer = setTimeout(() => {
    forcedTerminateTriggered = terminateChildBestEffort() || forcedTerminateTriggered;
    logEvent("marker_success_exit", { forced_terminate: forcedTerminateTriggered });
    finalizeAndExit(timeoutTimer, "success", { reason: "marker_detected" });
  }, 1500);
}

child.stdout.on("data", (chunk) => {
  const text = chunk.toString("utf8");
  stdout += text;
  process.stdout.write(text);
  if (!sawSmokeMarker && text.includes("ITCSUITE_ELECTRON_SMOKE")) {
    sawSmokeMarker = true;
    markerAt = Date.now();
    const markerLine = text.split(/\r?\n/).find((line) => line.includes("ITCSUITE_ELECTRON_SMOKE")) || "";
    markerPayloadRaw = markerLine.trim();
    logEvent("marker_detected", { marker_line: markerPayloadRaw });
    scheduleDeterministicSuccessExit();
  }
});

child.stderr.on("data", (chunk) => {
  const text = chunk.toString("utf8");
  stderr += text;
  process.stderr.write(text);
});

child.on("close", (code) => {
  closeCode = Number.isFinite(code) ? code : null;
  logEvent("child_close", { code: closeCode, saw_marker: sawSmokeMarker });
  if (sawSmokeMarker) {
    finalizeAndExit(timeoutTimer, "success", { reason: "marker_detected" });
    return;
  }
  finalizeAndExit(timeoutTimer, "failed", {
    reason: "child_exit_without_marker",
    stderr_tail: stderr.slice(-4000),
    stdout_tail: stdout.slice(-4000)
  });
});

child.on("error", (error) => {
  logEvent("child_error", { message: error.message });
  finalizeAndExit(timeoutTimer, "failed", { reason: "child_error", message: error.message });
});
