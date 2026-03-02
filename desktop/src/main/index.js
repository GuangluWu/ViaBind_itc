const { app, BrowserWindow, Menu, dialog, shell, session, nativeImage, powerMonitor, ipcMain } = require("electron");
const { spawn, spawnSync } = require("child_process");
const http = require("http");
const path = require("path");
const fs = require("fs");
const EventEmitter = require("events");
const os = require("os");
const {
  sanitizeDiagnosticsRequest,
  makeDiagnosticsResult,
  copyRedactedLog,
  collectDiagnosticsFiles,
  sha256File,
  buildManifest
} = require("./diagnostics");

const HOST = "127.0.0.1";
const READY_PREFIX = "ITCSUITE_READY ";
const ERROR_PREFIX = "ITCSUITE_ERROR ";
const SMOKE_PREFIX = "ITCSUITE_ELECTRON_SMOKE ";
const APP_NAME = "ViaBind";
const APP_SLOGAN = "Your Path, Your Model";
const APP_TITLE = `${APP_NAME}: ${APP_SLOGAN}`;
const APP_DEVELOPER_NAME = "Guanglu Wu (吴光鹭)";
const APP_DEVELOPER_EMAIL = "guanglu.wu@gmail.com";
const APP_DEVELOPER_SITE = "guanglu.xyz";
const appVersionSignature = () => `ViaBind v${app.getVersion()}`;
const OPEN_FILE_CHANNEL = "itcsuite:open-file";
const EXPORT_DIAGNOSTICS_CHANNEL = "itcsuite:export-diagnostics";
const OPEN_FILE_PURPOSES = new Set(["step1_import", "step2_import", "step3_import"]);
app.setName(APP_NAME);

const smokeMode = process.argv.includes("--smoke-test") || process.env.ITCSUITE_SMOKE_TEST === "1";

let mainWindow = null;
let backend = null;
let allowedUrlPrefix = null;
let logsDir = null;
let mainLogPath = null;
let isQuitting = false;
let smokeReported = false;
let recoveryInProgress = false;
let lastRecoveryAt = 0;
let unresponsiveRecoveryTimer = null;
let rendererMarkedUnresponsive = false;
let openFileLastDir = "";
let diagnosticsExportInFlight = false;
let latestPowerEventPayload = null;

const RECOVERY_COOLDOWN_MS = 10000;
const UNRESPONSIVE_RECOVERY_DELAY_MS = 3000;
const POWER_EVENT_REPLAY_WINDOW_MS = 3 * 60 * 1000;
const WINDOWS_SCROLLBAR_HIDE_CSS = `
  * {
    scrollbar-width: none !important;
  }
  *::-webkit-scrollbar {
    width: 0 !important;
    height: 0 !important;
  }
`;

function useBundledRuntimeInDev() {
  return process.env.ITCSUITE_USE_BUNDLED_R === "1";
}

function appendMainLog(eventName, details = {}) {
  if (!mainLogPath) return;

  const payload = {
    ts: new Date().toISOString(),
    event: eventName,
    ...details
  };

  try {
    fs.appendFileSync(mainLogPath, `${JSON.stringify(payload)}\n`);
  } catch (_) {
    // Best effort logging.
  }
}

function resolveAppIconPath() {
  const candidates = app.isPackaged
    ? [path.join(process.resourcesPath, "icon.png")]
    : [
        path.resolve(__dirname, "../../../icons/ViaBind_1024.png")
      ];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  return null;
}

class BackendController extends EventEmitter {
  constructor(options) {
    super();
    this.options = options;
    this.state = "stopped";
    this.child = null;
    this.stdoutBuffer = "";
    this.readyTimeout = null;
    this.readyPort = null;
  }

  transition(nextState) {
    this.state = nextState;
  }

  resolvePaths() {
    if (app.isPackaged) {
      const repoRoot = path.join(process.resourcesPath, "itcsuite");
      const launchScript = path.join(repoRoot, "ITCSuiteWeb", "scripts", "launch_shiny.R");
      const runtimeRoot = path.join(process.resourcesPath, "r-runtime");
      return { repoRoot, launchScript, runtimeRoot };
    }

    const repoRoot = process.env.ITCSUITE_REPO_ROOT || path.resolve(__dirname, "../../..");
    const launchScript = path.join(repoRoot, "ITCSuiteWeb", "scripts", "launch_shiny.R");
    const runtimeRoot = path.join(repoRoot, "desktop", "resources", "r-runtime");
    return { repoRoot, launchScript, runtimeRoot };
  }

  resolveRscript(runtimeRoot) {
    if (process.env.ITCSUITE_RSCRIPT) {
      return process.env.ITCSUITE_RSCRIPT;
    }

    // In development we prefer system Rscript to avoid accidentally
    // using a partially built bundled runtime.
    if (!app.isPackaged && !useBundledRuntimeInDev()) {
      return "Rscript";
    }

    const bundledCandidates = [
      ...(app.isPackaged ? [path.join(runtimeRoot, "bin", "itcsuite-rscript")] : []),
      path.join(runtimeRoot, "bin", "Rscript.exe"),
      path.join(runtimeRoot, "bin", "x64", "Rscript.exe"),
      path.join(runtimeRoot, "bin", "Rscript"),
      path.join(runtimeRoot, "Resources", "bin", "Rscript")
    ];

    for (const candidate of bundledCandidates) {
      if (fs.existsSync(candidate)) {
        return candidate;
      }
    }

    if (app.isPackaged) {
      throw new Error(`Bundled Rscript not found under ${runtimeRoot}`);
    }

    return "Rscript";
  }

  buildEnv(runtimeRoot) {
    const env = { ...process.env };
    env.ITCSUITE_DESKTOP = "1";
    env.ITCSUITE_USER_DATA_DIR = app.getPath("userData");
    env.ITCSUITE_APP_VERSION = app.getVersion();

    const bundledLib = path.join(runtimeRoot, "library");
    if (fs.existsSync(bundledLib) && (app.isPackaged || useBundledRuntimeInDev())) {
      env.R_HOME = runtimeRoot;
      env.R_LIBS = bundledLib;
      env.R_LIBS_USER = bundledLib;
      env.R_LIBS_SITE = bundledLib;

      const runtimeBins = [
        path.join(runtimeRoot, "bin", "x64"),
        path.join(runtimeRoot, "bin"),
        path.join(runtimeRoot, "Resources", "bin")
      ].filter((candidate) => fs.existsSync(candidate));

      if (runtimeBins.length > 0) {
        const currentPath = env.PATH || env.Path || "";
        const prefixedPath = runtimeBins.join(path.delimiter);
        env.PATH = currentPath ? `${prefixedPath}${path.delimiter}${currentPath}` : prefixedPath;
        if (Object.prototype.hasOwnProperty.call(env, "Path")) {
          env.Path = env.PATH;
        }
      }
    }

    // Preserve Unicode rendering in bundled runtime on Unix-like hosts.
    if (process.platform !== "win32") {
      if (!env.LANG || !/UTF-?8/i.test(env.LANG)) {
        env.LANG = "en_US.UTF-8";
      }
      if (!env.LC_CTYPE || !/UTF-?8/i.test(env.LC_CTYPE)) {
        env.LC_CTYPE = "en_US.UTF-8";
      }
    }

    return env;
  }

  processLine(line) {
    const trimmed = line.trim();
    if (!trimmed) return;

    if (trimmed.startsWith(READY_PREFIX)) {
      const payloadRaw = trimmed.slice(READY_PREFIX.length);
      try {
        const payload = JSON.parse(payloadRaw);
        const port = Number(payload.port);
        if (!Number.isFinite(port) || port <= 0) {
          throw new Error("invalid ready payload");
        }
        this.readyPort = port;
        clearTimeout(this.readyTimeout);
        this.transition("ready");
        this.emit("backend-ready", port);
      } catch (error) {
        this.emit("backend-error", `Malformed READY payload: ${error.message}`);
      }
      return;
    }

    if (trimmed.startsWith(ERROR_PREFIX)) {
      const payloadRaw = trimmed.slice(ERROR_PREFIX.length);
      let message = payloadRaw;
      try {
        const payload = JSON.parse(payloadRaw);
        message = payload.message || payloadRaw;
      } catch (_) {
        // Keep plain message fallback.
      }
      this.emit("backend-error", message);
    }
  }

  appendLog(line) {
    try {
      fs.appendFileSync(this.options.backendLogPath, line);
    } catch (_) {
      // Best effort logging.
    }
  }

  async start() {
    if (this.child) {
      return;
    }

    const { repoRoot, launchScript, runtimeRoot } = this.resolvePaths();
    if (!fs.existsSync(launchScript)) {
      throw new Error(`launch_shiny.R missing: ${launchScript}`);
    }

    const rscript = this.resolveRscript(runtimeRoot);
    const args = [
      launchScript,
      "--repo-root", repoRoot,
      "--app-dir", "ITCSuiteWeb",
      "--host", HOST,
      "--port", "0",
      "--log-dir", this.options.logsDir
    ];

    this.transition("booting");
    this.readyPort = null;
    this.stdoutBuffer = "";

    this.appendLog(`\n[${new Date().toISOString()}] START ${rscript} ${args.join(" ")}\n`);

    this.child = spawn(rscript, args, {
      cwd: repoRoot,
      env: this.buildEnv(runtimeRoot),
      stdio: ["ignore", "pipe", "pipe"]
    });

    this.child.stdout.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      this.appendLog(text);
      this.stdoutBuffer += text;

      let idx = this.stdoutBuffer.indexOf("\n");
      while (idx >= 0) {
        const line = this.stdoutBuffer.slice(0, idx);
        this.processLine(line);
        this.stdoutBuffer = this.stdoutBuffer.slice(idx + 1);
        idx = this.stdoutBuffer.indexOf("\n");
      }
    });

    this.child.stderr.on("data", (chunk) => {
      const text = chunk.toString("utf8");
      this.appendLog(text);
    });

    this.child.on("error", (error) => {
      this.emit("backend-error", `Failed to start backend: ${error.message}`);
    });

    this.child.on("close", (code) => {
      clearTimeout(this.readyTimeout);
      const exitCode = Number.isFinite(code) ? code : -1;
      const wasReady = this.state === "ready";
      const wasStopping = this.state === "stopping";
      this.child = null;
      this.transition("stopped");
      this.emit("backend-exit", exitCode, wasReady, wasStopping);
    });

    this.readyTimeout = setTimeout(() => {
      if (this.state !== "ready") {
        this.emit("backend-error", "Backend startup timed out (60s). Check logs.");
      }
    }, 60000);
  }

  async stop() {
    if (!this.child) {
      this.transition("stopped");
      return;
    }

    this.transition("stopping");
    const child = this.child;

    await new Promise((resolve) => {
      let settled = false;
      const finish = () => {
        if (!settled) {
          settled = true;
          resolve();
        }
      };

      const timeout = setTimeout(() => {
        if (!child.killed) {
          try {
            child.kill("SIGKILL");
          } catch (_) {
            // Ignore kill failures.
          }
        }
      }, 5000);

      child.once("close", () => {
        clearTimeout(timeout);
        finish();
      });

      try {
        child.kill("SIGTERM");
      } catch (_) {
        clearTimeout(timeout);
        finish();
      }
    });
  }

  async restart() {
    await this.stop();
    await this.start();
  }
}

function delay(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
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
        // Any HTTP response means socket is alive.
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

async function waitForBackendReady(port, timeoutMs = 90000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const ok = await probeBackend(port);
    if (ok) return true;
    await delay(500);
  }
  return false;
}

function parseAllowedPort() {
  if (!allowedUrlPrefix) return null;

  try {
    const parsed = new URL(allowedUrlPrefix);
    const rawPort = parsed.port || (parsed.protocol === "https:" ? "443" : "80");
    const port = Number(rawPort);
    if (!Number.isFinite(port) || port <= 0) {
      return null;
    }
    return port;
  } catch (_) {
    return null;
  }
}

function resolveKnownBackendPort() {
  const fromAllowed = parseAllowedPort();
  if (Number.isFinite(fromAllowed) && fromAllowed > 0) {
    return fromAllowed;
  }

  const fromBackend = backend ? Number(backend.readyPort) : NaN;
  if (Number.isFinite(fromBackend) && fromBackend > 0) {
    return fromBackend;
  }

  return null;
}

async function isBackendAlive() {
  const port = resolveKnownBackendPort();
  if (!port) {
    appendMainLog("backend_probe", { alive: false, reason: "missing_port" });
    return false;
  }

  const alive = await probeBackend(port);
  appendMainLog("backend_probe", { alive, port });
  return alive;
}

async function waitForReachableBackendPort(timeoutMs = 90000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const port = resolveKnownBackendPort();
    if (port) {
      const alive = await probeBackend(port);
      if (alive) {
        return port;
      }
    }
    await delay(500);
  }
  return null;
}

async function safeReloadRenderer(reason) {
  if (!mainWindow || mainWindow.isDestroyed()) {
    appendMainLog("renderer_reload_skipped", { reason, skip: "window_missing" });
    return false;
  }

  const currentUrl = mainWindow.webContents.getURL() || "";
  if (allowedUrlPrefix && !currentUrl.startsWith(allowedUrlPrefix)) {
    try {
      await loadShinyPage(allowedUrlPrefix);
      appendMainLog("renderer_reload_success", {
        reason,
        method: "loadShinyPage",
        url: allowedUrlPrefix
      });
      return true;
    } catch (error) {
      appendMainLog("renderer_reload_failed", {
        reason,
        method: "loadShinyPage",
        url: allowedUrlPrefix,
        error: error.message
      });
    }
  }

  try {
    mainWindow.webContents.reloadIgnoringCache();
    appendMainLog("renderer_reload_success", { reason, method: "reloadIgnoringCache" });
    return true;
  } catch (error) {
    appendMainLog("renderer_reload_failed", {
      reason,
      method: "reloadIgnoringCache",
      error: error.message
    });
  }

  return false;
}

async function restartBackendAndReload(reason) {
  if (!backend) {
    throw new Error("Backend controller not ready");
  }

  appendMainLog("backend_restart_start", { reason });
  allowedUrlPrefix = null;

  if (mainWindow && !mainWindow.isDestroyed()) {
    try {
      await mainWindow.loadURL(makeDataUrl(loadingHtml()));
    } catch (_) {
      // Best effort.
    }
  }

  await backend.restart();

  const port = await waitForReachableBackendPort(90000);
  if (!port) {
    appendMainLog("backend_restart_failed", {
      reason,
      error: "Backend not reachable after restart timeout"
    });
    throw new Error("Backend not reachable after restart timeout");
  }

  allowedUrlPrefix = `http://${HOST}:${port}`;
  // Let wireBackendEvents() own renderer navigation on backend-ready.
  // Avoid a second load here, otherwise one wake cycle can create two
  // back-to-back Shiny sessions and wipe the just-restored state.

  appendMainLog("backend_restart_success", { reason, port, url: allowedUrlPrefix });
}

async function recoverAfterResume(reason) {
  if (isQuitting) {
    appendMainLog("recover_skipped", { reason, skip: "quitting" });
    return;
  }

  if (recoveryInProgress) {
    appendMainLog("recover_skipped", { reason, skip: "in_progress" });
    return;
  }

  const now = Date.now();
  const elapsed = now - lastRecoveryAt;
  if (elapsed < RECOVERY_COOLDOWN_MS) {
    appendMainLog("recover_skipped", {
      reason,
      skip: "cooldown",
      elapsed_ms: elapsed
    });
    return;
  }

  recoveryInProgress = true;
  lastRecoveryAt = now;
  appendMainLog("recover_start", { reason });

  try {
    if (!mainWindow || mainWindow.isDestroyed()) {
      appendMainLog("recover_window_recreate", { reason });
      createMainWindow();
    }

    const knownPort = resolveKnownBackendPort();
    if (!allowedUrlPrefix && knownPort) {
      allowedUrlPrefix = `http://${HOST}:${knownPort}`;
    }

    const backendAlive = await isBackendAlive();
    if (!backendAlive) {
      appendMainLog("recover_path", { reason, action: "restart_backend" });
      await restartBackendAndReload(reason);
      appendMainLog("recover_end", { reason, result: "backend_restarted" });
      return;
    }

    appendMainLog("recover_path", { reason, action: "reload_renderer" });
    await safeReloadRenderer(reason);
    appendMainLog("recover_end", { reason, result: "renderer_reloaded" });
  } catch (error) {
    appendMainLog("recover_error", { reason, error: error.message });
  } finally {
    recoveryInProgress = false;
  }
}

function makeDataUrl(html) {
  return `data:text/html;charset=utf-8,${encodeURIComponent(html)}`;
}

function resolveLoadingIconDataUrl() {
  const iconPath = resolveAppIconPath();
  if (!iconPath || !fs.existsSync(iconPath)) return "";

  try {
    const ext = path.extname(iconPath).toLowerCase();
    const mime =
      ext === ".png" ? "image/png" :
      ext === ".jpg" || ext === ".jpeg" ? "image/jpeg" :
      ext === ".webp" ? "image/webp" :
      "";
    if (!mime) return "";

    const base64 = fs.readFileSync(iconPath).toString("base64");
    return `data:${mime};base64,${base64}`;
  } catch (_) {
    return "";
  }
}

function loadingHtml() {
  const iconDataUrl = resolveLoadingIconDataUrl();
  const iconHtml = iconDataUrl
    ? `<img class="app-icon" src="${iconDataUrl}" alt="${APP_NAME} icon" />`
    : "";
  return `<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>${APP_NAME}</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 0; background: #f5f7fa; color: #1f2937; }
    .wrap { max-width: 680px; margin: 12vh auto; padding: 24px; background: #ffffff; border: 1px solid #d1d5db; border-radius: 12px; }
    .title-row { display: flex; align-items: center; gap: 12px; margin-bottom: 12px; }
    .app-icon { width: 42px; height: 42px; border-radius: 10px; object-fit: cover; box-shadow: 0 1px 4px rgba(0,0,0,0.16); }
    h1 { margin: 0 0 12px; font-size: 22px; }
    p { margin: 8px 0; line-height: 1.5; }
    code { background: #f3f4f6; padding: 1px 4px; border-radius: 4px; }
  </style>
</head>
<body>
  <div class="wrap">
    <div class="title-row">
      ${iconHtml}
      <h1>${APP_NAME} is starting</h1>
    </div>
    <p><strong>${APP_SLOGAN}</strong></p>
    <p>The desktop shell is ready and the local Shiny backend is booting.</p>
    <p>First launch can take longer than usual.</p>
  </div>
</body>
</html>`;
}

function errorHtml(message, logPath) {
  return `<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>${APP_NAME} startup failed</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 0; background: #fff7ed; color: #7c2d12; }
    .wrap { max-width: 760px; margin: 10vh auto; padding: 24px; background: #ffffff; border: 1px solid #fdba74; border-radius: 12px; }
    h1 { margin: 0 0 12px; font-size: 22px; }
    p { margin: 10px 0; line-height: 1.5; }
    pre { white-space: pre-wrap; background: #fff1e6; border: 1px solid #fdba74; border-radius: 8px; padding: 12px; }
    code { background: #fff1e6; padding: 1px 4px; border-radius: 4px; }
  </style>
</head>
<body>
  <div class="wrap">
    <h1>Backend startup failed</h1>
    <p>Check whether the bundled runtime and required R packages are available.</p>
    <pre>${message}</pre>
    <p>Log file: <code>${logPath}</code></p>
    <p>Use menu action \"Open Logs Directory\" for quick access.</p>
  </div>
</body>
</html>`;
}

function isAllowedNavigation(url) {
  if (url.startsWith("data:")) return true;
  if (allowedUrlPrefix && url.startsWith(allowedUrlPrefix)) return true;
  return false;
}

function trimScalar(value, defaultValue = "") {
  const out = typeof value === "string" ? value : String(value ?? "");
  const trimmed = out.trim();
  return trimmed || defaultValue;
}

function isExistingDirectory(dirPath) {
  const candidate = trimScalar(dirPath, "");
  if (!candidate || !fs.existsSync(candidate)) return false;
  try {
    return fs.statSync(candidate).isDirectory();
  } catch (_) {
    return false;
  }
}

function resolveExamplesDir() {
  const candidate = app.isPackaged
    ? path.join(process.resourcesPath, "itcsuite", "Examples")
    : path.resolve(__dirname, "../../../Examples");
  if (!isExistingDirectory(candidate)) return "";
  return path.resolve(candidate);
}

function resolveOpenFileDefaultPath() {
  const rememberedDir = trimScalar(openFileLastDir, "");
  if (isExistingDirectory(rememberedDir)) {
    return path.resolve(rememberedDir);
  }
  return resolveExamplesDir();
}

function rememberOpenFileDir(selectedFilePath) {
  const selected = trimScalar(selectedFilePath, "");
  if (!selected) return;
  const dirPath = path.dirname(path.resolve(selected));
  if (!isExistingDirectory(dirPath)) return;
  openFileLastDir = dirPath;
}

function sanitizeOpenFileExtensions(extensions) {
  if (!Array.isArray(extensions)) return [];
  const cleaned = [];
  for (const raw of extensions) {
    const ext = trimScalar(raw, "").toLowerCase().replace(/^\.+/, "");
    if (!ext || !/^[a-z0-9]+$/.test(ext)) continue;
    if (!cleaned.includes(ext)) cleaned.push(ext);
  }
  return cleaned.slice(0, 12);
}

function sanitizeOpenFileFilters(filters, purpose) {
  if (!Array.isArray(filters)) {
    if (purpose === "step1_import") {
      return [{ name: "ITC Data", extensions: ["itc", "txt"] }];
    }
    return [{ name: "Spreadsheet", extensions: ["xlsx"] }];
  }

  const cleaned = [];
  for (const rawFilter of filters.slice(0, 4)) {
    if (!rawFilter || typeof rawFilter !== "object") continue;
    const name = trimScalar(rawFilter.name, "Files");
    const extensions = sanitizeOpenFileExtensions(rawFilter.extensions);
    if (extensions.length < 1) continue;
    cleaned.push({ name, extensions });
  }

  if (cleaned.length > 0) return cleaned;
  return sanitizeOpenFileFilters(null, purpose);
}

function sanitizeOpenFilePayload(rawPayload) {
  const payload = rawPayload && typeof rawPayload === "object" ? rawPayload : {};
  const requestId = trimScalar(payload.request_id, "");
  let purpose = trimScalar(payload.purpose, "");
  if (!OPEN_FILE_PURPOSES.has(purpose)) purpose = "step2_import";
  const fallbackTitle = purpose === "step1_import" ? "Select ITC File" : "Select Data File";
  const titleRaw = trimScalar(payload.title, fallbackTitle);
  const title = titleRaw.slice(0, 160);
  const filters = sanitizeOpenFileFilters(payload.filters, purpose);
  return { request_id: requestId, purpose, title, filters };
}

function makeOpenFileResult(payload, patch = {}) {
  return {
    request_id: payload.request_id,
    purpose: payload.purpose,
    canceled: true,
    file_path: "",
    file_name: "",
    error: "",
    ...patch
  };
}

function diagnosticsTimestamp() {
  const d = new Date();
  const pad = (v) => String(v).padStart(2, "0");
  return `${d.getFullYear()}${pad(d.getMonth() + 1)}${pad(d.getDate())}_${pad(d.getHours())}${pad(d.getMinutes())}${pad(d.getSeconds())}`;
}

function defaultDiagnosticsArchivePath() {
  const downloadsPath = app.getPath("downloads");
  return uniqueDownloadPath(downloadsPath, `ViaBind_diagnostics_${diagnosticsTimestamp()}.zip`);
}

function zipDirectoryToFile(sourceDir, targetZipPath) {
  if (process.platform === "win32") {
    const escapedTarget = targetZipPath.replace(/'/g, "''");
    const args = [
      "-NoProfile",
      "-Command",
      `Compress-Archive -Path * -DestinationPath '${escapedTarget}' -Force`
    ];
    const out = spawnSync("powershell", args, { cwd: sourceDir, encoding: "utf8" });
    if (out.status !== 0) {
      const msg = trimScalar(out.stderr, trimScalar(out.stdout, "Compress-Archive failed"));
      throw new Error(msg);
    }
    return;
  }

  const out = spawnSync("zip", ["-rq", targetZipPath, "."], {
    cwd: sourceDir,
    encoding: "utf8"
  });
  if (out.status !== 0) {
    const msg = trimScalar(out.stderr, trimScalar(out.stdout, "zip command failed"));
    throw new Error(msg);
  }
}

function buildDiagnosticsArchive(payload, outputZipPath) {
  const tmpRoot = fs.mkdtempSync(path.join(app.getPath("temp"), "viabind-diag-"));
  const redactionOpts = {
    userHome: os.homedir(),
    userDataDir: app.getPath("userData")
  };

  try {
    const candidates = collectDiagnosticsFiles(logsDir);
    const copiedFiles = [];

    for (const entry of candidates) {
      const srcPath = entry.abs_path;
      const destPath = path.join(tmpRoot, entry.name);
      const ok = copyRedactedLog(srcPath, destPath, redactionOpts);
      if (!ok) continue;
      copiedFiles.push({
        name: entry.name,
        path: destPath,
        bytes: fs.statSync(destPath).size,
        sha256: sha256File(destPath)
      });
    }

    const manifest = buildManifest({
      appVersion: app.getVersion(),
      platform: process.platform,
      locale: app.getLocale ? app.getLocale() : "",
      privacyMode: payload.privacy_mode,
      files: copiedFiles
    });
    const manifestPath = path.join(tmpRoot, "manifest.json");
    fs.writeFileSync(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`, "utf8");

    zipDirectoryToFile(tmpRoot, outputZipPath);

    return {
      file_count: copiedFiles.length + 1,
      manifest: manifestPath
    };
  } finally {
    try {
      fs.rmSync(tmpRoot, { recursive: true, force: true });
    } catch (_) {
      // Best effort cleanup.
    }
  }
}

function resolveDialogParentWindow() {
  if (mainWindow && !mainWindow.isDestroyed()) {
    return mainWindow;
  }
  return null;
}

async function runDiagnosticsExport(rawPayload, trigger = "ipc") {
  const payload = sanitizeDiagnosticsRequest(rawPayload);
  if (!mainWindow || mainWindow.isDestroyed()) {
    return makeDiagnosticsResult(payload, { error: "Main window is unavailable." });
  }
  if (!logsDir || !fs.existsSync(logsDir)) {
    return makeDiagnosticsResult(payload, { error: "Logs directory is unavailable." });
  }
  if (diagnosticsExportInFlight) {
    return makeDiagnosticsResult(payload, { error: "Diagnostics export already in progress." });
  }

  diagnosticsExportInFlight = true;
  try {
    const dialogParent = resolveDialogParentWindow();
    const dialogOptions = {
      title: "Export Diagnostics",
      buttonLabel: "Export",
      defaultPath: defaultDiagnosticsArchivePath(),
      filters: [{ name: "ZIP Archive", extensions: ["zip"] }],
      properties: ["showOverwriteConfirmation", "createDirectory", "showHiddenFiles"]
    };
    const savePath = dialogParent
      ? dialog.showSaveDialogSync(dialogParent, dialogOptions)
      : dialog.showSaveDialogSync(dialogOptions);

    if (!savePath) {
      return makeDiagnosticsResult(payload, { error: "Canceled by user." });
    }

    appendMainLog("diagnostics_export_start", {
      request_id: payload.request_id,
      privacy_mode: payload.privacy_mode,
      output: savePath,
      trigger
    });

    const archiveStats = buildDiagnosticsArchive(payload, savePath);
    appendMainLog("diagnostics_export_success", {
      request_id: payload.request_id,
      privacy_mode: payload.privacy_mode,
      output: savePath,
      file_count: archiveStats.file_count,
      trigger
    });

    return makeDiagnosticsResult(payload, {
      ok: true,
      file_path: path.resolve(savePath)
    });
  } catch (error) {
    const message = error && error.message ? error.message : "Diagnostics export failed.";
    appendMainLog("diagnostics_export_error", {
      request_id: payload.request_id,
      privacy_mode: payload.privacy_mode,
      error: message,
      trigger
    });
    return makeDiagnosticsResult(payload, { error: message });
  } finally {
    diagnosticsExportInFlight = false;
  }
}

async function showDiagnosticsExportSuccessMessage(filePath) {
  const dialogOptions = {
    type: "info",
    title: "Export Diagnostics",
    message: "Diagnostics package exported successfully.",
    detail: `File: ${filePath}\n\nAttach this ZIP file when reporting an issue.`,
    buttons: ["Open Folder", "OK"],
    defaultId: 0,
    cancelId: 1,
    noLink: true
  };
  const dialogParent = resolveDialogParentWindow();
  const result = dialogParent
    ? await dialog.showMessageBox(dialogParent, dialogOptions)
    : await dialog.showMessageBox(dialogOptions);
  if (result.response !== 0) return;

  const err = await shell.openPath(path.dirname(filePath));
  if (err) {
    dialog.showErrorBox("Open folder failed", err);
  }
}

async function handleMenuExportDiagnostics() {
  const result = await runDiagnosticsExport(
    { privacy_mode: "default_redacted", reason: "menu" },
    "menu"
  );
  if (result && result.ok === true && result.file_path) {
    await showDiagnosticsExportSuccessMessage(result.file_path);
    return;
  }

  const msg = trimScalar(result && result.error, "Diagnostics export failed.");
  if (msg === "Canceled by user.") return;
  dialog.showErrorBox("Export Diagnostics Failed", msg);
}

function registerIpcHandlers() {
  ipcMain.handle(OPEN_FILE_CHANNEL, async (_event, rawPayload) => {
    const payload = sanitizeOpenFilePayload(rawPayload);
    if (!mainWindow || mainWindow.isDestroyed()) {
      return makeOpenFileResult(payload, { error: "Main window is unavailable." });
    }

    const smokeMockPath = trimScalar(process.env.ITCSUITE_SMOKE_OPEN_FILE_PATH, "");
    if (smokeMode && smokeMockPath) {
      const resolvedMockPath = path.resolve(smokeMockPath);
      return makeOpenFileResult(payload, {
        canceled: false,
        file_path: resolvedMockPath,
        file_name: path.basename(resolvedMockPath)
      });
    }

    try {
      const dialogOptions = {
        title: payload.title,
        properties: ["openFile"],
        filters: payload.filters
      };
      const defaultPath = resolveOpenFileDefaultPath();
      if (defaultPath) {
        dialogOptions.defaultPath = defaultPath;
      }

      const dialogResult = await dialog.showOpenDialog(mainWindow, dialogOptions);

      if (dialogResult.canceled) {
        return makeOpenFileResult(payload, { canceled: true });
      }

      const selectedPath = Array.isArray(dialogResult.filePaths) ? dialogResult.filePaths[0] : "";
      const resolvedPath = trimScalar(selectedPath, "");
      if (!resolvedPath) {
        return makeOpenFileResult(payload, { error: "No file selected." });
      }
      const absolutePath = path.resolve(resolvedPath);
      rememberOpenFileDir(absolutePath);

      return makeOpenFileResult(payload, {
        canceled: false,
        file_path: absolutePath,
        file_name: path.basename(absolutePath)
      });
    } catch (error) {
      const message = error && error.message ? error.message : "Failed to open file dialog.";
      return makeOpenFileResult(payload, { error: message });
    }
  });

  ipcMain.handle(EXPORT_DIAGNOSTICS_CHANNEL, async (_event, rawPayload) => {
    return runDiagnosticsExport(rawPayload, "ipc");
  });
}

function uniqueDownloadPath(downloadsPath, filename) {
  const ext = path.extname(filename);
  const name = path.basename(filename, ext);

  let candidate = path.join(downloadsPath, filename);
  let index = 1;
  while (fs.existsSync(candidate)) {
    candidate = path.join(downloadsPath, `${name} (${index})${ext}`);
    index += 1;
  }
  return candidate;
}

function emitDownloadSavedEvent(fileName, savePath) {
  if (!mainWindow || mainWindow.isDestroyed()) return;
  const payload = JSON.stringify({
    file_name: fileName,
    save_path: savePath
  });
  const script = `(() => {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue("itcsuite_download_event", ${payload}, { priority: "event" });
    }
  })();`;
  mainWindow.webContents.executeJavaScript(script).catch(() => {});
}

function buildPowerEventPayload(type, source = "desktop-main", ts = Date.now()) {
  const typeSafe = trimScalar(type, "");
  if (!typeSafe) return null;
  const sourceSafe = trimScalar(source, "desktop-main");
  const tsNum = Number.isFinite(Number(ts)) ? Number(ts) : Date.now();
  return { type: typeSafe, ts: tsNum, source: sourceSafe };
}

function emitPowerPayloadToRenderer(payload, emitKind = "direct", options = {}) {
  if (!payload || typeof payload !== "object") return Promise.resolve(false);
  const normalized = buildPowerEventPayload(payload.type, payload.source, payload.ts);
  if (!normalized) return Promise.resolve(false);
  if (!mainWindow || mainWindow.isDestroyed()) {
    appendMainLog("power_event_emit_skipped", {
      type: normalized.type,
      source: normalized.source,
      emit_kind: emitKind,
      reason: "window_unavailable"
    });
    return Promise.resolve(false);
  }

  const payloadJson = JSON.stringify(normalized);
  const waitForShinyMsRaw = Number(options.waitForShinyMs);
  const waitForShinyMs = Number.isFinite(waitForShinyMsRaw) && waitForShinyMsRaw > 0
    ? Math.round(waitForShinyMsRaw)
    : 0;
  const pollIntervalMsRaw = Number(options.pollIntervalMs);
  const pollIntervalMs = Number.isFinite(pollIntervalMsRaw) && pollIntervalMsRaw > 0
    ? Math.max(25, Math.min(1000, Math.round(pollIntervalMsRaw)))
    : 120;

  const script = waitForShinyMs > 0
    ? `(() => {
      const payload = ${payloadJson};
      const deadline = Date.now() + ${waitForShinyMs};
      const pollMs = ${pollIntervalMs};
      return new Promise((resolve) => {
        const attempt = () => {
          try {
            if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
              window.Shiny.setInputValue("itcsuite_power_event", payload, { priority: "event" });
              resolve(true);
              return;
            }
          } catch (_) {}
          if (Date.now() >= deadline) {
            resolve(false);
            return;
          }
          window.setTimeout(attempt, pollMs);
        };
        attempt();
      });
    })();`
    : `(() => {
      if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
        window.Shiny.setInputValue("itcsuite_power_event", ${payloadJson}, { priority: "event" });
        return true;
      }
      return false;
    })();`;

  return mainWindow.webContents.executeJavaScript(script)
    .then((ok) => {
      const delivered = ok === true;
      appendMainLog("power_event_emit", {
        type: normalized.type,
        source: normalized.source,
        emit_kind: emitKind,
        wait_for_shiny_ms: waitForShinyMs,
        delivered
      });
      return delivered;
    })
    .catch((error) => {
      appendMainLog("power_event_emit_failed", {
        type: normalized.type,
        source: normalized.source,
        emit_kind: emitKind,
        wait_for_shiny_ms: waitForShinyMs,
        error: error.message
      });
      return false;
    });
}

function emitPowerEventToRenderer(type, source = "desktop-main") {
  const payload = buildPowerEventPayload(type, source, Date.now());
  if (!payload) return Promise.resolve(false);
  latestPowerEventPayload = payload;
  return emitPowerPayloadToRenderer(payload, "direct");
}

function replayLatestPowerEventToRenderer(reason = "did-finish-load") {
  const payload = latestPowerEventPayload;
  if (!payload || typeof payload !== "object") return Promise.resolve(false);
  if (!["resume", "unlock-screen"].includes(trimScalar(payload.type, ""))) {
    return Promise.resolve(false);
  }
  const ageMs = Date.now() - Number(payload.ts || 0);
  if (!Number.isFinite(ageMs) || ageMs < 0 || ageMs > POWER_EVENT_REPLAY_WINDOW_MS) {
    return Promise.resolve(false);
  }

  const replayPayload = {
    ...payload,
    source: `${trimScalar(payload.source, "desktop-main")}.replay`
  };
  return emitPowerPayloadToRenderer(replayPayload, "replay", {
    waitForShinyMs: 10000,
    pollIntervalMs: 120
  })
    .then((delivered) => {
      appendMainLog("power_event_replay", {
        reason: trimScalar(reason, "did-finish-load"),
        type: replayPayload.type,
        age_ms: ageMs,
        delivered
      });
      return delivered;
    });
}

function configureDownloadBehavior() {
  session.defaultSession.on("will-download", (_event, item) => {
    const downloadsPath = app.getPath("downloads");
    const suggestedPath = uniqueDownloadPath(downloadsPath, item.getFilename());
    const savePath = dialog.showSaveDialogSync(mainWindow, {
      title: "Save Export",
      buttonLabel: "Save",
      defaultPath: suggestedPath,
      showsTagField: true,
      properties: ["showOverwriteConfirmation", "createDirectory", "showHiddenFiles"]
    });

    if (!savePath) {
      item.cancel();
      return;
    }

    const downloadName = item.getFilename();
    item.setSavePath(savePath);
    item.once("done", (_doneEvent, state) => {
      if (state !== "completed") return;
      const finalPath = item.getSavePath() || savePath;
      emitDownloadSavedEvent(downloadName, finalPath);
    });
  });
}

async function applyWindowsScrollbarPolicy() {
  if (process.platform !== "win32") return;
  if (!mainWindow || mainWindow.isDestroyed()) return;

  try {
    await mainWindow.webContents.insertCSS(WINDOWS_SCROLLBAR_HIDE_CSS);
  } catch (error) {
    appendMainLog("windows_scrollbar_policy_failed", {
      error: trimScalar(error && error.message, "insertCSS failed")
    });
  }
}

function createMainWindow() {
  if (unresponsiveRecoveryTimer) {
    clearTimeout(unresponsiveRecoveryTimer);
    unresponsiveRecoveryTimer = null;
  }
  rendererMarkedUnresponsive = false;

  mainWindow = new BrowserWindow({
    width: 1500,
    height: 980,
    minWidth: 1200,
    minHeight: 760,
    title: APP_TITLE,
    show: true,
    autoHideMenuBar: process.platform === "win32",
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
      preload: path.join(__dirname, "preload.js")
    }
  });

  mainWindow.on("closed", () => {
    mainWindow = null;
  });
  if (process.platform === "win32") {
    // Keep menu available via Alt while reclaiming one row of vertical space.
    mainWindow.setMenuBarVisibility(false);
  }

  mainWindow.webContents.on("render-process-gone", (_event, details) => {
    appendMainLog("render_process_gone", {
      reason: details && details.reason ? details.reason : "unknown",
      exit_code: details && Number.isFinite(details.exitCode) ? details.exitCode : null
    });
    recoverAfterResume("render-process-gone");
  });

  mainWindow.webContents.on("unresponsive", () => {
    rendererMarkedUnresponsive = true;
    appendMainLog("renderer_unresponsive", {});
    if (unresponsiveRecoveryTimer) {
      clearTimeout(unresponsiveRecoveryTimer);
    }
    unresponsiveRecoveryTimer = setTimeout(() => {
      if (!rendererMarkedUnresponsive) return;
      recoverAfterResume("renderer-unresponsive");
    }, UNRESPONSIVE_RECOVERY_DELAY_MS);
  });

  mainWindow.webContents.on("responsive", () => {
    rendererMarkedUnresponsive = false;
    if (unresponsiveRecoveryTimer) {
      clearTimeout(unresponsiveRecoveryTimer);
      unresponsiveRecoveryTimer = null;
    }
    appendMainLog("renderer_responsive", {});
  });

  mainWindow.webContents.on(
    "did-fail-load",
    (_event, errorCode, errorDescription, validatedURL, isMainFrame) => {
      if (!isMainFrame) return;
      if (errorCode === -3) return; // Ignore aborted navigations.
      appendMainLog("did_fail_load", {
        error_code: errorCode,
        error_description: errorDescription,
        url: validatedURL || ""
      });
      recoverAfterResume("did-fail-load");
    }
  );

  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (isAllowedNavigation(url)) {
      return { action: "allow" };
    }
    shell.openExternal(url);
    return { action: "deny" };
  });

  mainWindow.webContents.on("will-navigate", (event, url) => {
    if (!isAllowedNavigation(url)) {
      event.preventDefault();
      shell.openExternal(url);
    }
  });

  mainWindow.webContents.on("page-title-updated", (event) => {
    // Keep desktop window title stable instead of inheriting inner Shiny page titles.
    event.preventDefault();
    mainWindow.setTitle(APP_TITLE);
  });

  mainWindow.webContents.on("did-finish-load", async () => {
    await applyWindowsScrollbarPolicy();

    const currentUrl = mainWindow.webContents.getURL();
    if (allowedUrlPrefix && currentUrl.startsWith(allowedUrlPrefix)) {
      replayLatestPowerEventToRenderer("did-finish-load").catch(() => {});
    }

    if (!smokeMode || smokeReported || !allowedUrlPrefix) {
      return;
    }
    if (!currentUrl.startsWith(allowedUrlPrefix)) {
      return;
    }

    try {
      const payload = await mainWindow.webContents.executeJavaScript(`(() => ({
        step1: document.getElementById("main_tab_label_step1")?.textContent?.trim() || null,
        step2: document.getElementById("main_tab_label_step2")?.textContent?.trim() || null,
        step3: document.getElementById("main_tab_label_step3")?.textContent?.trim() || null
      }))()`);

      if (payload.step1 && payload.step2 && payload.step3) {
        smokeReported = true;
        console.log(`${SMOKE_PREFIX}${JSON.stringify(payload)}`);
        setTimeout(() => app.quit(), 200);
      } else {
        console.error(
          `Smoke check failed: tab labels missing. url=${currentUrl} payload=${JSON.stringify(payload)}`
        );
        app.exit(1);
      }
    } catch (error) {
      console.error(`Smoke check failed: ${error.message}`);
      app.exit(1);
    }
  });

  mainWindow.loadURL(makeDataUrl(loadingHtml()));
}

async function loadShinyPage(url) {
  if (!mainWindow) return;

  const attempts = 120;
  for (let i = 1; i <= attempts; i += 1) {
    try {
      await mainWindow.loadURL(url);
      return;
    } catch (error) {
      if (i === attempts) {
        throw error;
      }
      await delay(500);
    }
  }
}

function showStartupError(message) {
  const logPath = path.join(logsDir, "backend.log");
  if (mainWindow) {
    mainWindow.loadURL(makeDataUrl(errorHtml(message, logPath)));
  }

  if (smokeMode) {
    console.error(`Smoke mode startup error: ${message}`);
    console.error(`Smoke backend log: ${logPath}`);
    app.exit(1);
  }
}

function buildAppMenu() {
  const template = [
    {
      label: APP_NAME,
      submenu: [
        {
          label: "About",
          click: () => {
            dialog.showMessageBox({
              type: "info",
              title: `About ${APP_NAME}`,
              message: `${APP_NAME} Desktop`,
              detail: `${APP_SLOGAN}\n\nDeveloper: ${APP_DEVELOPER_NAME}\nEmail: ${APP_DEVELOPER_EMAIL}\nWebsite: ${APP_DEVELOPER_SITE}\nVersion: ${appVersionSignature()}\n\nElectron shell with local Shiny backend.`
            });
          }
        },
        { type: "separator" },
        {
          label: "Restart Backend",
          click: async () => {
            if (!backend) return;
            allowedUrlPrefix = null;
            if (mainWindow) {
              mainWindow.loadURL(makeDataUrl(loadingHtml()));
            }
            try {
              await backend.restart();
            } catch (error) {
              showStartupError(error.message);
            }
          }
        },
        {
          label: "Open Logs Directory",
          click: async () => {
            const err = await shell.openPath(logsDir);
            if (err) {
              dialog.showErrorBox("Open logs failed", err);
            }
          }
        },
        {
          label: "Export Diagnostics Package...",
          click: async () => {
            await handleMenuExportDiagnostics();
          }
        },
        { type: "separator" },
        { role: "quit" }
      ]
    },
    {
      label: "View",
      submenu: [
        { role: "reload" },
        { role: "toggledevtools" },
        { role: "togglefullscreen" },
        { role: "resetzoom" },
        { role: "zoomin" },
        { role: "zoomout" }
      ]
    }
  ];

  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

function wireBackendEvents() {
  backend.on("backend-ready", async (port) => {
    appendMainLog("backend_ready_event", { port });
    const backendReady = await waitForBackendReady(port, 90000);
    if (!backendReady) {
      appendMainLog("backend_ready_timeout", { port });
      showStartupError(`Backend did not become reachable on ${HOST}:${port} within timeout.`);
      return;
    }

    allowedUrlPrefix = `http://${HOST}:${port}`;
    try {
      await loadShinyPage(allowedUrlPrefix);
      appendMainLog("backend_page_load_success", { port, url: allowedUrlPrefix });
    } catch (error) {
      appendMainLog("backend_page_load_failed", {
        port,
        url: allowedUrlPrefix,
        error: error.message
      });
      showStartupError(`Unable to load Shiny URL (${allowedUrlPrefix}): ${error.message}`);
    }
  });

  backend.on("backend-error", (message) => {
    appendMainLog("backend_error_event", { message });
    showStartupError(message);
  });

  backend.on("backend-exit", (code, wasReady, wasStopping) => {
    appendMainLog("backend_exit_event", { code, wasReady, wasStopping });
    if (isQuitting) return;
    if (wasStopping) return;
    if (code === 0 && !wasReady) return;

    showStartupError(`Backend exited unexpectedly with code ${code}.`);
  });
}

app.on("before-quit", (event) => {
  if (isQuitting) return;
  isQuitting = true;
  event.preventDefault();
  if (unresponsiveRecoveryTimer) {
    clearTimeout(unresponsiveRecoveryTimer);
    unresponsiveRecoveryTimer = null;
  }

  Promise.resolve()
    .then(() => (backend ? backend.stop() : null))
    .finally(() => {
      app.quit();
    });
});

app.on("window-all-closed", () => {
  app.quit();
});

app.whenReady().then(async () => {
  if (process.platform === "darwin" && app.dock && typeof app.dock.setIcon === "function") {
    const iconPath = resolveAppIconPath();
    if (iconPath) {
      const iconImage = nativeImage.createFromPath(iconPath);
      if (!iconImage.isEmpty()) {
        app.dock.setIcon(iconImage);
      }
    }
  }

  logsDir = path.join(app.getPath("userData"), "logs");
  fs.mkdirSync(logsDir, { recursive: true });
  mainLogPath = path.join(logsDir, "main.log");
  appendMainLog("app_ready", { platform: process.platform });

  backend = new BackendController({
    logsDir,
    backendLogPath: path.join(logsDir, "backend.log")
  });

  buildAppMenu();
  registerIpcHandlers();
  configureDownloadBehavior();
  createMainWindow();
  wireBackendEvents();
  powerMonitor.on("suspend", () => {
    appendMainLog("power_suspend", {});
    emitPowerEventToRenderer("suspend", "power-monitor");
  });
  powerMonitor.on("resume", () => {
    appendMainLog("power_resume", {});
    emitPowerEventToRenderer("resume", "power-monitor");
    recoverAfterResume("power-resume");
  });
  powerMonitor.on("unlock-screen", () => {
    appendMainLog("unlock_screen", {});
    emitPowerEventToRenderer("unlock-screen", "power-monitor");
    recoverAfterResume("unlock-screen");
  });

  try {
    await backend.start();
  } catch (error) {
    showStartupError(error.message);
  }
}).catch((error) => {
  dialog.showErrorBox("Startup failure", error.message);
  app.exit(1);
});
