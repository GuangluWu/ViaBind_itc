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
const { trimScalar, escapeHtml } = require("./utils");

const HOST = "127.0.0.1";
const SMOKE_PREFIX = "ITCSUITE_ELECTRON_SMOKE ";
const APP_NAME = "ViaBind";
const APP_SLOGAN = "Your Path, Your Model";
const APP_TITLE = `${APP_NAME}: ${APP_SLOGAN}`;
const APP_DEVELOPER_NAME = "Guanglu Wu (吴光鹭)";
const APP_DEVELOPER_EMAIL = "guanglu.wu@gmail.com";
const APP_DEVELOPER_SITE = "guanglu.xyz";
const appVersionSignature = () => `ViaBind v${app.getVersion()}`;
app.setName(APP_NAME);

const smokeMode = process.argv.includes("--smoke-test") || process.env.ITCSUITE_SMOKE_TEST === "1";

if (process.platform === "win32" && smokeMode) {
  // GitHub-hosted Windows runners can crash packaged Electron apps during
  // early GPU initialization. Keep smoke mode CPU-rendered only.
  app.disableHardwareAcceleration();
  app.commandLine.appendSwitch("disable-gpu");
  app.commandLine.appendSwitch("disable-gpu-compositing");
}

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

const RECOVERY_COOLDOWN_MS = 10000;
const UNRESPONSIVE_RECOVERY_DELAY_MS = 3000;
const WINDOWS_SCROLLBAR_HIDE_CSS = `
  * {
    scrollbar-width: none !important;
  }
  *::-webkit-scrollbar {
    width: 0 !important;
    height: 0 !important;
  }
`;

const BackendController = require("./backend-controller");
const { setupIpcHandlers, handleMenuExportDiagnostics } = require("./ipc-handlers");
const { setupMenu } = require("./menu");
const { setupPowerEvents, replayLatestPowerEventToRenderer } = require("./power-events");

function appendMainLog(eventName, details = {}) {
  if (!mainLogPath) return;

  const payload = {
    ts: new Date().toISOString(),
    event: eventName,
    ...details
  };

  fs.promises.appendFile(mainLogPath, `${JSON.stringify(payload)}\n`).catch(() => {
    // Best effort logging.
  });
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

async function isRendererSessionAlive() {
  if (!mainWindow || mainWindow.isDestroyed()) return false;

  const script = `(() => {
    try {
      if (window.Shiny && window.Shiny.shinyapp &&
          window.Shiny.shinyapp.$socket &&
          window.Shiny.shinyapp.$socket.readyState === WebSocket.OPEN) {
        return true;
      }
    } catch (_) {}
    return false;
  })();`;

  try {
    const alive = await Promise.race([
      mainWindow.webContents.executeJavaScript(script),
      delay(3000).then(() => false)
    ]);
    const result = alive === true;
    appendMainLog("renderer_session_probe", { alive: result });
    return result;
  } catch (error) {
    appendMainLog("renderer_session_probe", {
      alive: false,
      error: error.message
    });
    return false;
  }
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

    const sessionAlive = await isRendererSessionAlive();
    if (sessionAlive) {
      appendMainLog("recover_end", { reason, result: "session_still_alive" });
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

let cachedIconDataUrl = null;

function resolveLoadingIconDataUrl() {
  if (cachedIconDataUrl !== null) return cachedIconDataUrl;

  const iconPath = resolveAppIconPath();
  if (!iconPath || !fs.existsSync(iconPath)) {
    cachedIconDataUrl = "";
    return cachedIconDataUrl;
  }

  try {
    const ext = path.extname(iconPath).toLowerCase();
    const mime =
      ext === ".png" ? "image/png" :
      ext === ".jpg" || ext === ".jpeg" ? "image/jpeg" :
      ext === ".webp" ? "image/webp" :
      "";
    if (!mime) {
      cachedIconDataUrl = "";
      return cachedIconDataUrl;
    }

    const base64 = fs.readFileSync(iconPath).toString("base64");
    cachedIconDataUrl = `data:${mime};base64,${base64}`;
    return cachedIconDataUrl;
  } catch (_) {
    cachedIconDataUrl = "";
    return cachedIconDataUrl;
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
    <pre>${escapeHtml(message)}</pre>
    <p>Log file: <code>${escapeHtml(logPath)}</code></p>
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
    show: !smokeMode,
    paintWhenInitiallyHidden: true,
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
      if (i > 1) {
        appendMainLog("load_shiny_page_success", { url, attempt: i });
      }
      return;
    } catch (error) {
      // Log the first failure and then every 10 attempts for diagnostics.
      if (i === 1 || i % 10 === 0) {
        appendMainLog("load_shiny_page_retry", {
          url,
          attempt: i,
          max_attempts: attempts,
          error: error.message
        });
      }
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
    host: HOST,
    logsDir,
    backendLogPath: path.join(logsDir, "backend.log")
  });

  setupMenu({
    appName: APP_NAME,
    appSlogan: APP_SLOGAN,
    developerName: APP_DEVELOPER_NAME,
    developerEmail: APP_DEVELOPER_EMAIL,
    developerSite: APP_DEVELOPER_SITE,
    versionSignature: appVersionSignature(),
    restartBackend: async () => {
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
    },
    openLogsDir: async () => {
      const err = await shell.openPath(logsDir);
      if (err) {
        dialog.showErrorBox("Open logs failed", err);
      }
    },
    exportDiagnostics: async () => {
      await handleMenuExportDiagnostics();
    }
  });
  setupIpcHandlers({
    getMainWindow: () => mainWindow,
    getLogsDir: () => logsDir,
    appendMainLog
  });
  setupPowerEvents({
    smokeMode,
    appendMainLog,
    recoverAfterResume,
    getMainWindow: () => mainWindow
  });
  configureDownloadBehavior();
  createMainWindow();
  wireBackendEvents();


  try {
    await backend.start();
  } catch (error) {
    showStartupError(error.message);
  }
}).catch((error) => {
  dialog.showErrorBox("Startup failure", error.message);
  app.exit(1);
});
