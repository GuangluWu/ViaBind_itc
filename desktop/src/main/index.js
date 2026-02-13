const { app, BrowserWindow, Menu, dialog, shell, session } = require("electron");
const { spawn } = require("child_process");
const http = require("http");
const path = require("path");
const fs = require("fs");
const EventEmitter = require("events");

const HOST = "127.0.0.1";
const READY_PREFIX = "ITCSUITE_READY ";
const ERROR_PREFIX = "ITCSUITE_ERROR ";
const SMOKE_PREFIX = "ITCSUITE_ELECTRON_SMOKE ";

const smokeMode = process.argv.includes("--smoke-test") || process.env.ITCSUITE_SMOKE_TEST === "1";

let mainWindow = null;
let backend = null;
let allowedUrlPrefix = null;
let logsDir = null;
let isQuitting = false;
let smokeReported = false;

function useBundledRuntimeInDev() {
  return process.env.ITCSUITE_USE_BUNDLED_R === "1";
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

    const bundledLib = path.join(runtimeRoot, "library");
    if (fs.existsSync(bundledLib) && (app.isPackaged || useBundledRuntimeInDev())) {
      env.R_HOME = runtimeRoot;
      env.R_LIBS = bundledLib;
      env.R_LIBS_USER = bundledLib;
      env.R_LIBS_SITE = bundledLib;
    }

    // Preserve Unicode rendering (Chinese/special symbols) in R runtime.
    if (!env.LANG || !/UTF-?8/i.test(env.LANG)) {
      env.LANG = "en_US.UTF-8";
    }
    if (!env.LC_CTYPE || !/UTF-?8/i.test(env.LC_CTYPE)) {
      env.LC_CTYPE = "en_US.UTF-8";
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

function makeDataUrl(html) {
  return `data:text/html;charset=utf-8,${encodeURIComponent(html)}`;
}

function loadingHtml() {
  return `<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>ITCSuite</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 0; background: #f5f7fa; color: #1f2937; }
    .wrap { max-width: 680px; margin: 12vh auto; padding: 24px; background: #ffffff; border: 1px solid #d1d5db; border-radius: 12px; }
    h1 { margin: 0 0 12px; font-size: 22px; }
    p { margin: 8px 0; line-height: 1.5; }
    code { background: #f3f4f6; padding: 1px 4px; border-radius: 4px; }
  </style>
</head>
<body>
  <div class="wrap">
    <h1>ITCSuite is starting</h1>
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
  <title>ITCSuite startup failed</title>
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
    item.setSavePath(savePath);
  });
}

function createMainWindow() {
  mainWindow = new BrowserWindow({
    width: 1500,
    height: 980,
    minWidth: 1200,
    minHeight: 760,
    show: true,
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true
    }
  });

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

  mainWindow.webContents.on("did-finish-load", async () => {
    if (!smokeMode || smokeReported || !allowedUrlPrefix) {
      return;
    }

    const currentUrl = mainWindow.webContents.getURL();
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
      label: "ITCSuite",
      submenu: [
        {
          label: "About",
          click: () => {
            dialog.showMessageBox({
              type: "info",
              title: "About ITCSuite",
              message: "ITCSuite Desktop",
              detail: "Electron shell with local Shiny backend."
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
        { type: "separator" },
        { role: "quit" }
      ]
    },
    {
      label: "View",
      submenu: [
        { role: "reload" },
        { role: "toggledevtools" },
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
    const backendReady = await waitForBackendReady(port, 90000);
    if (!backendReady) {
      showStartupError(`Backend did not become reachable on ${HOST}:${port} within timeout.`);
      return;
    }

    allowedUrlPrefix = `http://${HOST}:${port}`;
    try {
      await loadShinyPage(allowedUrlPrefix);
    } catch (error) {
      showStartupError(`Unable to load Shiny URL (${allowedUrlPrefix}): ${error.message}`);
    }
  });

  backend.on("backend-error", (message) => {
    showStartupError(message);
  });

  backend.on("backend-exit", (code, wasReady, wasStopping) => {
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
  logsDir = path.join(app.getPath("userData"), "logs");
  fs.mkdirSync(logsDir, { recursive: true });

  backend = new BackendController({
    logsDir,
    backendLogPath: path.join(logsDir, "backend.log")
  });

  buildAppMenu();
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
