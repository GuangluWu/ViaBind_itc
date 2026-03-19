const { app, dialog, shell, ipcMain } = require("electron");
const path = require("path");
const fs = require("fs");
const os = require("os");
const { spawnSync } = require("child_process");
const { trimScalar } = require("./utils");
const { copyRedactedLog, buildManifest, collectDiagnosticsFiles, sha256File } = require("./diagnostics");

const OPEN_FILE_CHANNEL = "itcsuite:open-file";
const EXPORT_DIAGNOSTICS_CHANNEL = "itcsuite:export-diagnostics";
const OPEN_FILE_PURPOSES = new Set(["step1_import", "step2_import", "step3_import"]);

const smokeMode = process.argv.includes("--smoke-test") || process.env.ITCSUITE_SMOKE_TEST === "1";

let openFileLastDir = "";
let diagnosticsExportInFlight = false;

let ctx = {
  getMainWindow: () => null,
  getLogsDir: () => null,
  appendMainLog: () => {}
};

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
      return [{ name: "ITC/TA Data", extensions: ["itc", "txt", "nitc", "csc", "xml"] }];
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
  const fallbackTitle = purpose === "step1_import" ? "Select ITC/TA Data File" : "Select Data File";
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

function sanitizeDiagnosticsRequest(rawPayload) {
  const payload = rawPayload && typeof rawPayload === "object" ? rawPayload : {};
  const requestId = trimScalar(payload.request_id, "");
  const allowedPrivacyModes = new Set(["default_redacted", "raw_unredacted"]);
  let privacyMode = trimScalar(payload.privacy_mode, "default_redacted");
  if (!allowedPrivacyModes.has(privacyMode)) {
    privacyMode = "default_redacted";
  }
  return { request_id: requestId, privacy_mode: privacyMode };
}

function makeDiagnosticsResult(payload, patch = {}) {
  return {
    request_id: payload.request_id,
    privacy_mode: payload.privacy_mode,
    ok: false,
    file_path: "",
    error: "",
    ...patch
  };
}

function diagnosticsTimestamp() {
  const d = new Date();
  const pad = (v) => String(v).padStart(2, "0");
  return `${d.getFullYear()}${pad(d.getMonth() + 1)}${pad(d.getDate())}_${pad(d.getHours())}${pad(d.getMinutes())}${pad(d.getSeconds())}`;
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
    const candidates = collectDiagnosticsFiles(ctx.getLogsDir());
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
  const win = ctx.getMainWindow();
  if (win && !win.isDestroyed()) {
    return win;
  }
  return null;
}

async function runDiagnosticsExport(rawPayload, trigger = "ipc") {
  const payload = sanitizeDiagnosticsRequest(rawPayload);
  const win = ctx.getMainWindow();
  if (!win || win.isDestroyed()) {
    return makeDiagnosticsResult(payload, { error: "Main window is unavailable." });
  }
  const logsDir = ctx.getLogsDir();
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

    ctx.appendMainLog("diagnostics_export_start", {
      request_id: payload.request_id,
      privacy_mode: payload.privacy_mode,
      output: savePath,
      trigger
    });

    const archiveStats = buildDiagnosticsArchive(payload, savePath);
    ctx.appendMainLog("diagnostics_export_success", {
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
    ctx.appendMainLog("diagnostics_export_error", {
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

function setupIpcHandlers(context) {
  ctx = Object.assign(ctx, context);

  ipcMain.handle(OPEN_FILE_CHANNEL, async (_event, rawPayload) => {
    const payload = sanitizeOpenFilePayload(rawPayload);
    const mainWindow = ctx.getMainWindow();
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

module.exports = {
  setupIpcHandlers,
  handleMenuExportDiagnostics
};
