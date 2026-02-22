const crypto = require("crypto");
const fs = require("fs");
const os = require("os");
const path = require("path");

const DIAGNOSTIC_LOG_FILES = [
  "main.log",
  "backend.log",
  "launch_shiny.log",
  "app-events.log",
  "error.log",
  "session.log"
];

function trimScalar(value, defaultValue = "") {
  const out = typeof value === "string" ? value : String(value ?? "");
  const trimmed = out.trim();
  return trimmed || defaultValue;
}

function sanitizeDiagnosticsRequest(rawPayload) {
  const payload = rawPayload && typeof rawPayload === "object" ? rawPayload : {};
  const modeRaw = trimScalar(payload.privacy_mode, "default_redacted");
  const mode = ["default_redacted", "balanced", "deep_debug"].includes(modeRaw)
    ? modeRaw
    : "default_redacted";
  return {
    request_id: trimScalar(payload.request_id, ""),
    privacy_mode: mode,
    reason: trimScalar(payload.reason, "")
  };
}

function makeDiagnosticsResult(request, patch = {}) {
  return {
    request_id: request.request_id,
    ok: false,
    file_path: "",
    error: "",
    ...patch
  };
}

function redactAbsolutePaths(text, opts = {}) {
  let out = typeof text === "string" ? text : String(text ?? "");
  const userHome = trimScalar(opts.userHome, os.homedir());
  const userDataDir = trimScalar(opts.userDataDir, "");

  if (userHome) {
    out = out.split(userHome).join("~");
  }
  if (userDataDir) {
    out = out.split(userDataDir).join("<itcsuite_user_data>");
  }
  return out;
}

function redactPotentialRawDataLines(text) {
  const lines = String(text ?? "").split(/\r?\n/);
  const forbidden = /(power_original|power_corrected|integration(_rev)?|simulation|Heat_Raw|dQ_App)\s*[:=]/i;
  const out = lines.map((line) => (forbidden.test(line) ? "[REDACTED_DATA_LINE]" : line));
  return out.join("\n");
}

function redactDiagnosticsContent(text, opts = {}) {
  let out = redactAbsolutePaths(text, opts);
  out = redactPotentialRawDataLines(out);
  return out;
}

function ensureDirectory(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function copyRedactedLog(srcPath, destPath, opts = {}) {
  if (!fs.existsSync(srcPath)) return false;
  const content = fs.readFileSync(srcPath, "utf8");
  const redacted = redactDiagnosticsContent(content, opts);
  ensureDirectory(path.dirname(destPath));
  fs.writeFileSync(destPath, redacted, "utf8");
  return true;
}

function collectDiagnosticsFiles(logsDir) {
  const out = [];
  for (const name of DIAGNOSTIC_LOG_FILES) {
    const absPath = path.join(logsDir, name);
    if (fs.existsSync(absPath) && fs.statSync(absPath).isFile()) {
      out.push({
        name,
        abs_path: absPath
      });
    }
  }
  return out;
}

function sha256File(filePath) {
  const hash = crypto.createHash("sha256");
  const buf = fs.readFileSync(filePath);
  hash.update(buf);
  return hash.digest("hex");
}

function buildManifest({
  appVersion,
  platform,
  locale,
  privacyMode,
  files
}) {
  const generatedAt = new Date().toISOString();
  const fileItems = Array.isArray(files) ? files : [];
  const checksums = fileItems.map((f) => ({
    name: trimScalar(f.name, "unknown"),
    sha256: trimScalar(f.sha256, "")
  }));
  return {
    app_version: trimScalar(appVersion, "0.0.0-dev"),
    platform: trimScalar(platform, process.platform),
    locale: trimScalar(locale, ""),
    generated_at_utc: generatedAt,
    privacy_mode: trimScalar(privacyMode, "default_redacted"),
    files: fileItems.map((f) => ({
      name: trimScalar(f.name, "unknown"),
      bytes: Number.isFinite(f.bytes) ? f.bytes : 0
    })),
    redaction_rules: [
      "replace user home path with ~",
      "replace ITCSUITE_USER_DATA_DIR with <itcsuite_user_data>",
      "remove lines that may include raw experiment data arrays"
    ],
    checksum: checksums
  };
}

module.exports = {
  DIAGNOSTIC_LOG_FILES,
  trimScalar,
  sanitizeDiagnosticsRequest,
  makeDiagnosticsResult,
  redactAbsolutePaths,
  redactDiagnosticsContent,
  copyRedactedLog,
  collectDiagnosticsFiles,
  sha256File,
  buildManifest
};
