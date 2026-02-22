import assert from "node:assert/strict";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const diagnostics = require("../src/main/diagnostics.js");

function run() {
  const req = diagnostics.sanitizeDiagnosticsRequest({
    request_id: "  req_1 ",
    privacy_mode: "default_redacted",
    reason: "  user_report "
  });
  assert.equal(req.request_id, "req_1");
  assert.equal(req.privacy_mode, "default_redacted");
  assert.equal(req.reason, "user_report");

  const reqFallback = diagnostics.sanitizeDiagnosticsRequest({
    request_id: "x",
    privacy_mode: "unknown_mode"
  });
  assert.equal(reqFallback.privacy_mode, "default_redacted");

  const result = diagnostics.makeDiagnosticsResult(req, { ok: true, file_path: "/tmp/a.zip" });
  assert.equal(result.request_id, "req_1");
  assert.equal(result.ok, true);
  assert.equal(result.file_path, "/tmp/a.zip");

  const text = diagnostics.redactDiagnosticsContent(
    "Path: /Users/alice/Library/Application Support/itcsuite/logs/main.log\nintegration: [1,2,3]"
  );
  assert.ok(text.includes("[REDACTED_DATA_LINE]"));

  const manifest = diagnostics.buildManifest({
    appVersion: "0.3.4",
    platform: "darwin",
    locale: "en-US",
    privacyMode: "default_redacted",
    files: [{ name: "main.log", bytes: 123, sha256: "abc" }]
  });
  assert.equal(manifest.app_version, "0.3.4");
  assert.equal(manifest.platform, "darwin");
  assert.equal(manifest.privacy_mode, "default_redacted");
  assert.equal(Array.isArray(manifest.files), true);
  assert.equal(Array.isArray(manifest.checksum), true);
}

run();
console.log("diagnostics.test.mjs: OK");
