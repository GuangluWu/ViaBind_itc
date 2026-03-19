/**
 * Shared utility functions for the desktop main process.
 * Extracted to enable both usage in index.js and unit testing.
 */

function trimScalar(value, defaultValue = "") {
  const out = typeof value === "string" ? value : String(value ?? "");
  const trimmed = out.trim();
  return trimmed || defaultValue;
}

function escapeHtml(unsafe) {
  const text = typeof unsafe === "string" ? unsafe : String(unsafe ?? "");
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

module.exports = { trimScalar, escapeHtml };
