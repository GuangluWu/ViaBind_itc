const { contextBridge, ipcRenderer } = require("electron");

const OPEN_FILE_CHANNEL = "itcsuite:open-file";

function trimScalar(value, defaultValue = "") {
  const out = typeof value === "string" ? value : String(value ?? "");
  const trimmed = out.trim();
  return trimmed || defaultValue;
}

function sanitizeExtensions(extensions) {
  if (!Array.isArray(extensions)) return [];
  const cleaned = [];
  for (const raw of extensions) {
    const ext = trimScalar(raw, "").toLowerCase().replace(/^\.+/, "");
    if (!ext || !/^[a-z0-9]+$/.test(ext)) continue;
    if (!cleaned.includes(ext)) cleaned.push(ext);
  }
  return cleaned.slice(0, 12);
}

function sanitizeFilters(filters) {
  if (!Array.isArray(filters)) return [];
  const cleaned = [];
  for (const rawFilter of filters.slice(0, 4)) {
    if (!rawFilter || typeof rawFilter !== "object") continue;
    const name = trimScalar(rawFilter.name, "Files");
    const extensions = sanitizeExtensions(rawFilter.extensions);
    if (extensions.length < 1) continue;
    cleaned.push({ name, extensions });
  }
  return cleaned;
}

function sanitizePayload(payload) {
  const raw = payload && typeof payload === "object" ? payload : {};
  return {
    request_id: trimScalar(raw.request_id, ""),
    purpose: trimScalar(raw.purpose, ""),
    title: trimScalar(raw.title, "").slice(0, 160),
    filters: sanitizeFilters(raw.filters)
  };
}

contextBridge.exposeInMainWorld("itcsuiteDesktop", {
  openFile: async (payload = {}) => {
    const normalized = sanitizePayload(payload);
    const response = await ipcRenderer.invoke(OPEN_FILE_CHANNEL, normalized);
    if (!response || typeof response !== "object") {
      return {
        request_id: normalized.request_id,
        purpose: normalized.purpose,
        canceled: true,
        file_path: "",
        file_name: "",
        error: "Invalid response from desktop open file handler."
      };
    }
    return response;
  }
});
