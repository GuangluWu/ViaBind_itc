import fs from "node:fs";
import path from "node:path";

export const DEFAULT_MACOS_MIN_VERSION = "13.0";
export const HOST_R_FRAMEWORK_RESOURCES_RE = /^\/Library\/Frameworks\/R\.framework\/Versions\/[^/]+\/Resources(?:\/|$)/;
export const PORTABLE_RSCRIPT_LAUNCHER_NAME = "itcsuite-rscript";

const APPLE_SYSTEM_LIBRARY_PREFIXES = [
  "/usr/lib/",
  "/System/Library/",
  "/Library/Apple/System/Library/"
];

const MACH_O_MAGIC_NUMBERS = new Set([
  0xfeedface,
  0xcefaedfe,
  0xfeedfacf,
  0xcffaedfe,
  0xcafebabe,
  0xbebafeca
]);

export function normalizeMacOSVersion(value, fallback = DEFAULT_MACOS_MIN_VERSION) {
  const raw = String(value || fallback).trim();
  if (!/^\d+\.\d+(?:\.\d+)?$/.test(raw)) {
    throw new Error(`invalid macOS version: ${raw || "<empty>"}`);
  }
  return raw;
}

export function parseMacOSVersion(value) {
  return normalizeMacOSVersion(value)
    .split(".")
    .map((part) => Number.parseInt(part, 10));
}

export function compareMacOSVersions(a, b) {
  const left = parseMacOSVersion(a);
  const right = parseMacOSVersion(b);
  const size = Math.max(left.length, right.length);
  for (let i = 0; i < size; i += 1) {
    const lhs = left[i] || 0;
    const rhs = right[i] || 0;
    if (lhs !== rhs) return lhs < rhs ? -1 : 1;
  }
  return 0;
}

export function parseOtoolLinkedLibraries(output) {
  const refs = [];
  const lines = String(output || "").split(/\r?\n/).slice(1);
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    const match = trimmed.match(/^(.+?)\s+\(/);
    if (match && match[1]) {
      refs.push(match[1]);
    }
  }
  return refs;
}

export function parseOtoolInstallName(output) {
  const lines = String(output || "")
    .split(/\r?\n/)
    .slice(1)
    .map((line) => line.trim())
    .filter(Boolean);
  if (lines.length < 1) return "";
  return lines[0];
}

export function parseOtoolMinimumMacOSVersions(output) {
  const versions = [];
  let awaiting = "";

  for (const line of String(output || "").split(/\r?\n/)) {
    const trimmed = line.trim();
    if (!trimmed) continue;

    if (trimmed.startsWith("cmd ")) {
      awaiting = "";
      if (trimmed === "cmd LC_BUILD_VERSION") awaiting = "build";
      if (trimmed === "cmd LC_VERSION_MIN_MACOSX") awaiting = "legacy";
      continue;
    }

    if (awaiting === "build" && trimmed.startsWith("minos ")) {
      versions.push(trimmed.replace(/^minos\s+/, "").trim());
      awaiting = "";
      continue;
    }

    if (awaiting === "legacy" && trimmed.startsWith("version ")) {
      versions.push(trimmed.replace(/^version\s+/, "").trim());
      awaiting = "";
    }
  }

  return versions.filter(Boolean);
}

export function isAllowedAbsoluteSystemReference(reference) {
  if (!reference.startsWith("/")) return false;
  return APPLE_SYSTEM_LIBRARY_PREFIXES.some((prefix) => reference.startsWith(prefix));
}

export function findDisallowedAbsoluteReferences(references) {
  return references.filter((reference) => reference.startsWith("/") && !isAllowedAbsoluteSystemReference(reference));
}

export function findUnsupportedMinimumMacOSVersions(versions, maximumVersion = DEFAULT_MACOS_MIN_VERSION) {
  return versions.filter((version) => compareMacOSVersions(version, maximumVersion) > 0);
}

function readMagicNumber(filePath) {
  const fd = fs.openSync(filePath, "r");
  const buffer = Buffer.alloc(8);
  try {
    const bytesRead = fs.readSync(fd, buffer, 0, 8, 0);
    if (bytesRead < 4) return null;
    const be = buffer.readUInt32BE(0);
    const le = buffer.readUInt32LE(0);
    if (be === 0xcafebabe && bytesRead >= 8) {
      const nfatArch = buffer.readUInt32BE(4);
      return nfatArch >= 1 && nfatArch <= 16 ? be : null;
    }
    if (le === 0xcafebabe && bytesRead >= 8) {
      const nfatArch = buffer.readUInt32LE(4);
      return nfatArch >= 1 && nfatArch <= 16 ? le : null;
    }
    if (MACH_O_MAGIC_NUMBERS.has(be)) return be;
    if (MACH_O_MAGIC_NUMBERS.has(le)) return le;
    return null;
  } finally {
    fs.closeSync(fd);
  }
}

export function isMachOBinary(filePath) {
  try {
    return readMagicNumber(filePath) !== null;
  } catch (_) {
    return false;
  }
}

export function walkRuntime(root, onEntry) {
  if (!fs.existsSync(root)) return;
  const stack = [root];
  while (stack.length > 0) {
    const current = stack.pop();
    let entries = [];
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch (_) {
      continue;
    }
    for (const entry of entries) {
      const fullPath = path.join(current, entry.name);
      onEntry(fullPath, entry);
      if (entry.isDirectory() && !entry.isSymbolicLink()) {
        stack.push(fullPath);
      }
    }
  }
}

export function collectMachOBinaries(root) {
  const files = [];
  walkRuntime(root, (fullPath, entry) => {
    if (!entry.isFile()) return;
    if (isMachOBinary(fullPath)) files.push(fullPath);
  });
  return files;
}

export function collectSymlinks(root) {
  const links = [];
  walkRuntime(root, (fullPath, entry) => {
    if (entry.isSymbolicLink()) links.push(fullPath);
  });
  return links;
}

export function isHostRFrameworkReference(reference) {
  return HOST_R_FRAMEWORK_RESOURCES_RE.test(reference);
}
