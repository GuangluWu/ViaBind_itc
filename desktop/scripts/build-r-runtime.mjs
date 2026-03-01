#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(__filename);
const desktopDir = path.resolve(scriptDir, "..");

const DEFAULT_OUT_DIR = path.join(desktopDir, "resources", "r-runtime");
const DEFAULT_MANIFEST_PATH = path.join(desktopDir, "resources", "r-runtime-manifest.txt");

const FALLBACK_PKGS = [
  "shiny",
  "plotly",
  "DT",
  "writexl",
  "ggplot2",
  "patchwork",
  "readxl",
  "jsonlite",
  "rootSolve",
  "tidyverse",
  "DEoptim",
  "MASS",
  "gridExtra",
  "later",
  "digest",
  "colourpicker"
];

const INSTALL_SCRIPT = `
lib_dir <- Sys.getenv("ITCSUITE_LIB_DIR")
pkg_csv <- Sys.getenv("ITCSUITE_PKG_CSV")
if (!nzchar(lib_dir) || !nzchar(pkg_csv)) stop("missing env")
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))
pkgs <- strsplit(pkg_csv, ",", fixed = TRUE)[[1]]
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org", lib = lib_dir)
}
remaining <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(remaining) > 0) {
  stop(sprintf("Failed to install packages: %s", paste(remaining, collapse = ", ")))
}
cat(sprintf("Installed/verified %d packages in %s\\n", length(pkgs), lib_dir))
`;

const VERIFY_AND_PRUNE_SCRIPT = `
trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)
lib_dir <- Sys.getenv("ITCSUITE_LIB_DIR")
manifest_path <- Sys.getenv("ITCSUITE_MANIFEST_PATH")
strict_mode <- identical(Sys.getenv("ITCSUITE_STRICT_RUNTIME_MANIFEST"), "1")
if (!dir.exists(lib_dir)) stop("library dir does not exist")

manifest_pkgs <- character()
if (nzchar(manifest_path) && file.exists(manifest_path)) {
  lines <- readLines(manifest_path, warn = FALSE)
  lines <- trim(sub("#.*$", "", lines))
  manifest_pkgs <- unique(lines[nzchar(lines)])
}

if (length(manifest_pkgs) == 0L) {
  if (strict_mode) stop("manifest package list is empty")
  q(status = 0L)
}

ip <- installed.packages(lib.loc = lib_dir)
installed_names <- rownames(ip)
missing <- setdiff(manifest_pkgs, installed_names)
if (length(missing) > 0L) {
  msg <- sprintf("manifest package missing in runtime library: %s", paste(missing, collapse = ", "))
  if (strict_mode) stop(msg)
  warning(msg, call. = FALSE)
}

roots <- intersect(manifest_pkgs, installed_names)
deps <- tools::package_dependencies(
  roots,
  db = ip,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
deps <- unique(unlist(deps, use.names = FALSE))
base_keep <- rownames(ip)[ip[, "Priority"] %in% c("base", "recommended")]
keep <- unique(c(roots, deps, base_keep))
drop <- setdiff(installed_names, keep)
if (length(drop) > 0L) {
  unlink(file.path(lib_dir, drop), recursive = TRUE, force = TRUE)
  cat(sprintf("Pruned %d non-manifest packages from runtime library.\\n", length(drop)))
} else {
  cat("No non-manifest packages to prune.\\n")
}
`;

const PRUNE_DIR_NAMES = new Set([
  "help",
  "html",
  "doc",
  "docs",
  "demo",
  "demos",
  "examples",
  "tests",
  "testthat",
  "vignettes",
  "man"
]);

const DEFAULT_RUNTIME_WARN_MAX_MB = 260;
const DEFAULT_RUNTIME_WARN_MAX_FILES = 8000;

function usage() {
  console.log(`Usage:
  ${path.basename(__filename)} [out_dir] [options]

Options:
  [out_dir]                    Optional runtime output dir (default: ${DEFAULT_OUT_DIR})
  --out-dir <dir>              Runtime output dir
  --profile <release|debug>    Build profile (default: release)
  --manifest <path>            Runtime package manifest path
  --strict-runtime-manifest    Fail if manifest package is missing
  --symbols-out <dir>          Archive dSYM files to this dir before pruning
  -h, --help                   Show this help
`);
}

function resolveInputPath(value) {
  if (!value) return value;
  if (path.isAbsolute(value)) return value;
  return path.resolve(process.cwd(), value);
}

function parseArgs(argv) {
  let outDir = "";
  let profile = "release";
  let manifestPath = DEFAULT_MANIFEST_PATH;
  let strictRuntimeManifest = false;
  let symbolsOut = "";

  const args = [...argv];
  if (args.length > 0 && !args[0].startsWith("--")) {
    outDir = args.shift();
  }

  while (args.length > 0) {
    const arg = args.shift();
    switch (arg) {
      case "--out-dir":
        outDir = args.shift() || "";
        break;
      case "--profile":
        profile = args.shift() || "";
        break;
      case "--manifest":
        manifestPath = args.shift() || "";
        break;
      case "--strict-runtime-manifest":
        strictRuntimeManifest = true;
        break;
      case "--symbols-out":
        symbolsOut = args.shift() || "";
        break;
      case "-h":
      case "--help":
        usage();
        process.exit(0);
        break;
      default:
        throw new Error(`unknown argument: ${arg}`);
    }
  }

  if (!outDir) outDir = DEFAULT_OUT_DIR;
  if (profile !== "release" && profile !== "debug") {
    throw new Error("--profile must be release or debug");
  }
  if (!manifestPath) {
    throw new Error("--manifest cannot be empty");
  }

  return {
    outDir: resolveInputPath(outDir),
    profile,
    manifestPath: resolveInputPath(manifestPath),
    strictRuntimeManifest,
    symbolsOut: symbolsOut ? resolveInputPath(symbolsOut) : "",
    rBin: process.env.R_BIN || "R"
  };
}

function readPositiveIntEnv(name, fallback) {
  const raw = process.env[name];
  if (!raw) return fallback;
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed < 1) return fallback;
  return parsed;
}

function runCommand(command, args, options = {}) {
  const {
    cwd = process.cwd(),
    env = process.env,
    capture = false,
    stdio = "inherit"
  } = options;

  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      env,
      stdio: capture ? ["ignore", "pipe", "pipe"] : stdio
    });

    let stdout = "";
    let stderr = "";

    if (capture) {
      child.stdout.on("data", (chunk) => {
        stdout += chunk.toString("utf8");
      });
      child.stderr.on("data", (chunk) => {
        stderr += chunk.toString("utf8");
      });
    }

    child.on("error", (error) => {
      if (error && error.code === "ENOENT") {
        reject(new Error(`${command} not found in PATH`));
        return;
      }
      reject(error);
    });

    child.on("close", (code) => {
      if (code === 0) {
        resolve({ stdout, stderr });
        return;
      }
      const suffix = capture && stderr ? `: ${stderr.trim()}` : "";
      const normalizedArgs = args.map((arg) => {
        const singleLine = String(arg).replace(/\s+/g, " ").trim();
        if (singleLine.length > 140) {
          return `${singleLine.slice(0, 137)}...`;
        }
        return singleLine;
      });
      reject(new Error(`${command} ${normalizedArgs.join(" ")} exited with code ${code}${suffix}`));
    });
  });
}

function readManifestPackages(manifestPath) {
  if (!fs.existsSync(manifestPath)) return null;
  const lines = fs.readFileSync(manifestPath, "utf8").split(/\r?\n/);
  const packages = [];

  for (const line of lines) {
    const trimmed = line.replace(/\s*#.*$/, "").trim();
    if (!trimmed) continue;
    if (!packages.includes(trimmed)) {
      packages.push(trimmed);
    }
  }

  return packages;
}

function resolveBundledRscript(runtimeRoot) {
  const candidates = [
    path.join(runtimeRoot, "bin", "Rscript.exe"),
    path.join(runtimeRoot, "bin", "x64", "Rscript.exe"),
    path.join(runtimeRoot, "bin", "Rscript"),
    path.join(runtimeRoot, "Resources", "bin", "Rscript")
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  return "";
}

function resolveHostRscript(rHomeDir) {
  const candidates = [
    path.join(rHomeDir, "bin", "x64", "Rscript.exe"),
    path.join(rHomeDir, "bin", "Rscript.exe"),
    path.join(rHomeDir, "bin", "Rscript"),
    path.join(rHomeDir, "Resources", "bin", "Rscript")
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  return "";
}

function buildHostREnv(rHomeDir, libDir) {
  const env = { ...process.env };
  const pathEntries = [];
  const candidates = [
    path.join(rHomeDir, "bin", "x64"),
    path.join(rHomeDir, "bin")
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate) && !pathEntries.includes(candidate)) {
      pathEntries.push(candidate);
    }
  }

  const currentPath = env.PATH || env.Path || "";
  if (currentPath) {
    pathEntries.push(currentPath);
  }

  env.PATH = pathEntries.join(path.delimiter);
  env.R_LIBS = libDir;
  env.R_LIBS_USER = libDir;
  env.R_LIBS_SITE = libDir;
  if (Object.prototype.hasOwnProperty.call(env, "Path")) {
    env.Path = env.PATH;
  }
  return env;
}

function collectDirectories(root, predicate) {
  const out = [];
  function walk(current) {
    let entries;
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch (_) {
      return;
    }
    for (const entry of entries) {
      if (!entry.isDirectory()) continue;
      const fullPath = path.join(current, entry.name);
      if (predicate(fullPath, entry.name)) {
        out.push(fullPath);
      }
      walk(fullPath);
    }
  }
  if (fs.existsSync(root)) {
    walk(root);
  }
  return out;
}

function removeDirectories(root, predicate) {
  function walk(current) {
    let entries;
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch (_) {
      return;
    }
    for (const entry of entries) {
      if (!entry.isDirectory()) continue;
      const fullPath = path.join(current, entry.name);
      if (predicate(fullPath, entry.name)) {
        fs.rmSync(fullPath, { recursive: true, force: true });
        continue;
      }
      walk(fullPath);
    }
  }
  if (fs.existsSync(root)) {
    walk(root);
  }
}

function formatTimestampForFilename(date = new Date()) {
  const yyyy = String(date.getFullYear());
  const mm = String(date.getMonth() + 1).padStart(2, "0");
  const dd = String(date.getDate()).padStart(2, "0");
  const hh = String(date.getHours()).padStart(2, "0");
  const min = String(date.getMinutes()).padStart(2, "0");
  const ss = String(date.getSeconds()).padStart(2, "0");
  return `${yyyy}${mm}${dd}-${hh}${min}${ss}`;
}

async function archiveSymbols(runtimeRoot, symbolsOut) {
  const dsymDirs = collectDirectories(runtimeRoot, (_fullPath, name) => name.endsWith(".dSYM"));
  if (dsymDirs.length < 1) {
    console.log("[build-r-runtime] no .dSYM found; symbols archive skipped.");
    return;
  }

  await fs.promises.mkdir(symbolsOut, { recursive: true });
  const archivePath = path.join(symbolsOut, `r-runtime-dsym-${formatTimestampForFilename()}.tar.gz`);
  const relativePaths = dsymDirs.map((dir) => path.relative(runtimeRoot, dir));

  try {
    await runCommand("tar", ["-czf", archivePath, "-C", runtimeRoot, ...relativePaths]);
    console.log(`[build-r-runtime] symbols archive: ${archivePath}`);
  } catch (error) {
    console.warn(`[build-r-runtime] WARN: failed to archive symbols: ${error.message}`);
  }
}

function countFiles(target) {
  if (!fs.existsSync(target)) return 0;
  const stack = [target];
  let total = 0;

  while (stack.length > 0) {
    const current = stack.pop();
    let stat;
    try {
      stat = fs.lstatSync(current);
    } catch (_) {
      continue;
    }

    if (stat.isSymbolicLink()) continue;
    if (stat.isDirectory()) {
      let children;
      try {
        children = fs.readdirSync(current);
      } catch (_) {
        continue;
      }
      for (const child of children) {
        stack.push(path.join(current, child));
      }
      continue;
    }

    total += 1;
  }

  return total;
}

function getPathSizeBytes(target) {
  if (!fs.existsSync(target)) return 0;
  const stack = [target];
  let total = 0;

  while (stack.length > 0) {
    const current = stack.pop();
    let stat;
    try {
      stat = fs.lstatSync(current);
    } catch (_) {
      continue;
    }

    if (stat.isSymbolicLink()) continue;
    if (stat.isDirectory()) {
      let children;
      try {
        children = fs.readdirSync(current);
      } catch (_) {
        continue;
      }
      for (const child of children) {
        stack.push(path.join(current, child));
      }
      continue;
    }
    total += stat.size;
  }

  return total;
}

function collectTopPaths(root, maxDepth = 2, limit = 80) {
  const rows = [];

  function walk(current, depth) {
    let total = 0;
    let entries = [];

    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch (_) {
      return 0;
    }

    for (const entry of entries) {
      const fullPath = path.join(current, entry.name);
      if (entry.isSymbolicLink()) continue;
      if (entry.isDirectory()) {
        total += walk(fullPath, depth + 1);
      } else {
        try {
          total += fs.lstatSync(fullPath).size;
        } catch (_) {
          // ignore
        }
      }
    }

    if (depth <= maxDepth) {
      rows.push({
        rel: path.relative(root, current) || ".",
        bytes: total
      });
    }

    return total;
  }

  if (fs.existsSync(root)) {
    walk(root, 0);
  }

  return rows.sort((a, b) => b.bytes - a.bytes).slice(0, limit);
}

function mb1(bytes) {
  return (bytes / (1024 * 1024)).toFixed(1);
}

async function main() {
  const cfg = parseArgs(process.argv.slice(2));
  const runtimeWarnMaxMb = readPositiveIntEnv("ITCSUITE_RUNTIME_WARN_MAX_MB", DEFAULT_RUNTIME_WARN_MAX_MB);
  const runtimeWarnMaxFiles = readPositiveIntEnv("ITCSUITE_RUNTIME_WARN_MAX_FILES", DEFAULT_RUNTIME_WARN_MAX_FILES);

  console.log(`[build-r-runtime] desktop: ${desktopDir}`);
  console.log(`[build-r-runtime] out: ${cfg.outDir}`);
  console.log(`[build-r-runtime] profile: ${cfg.profile}`);
  console.log(`[build-r-runtime] manifest: ${cfg.manifestPath}`);
  console.log(`[build-r-runtime] strict manifest: ${cfg.strictRuntimeManifest ? 1 : 0}`);
  console.log(`[build-r-runtime] warn max size: ${runtimeWarnMaxMb}MB`);
  console.log(`[build-r-runtime] warn max files: ${runtimeWarnMaxFiles}`);

  const rHomeResult = await runCommand(cfg.rBin, ["RHOME"], { capture: true });
  const rHomeDir = rHomeResult.stdout.trim();
  if (!rHomeDir || !fs.existsSync(rHomeDir)) {
    throw new Error(`invalid RHOME: ${rHomeDir || "<empty>"}`);
  }
  const rHomeRealDir = fs.realpathSync(rHomeDir);

  console.log(`[build-r-runtime] R_HOME=${rHomeDir}`);
  if (rHomeRealDir !== rHomeDir) {
    console.log(`[build-r-runtime] R_HOME(realpath)=${rHomeRealDir}`);
  }

  await fs.promises.rm(cfg.outDir, { recursive: true, force: true });
  await fs.promises.mkdir(path.dirname(cfg.outDir), { recursive: true });
  await fs.promises.cp(rHomeRealDir, cfg.outDir, { recursive: true, force: true });

  const bundledRscript = resolveBundledRscript(cfg.outDir);
  if (!bundledRscript) {
    throw new Error(`Rscript missing after copy under ${cfg.outDir}`);
  }
  const hostRscript = resolveHostRscript(rHomeRealDir);
  if (!hostRscript) {
    throw new Error(`Host Rscript not found under ${rHomeRealDir}`);
  }

  const libDir = path.join(cfg.outDir, "library");
  await fs.promises.mkdir(libDir, { recursive: true });

  let requiredPkgs = readManifestPackages(cfg.manifestPath);
  if (!requiredPkgs) {
    if (cfg.strictRuntimeManifest) {
      throw new Error(`manifest missing: ${cfg.manifestPath}`);
    }
    console.warn("[build-r-runtime] WARN: manifest missing, fallback package list will be used.");
    requiredPkgs = [...FALLBACK_PKGS];
  }

  if (requiredPkgs.length < 1) {
    throw new Error("no packages provided by manifest/fallback list");
  }

  const hostREnv = buildHostREnv(rHomeRealDir, libDir);

  await runCommand(hostRscript, ["--vanilla", "-e", INSTALL_SCRIPT], {
    env: {
      ...hostREnv,
      ITCSUITE_LIB_DIR: libDir,
      ITCSUITE_PKG_CSV: requiredPkgs.join(",")
    }
  });

  await runCommand(hostRscript, ["--vanilla", "-e", VERIFY_AND_PRUNE_SCRIPT], {
    env: {
      ...hostREnv,
      ITCSUITE_LIB_DIR: libDir,
      ITCSUITE_MANIFEST_PATH: cfg.manifestPath,
      ITCSUITE_STRICT_RUNTIME_MANIFEST: cfg.strictRuntimeManifest ? "1" : "0"
    }
  });

  const prePruneBytes = getPathSizeBytes(cfg.outDir);

  if (cfg.symbolsOut) {
    await archiveSymbols(cfg.outDir, cfg.symbolsOut);
  }

  if (cfg.profile === "release") {
    removeDirectories(cfg.outDir, (_fullPath, name) => name.endsWith(".dSYM"));
    removeDirectories(cfg.outDir, (_fullPath, name) => PRUNE_DIR_NAMES.has(name));

    if (fs.existsSync(libDir)) {
      const children = fs.readdirSync(libDir, { withFileTypes: true });
      for (const entry of children) {
        if (!entry.isDirectory()) continue;
        if (!/^file[0-9a-f]{6,}$/i.test(entry.name)) continue;
        fs.rmSync(path.join(libDir, entry.name), { recursive: true, force: true });
      }
    }
  }

  const postPruneBytes = getPathSizeBytes(cfg.outDir);
  const postPruneFileCount = countFiles(cfg.outDir);
  const savedBytes = Math.max(0, prePruneBytes - postPruneBytes);
  const sizeWarnExceeded = cfg.profile === "release" && postPruneBytes > runtimeWarnMaxMb * 1024 * 1024;
  const fileWarnExceeded = cfg.profile === "release" && postPruneFileCount > runtimeWarnMaxFiles;
  const reportPath = path.join(cfg.outDir, "runtime-size-report.txt");
  const topPaths = collectTopPaths(cfg.outDir, 2, 80);

  if (sizeWarnExceeded) {
    console.warn(
      `[build-r-runtime] WARN: runtime size ${mb1(postPruneBytes)}MB exceeds warning threshold ${runtimeWarnMaxMb}MB`
    );
  }
  if (fileWarnExceeded) {
    console.warn(
      `[build-r-runtime] WARN: runtime file count ${postPruneFileCount} exceeds warning threshold ${runtimeWarnMaxFiles}`
    );
  }

  const reportLines = [
    `profile=${cfg.profile}`,
    `manifest=${cfg.manifestPath}`,
    `strict_runtime_manifest=${cfg.strictRuntimeManifest ? 1 : 0}`,
    `size_before_prune_mb=${mb1(prePruneBytes)}`,
    `size_after_prune_mb=${mb1(postPruneBytes)}`,
    `size_saved_mb=${mb1(savedBytes)}`,
    `file_count=${postPruneFileCount}`,
    `warn_runtime_max_mb=${runtimeWarnMaxMb}`,
    `warn_runtime_max_files=${runtimeWarnMaxFiles}`,
    `warn_size_exceeded=${sizeWarnExceeded ? 1 : 0}`,
    `warn_file_count_exceeded=${fileWarnExceeded ? 1 : 0}`,
    "",
    "top_paths:"
  ];

  for (const row of topPaths) {
    reportLines.push(`${row.rel}\t${mb1(row.bytes)}MB`);
  }

  fs.writeFileSync(reportPath, `${reportLines.join("\n")}\n`, "utf8");
  console.log(`[build-r-runtime] size report: ${reportPath}`);
  console.log("[build-r-runtime] done");
}

main().catch((error) => {
  console.error(`[build-r-runtime] ERROR: ${error.message}`);
  process.exit(1);
});
