#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";
import {
  DEFAULT_MACOS_MIN_VERSION,
  PORTABLE_RSCRIPT_LAUNCHER_NAME,
  collectMachOBinaries,
  collectSymlinks,
  compareMacOSVersions,
  findDisallowedAbsoluteReferences,
  isAllowedAbsoluteSystemReference,
  isHostRFrameworkReference,
  normalizeMacOSVersion,
  parseOtoolInstallName,
  parseOtoolLinkedLibraries,
  parseOtoolMinimumMacOSVersions
} from "./runtime-macos-utils.mjs";

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
  "ragg",
  "systemfonts",
  "textshaping",
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

const DEFAULT_RUNTIME_MACOS_MIN_VERSION = DEFAULT_MACOS_MIN_VERSION;

const INSTALL_SCRIPT = `
lib_dir <- Sys.getenv("ITCSUITE_LIB_DIR")
pkg_csv <- Sys.getenv("ITCSUITE_PKG_CSV")
repo <- Sys.getenv("ITCSUITE_R_REPO", unset = "https://cloud.r-project.org")
pkg_type <- Sys.getenv("ITCSUITE_R_PKG_TYPE", unset = "")
timeout_secs <- suppressWarnings(as.integer(Sys.getenv("ITCSUITE_R_TIMEOUT", unset = "600")))
retry_count <- suppressWarnings(as.integer(Sys.getenv("ITCSUITE_R_RETRY", unset = "2")))
is_darwin <- identical(Sys.info()[["sysname"]], "Darwin")
if (!nzchar(lib_dir) || !nzchar(pkg_csv)) stop("missing env")
if (!nzchar(repo)) repo <- "https://cloud.r-project.org"
if (!is.finite(timeout_secs) || is.na(timeout_secs) || timeout_secs < 1L) timeout_secs <- 600L
if (!is.finite(retry_count) || is.na(retry_count) || retry_count < 0L) retry_count <- 2L
if (!nzchar(pkg_type)) {
  pkg_type <- if (.Platform$OS.type == "windows" || is_darwin) "binary" else getOption("pkgType")
}
if (!nzchar(pkg_type)) pkg_type <- if (is_darwin) "binary" else "source"
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(lib_dir)
options(timeout = timeout_secs)
options(repos = c(CRAN = repo))
options(pkgType = pkg_type)
if (is_darwin && !identical(getOption("pkgType"), "binary")) {
  stop(
    paste(
      "macOS bundled runtime builds must use CRAN binary packages.",
      "Set ITCSUITE_R_PKG_TYPE=binary and use an R version that still has macOS binaries available."
    )
  )
}
cat(sprintf(
  "[build-r-runtime] install config: repo=%s pkgType=%s timeout=%ss retry=%d\\n",
  getOption("repos")[["CRAN"]],
  getOption("pkgType"),
  getOption("timeout"),
  retry_count
))
cat(sprintf("[build-r-runtime] capabilities(libcurl)=%s\\n", capabilities("libcurl")))
pkgs <- strsplit(pkg_csv, ",", fixed = TRUE)[[1]]
dep_db <- available.packages(repos = getOption("repos")[["CRAN"]], type = getOption("pkgType"))
dep_fields <- c("Depends", "Imports", "LinkingTo")
dep_map <- tools::package_dependencies(pkgs, db = dep_db, which = dep_fields, recursive = TRUE)
pkgs <- unique(c(pkgs, unlist(dep_map, use.names = FALSE)))
pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
installed_names <- rownames(installed.packages(lib.loc = lib_dir))
missing <- setdiff(pkgs, installed_names)
if (length(missing) > 0) {
  attempts <- retry_count + 1L
  remaining <- missing
  for (attempt in seq_len(attempts)) {
    cat(sprintf(
      "[build-r-runtime] install attempt %d/%d (%d package(s)): %s\\n",
      attempt,
      attempts,
      length(remaining),
      paste(remaining, collapse = ", ")
    ))
    tryCatch(
      install.packages(
        remaining,
        repos = getOption("repos")[["CRAN"]],
        lib = lib_dir,
        type = getOption("pkgType"),
        dependencies = c("Depends", "Imports", "LinkingTo")
      ),
      error = function(err) {
        message(sprintf("[build-r-runtime] install attempt %d error: %s", attempt, conditionMessage(err)))
      }
    )
    installed_names <- rownames(installed.packages(lib.loc = lib_dir))
    remaining <- setdiff(remaining, installed_names)
    if (length(remaining) < 1L) break
    if (attempt < attempts) {
      cat(sprintf(
        "[build-r-runtime] retrying in 10s; remaining %d package(s): %s\\n",
        length(remaining),
        paste(remaining, collapse = ", ")
      ))
      Sys.sleep(10L)
    }
  }
}
installed_names <- rownames(installed.packages(lib.loc = lib_dir))
remaining <- setdiff(pkgs, installed_names)
if (length(remaining) > 0) {
  stop(sprintf("Failed to install packages: %s", paste(remaining, collapse = ", ")))
}
cat(sprintf("Installed/verified %d packages in %s\\n", length(pkgs), lib_dir))
`;

const SANITIZE_STAGED_LIBRARY_SCRIPT = `
lib_dir <- Sys.getenv("ITCSUITE_LIB_DIR")
if (!dir.exists(lib_dir)) stop("library dir does not exist")
ip <- installed.packages(lib.loc = lib_dir)
if (nrow(ip) < 1L) q(status = 0L)
priority <- ip[, "Priority"]
priority[is.na(priority)] <- ""
drop <- rownames(ip)[!priority %in% c("base", "recommended")]
if (length(drop) > 0L) {
  unlink(file.path(lib_dir, drop), recursive = TRUE, force = TRUE)
  cat(sprintf("Removed %d staged non-base package(s) before runtime install.\\n", length(drop)))
} else {
  cat("No staged non-base packages to remove before runtime install.\\n")
}
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

const MACOS_OPTIONAL_PRUNE_PATHS = [
  "modules/R_X11.so",
  "modules/R_de.so",
  "library/tcltk",
  "library/grDevices/libs/cairo.so"
];

const DEFAULT_RUNTIME_WARN_MAX_MB = 260;
const DEFAULT_RUNTIME_WARN_MAX_FILES = 8000;
const HOST_R_FRAMEWORK_RESOURCES_RE = /^\/Library\/Frameworks\/R\.framework\/Versions\/[^/]+\/Resources(?:\/(.*))?$/;

function usage() {
  console.log(`Usage:
  ${path.basename(__filename)} [out_dir] [options]

Options:
  [out_dir]                    Optional runtime output dir (default: ${DEFAULT_OUT_DIR})
  --out-dir <dir>              Runtime output dir
  --profile <release|debug>    Build profile (default: release)
  --manifest <path>            Runtime package manifest path
  --strict-runtime-manifest    Fail if manifest package is missing
  --macos-min-version <ver>    Maximum supported minos for bundled binaries (default: ${DEFAULT_RUNTIME_MACOS_MIN_VERSION})
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
  let macosMinVersion = DEFAULT_RUNTIME_MACOS_MIN_VERSION;
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
      case "--macos-min-version":
        macosMinVersion = args.shift() || "";
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
    macosMinVersion: normalizeMacOSVersion(macosMinVersion),
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

function normalizePrefix(prefix) {
  if (!prefix) return "";
  return prefix.endsWith(path.sep) ? prefix.slice(0, -1) : prefix;
}

function toPosixPath(value) {
  return value.split(path.sep).join("/");
}

function getRuntimeRelativePath(reference, sourcePrefixes) {
  for (const rawPrefix of sourcePrefixes) {
    const prefix = normalizePrefix(rawPrefix);
    if (!prefix) continue;
    if (reference === prefix) return "";
    if (reference.startsWith(`${prefix}/`)) {
      return reference.slice(prefix.length + 1);
    }
  }

  const match = reference.match(HOST_R_FRAMEWORK_RESOURCES_RE);
  if (!match) return null;
  return match[1] || "";
}

function resolveMappedRuntimeTarget(reference, runtimeRoot, sourcePrefixes) {
  const rel = getRuntimeRelativePath(reference, sourcePrefixes);
  if (rel === null) return "";
  if (!rel) return runtimeRoot;
  return path.join(runtimeRoot, rel);
}

function toLoaderPath(fromDir, targetPath) {
  let rel = path.relative(fromDir, targetPath);
  if (!rel || rel === ".") {
    rel = path.basename(targetPath);
  }
  return `@loader_path/${toPosixPath(rel)}`;
}

async function relocateMacRuntimeReferences(runtimeRoot, sourcePrefixes) {
  if (process.platform !== "darwin") {
    return {
      rewrittenFiles: 0,
      rewrittenReferences: 0,
      rewrittenInstallNames: 0,
      rewrittenSymlinks: 0
    };
  }

  const binaries = collectMachOBinaries(runtimeRoot);
  const unresolved = [];
  let rewrittenFiles = 0;
  let rewrittenReferences = 0;
  let rewrittenInstallNames = 0;

  for (const binaryPath of binaries) {
    let deps = [];
    try {
      const linked = await runCommand("otool", ["-L", binaryPath], { capture: true });
      deps = parseOtoolLinkedLibraries(linked.stdout);
    } catch (_) {
      continue;
    }

    const changes = new Map();
    for (const dep of deps) {
      if (!dep.startsWith("/")) continue;
      const mapped = resolveMappedRuntimeTarget(dep, runtimeRoot, sourcePrefixes);
      if (!mapped) continue;
      if (!fs.existsSync(mapped)) {
        unresolved.push(`${path.relative(runtimeRoot, binaryPath)} -> ${dep}`);
        continue;
      }
      const replacement = toLoaderPath(path.dirname(binaryPath), mapped);
      if (replacement !== dep) {
        changes.set(dep, replacement);
      }
    }

    let rewrittenInstallName = "";
    try {
      const installNameRaw = await runCommand("otool", ["-D", binaryPath], { capture: true });
      const installName = parseOtoolInstallName(installNameRaw.stdout);
      if (installName && installName.startsWith("/")) {
        const mapped = resolveMappedRuntimeTarget(installName, runtimeRoot, sourcePrefixes);
        if (mapped && fs.existsSync(mapped)) {
          const replacement = toLoaderPath(path.dirname(binaryPath), mapped);
          if (replacement !== installName) {
            rewrittenInstallName = replacement;
          }
        } else if (isHostRFrameworkReference(installName)) {
          unresolved.push(`${path.relative(runtimeRoot, binaryPath)} -> ${installName}`);
        }
      }
    } catch (_) {
      // Executables generally don't have LC_ID_DYLIB.
    }

    if (changes.size < 1 && !rewrittenInstallName) {
      continue;
    }

    const args = [];
    if (rewrittenInstallName) {
      args.push("-id", rewrittenInstallName);
    }
    for (const [from, to] of changes.entries()) {
      args.push("-change", from, to);
    }
    args.push(binaryPath);
    await runCommand("install_name_tool", args);

    rewrittenFiles += 1;
    rewrittenReferences += changes.size;
    if (rewrittenInstallName) {
      rewrittenInstallNames += 1;
    }
  }

  let rewrittenSymlinks = 0;
  const symlinks = collectSymlinks(runtimeRoot);
  for (const linkPath of symlinks) {
    let target;
    try {
      target = fs.readlinkSync(linkPath);
    } catch (_) {
      continue;
    }
    if (!path.isAbsolute(target)) continue;
    const mapped = resolveMappedRuntimeTarget(target, runtimeRoot, sourcePrefixes);
    if (!mapped) continue;
    if (!fs.existsSync(mapped)) {
      unresolved.push(`${path.relative(runtimeRoot, linkPath)} -> ${target}`);
      continue;
    }
    const relativeTarget = path.relative(path.dirname(linkPath), mapped) || path.basename(mapped);
    fs.rmSync(linkPath, { force: true });
    fs.symlinkSync(relativeTarget, linkPath);
    rewrittenSymlinks += 1;
  }

  if (unresolved.length > 0) {
    const sample = unresolved.slice(0, 12).join("\n  ");
    throw new Error(
      `[relocate-runtime] unresolved host R references (${unresolved.length}):\n  ${sample}`
    );
  }

  return {
    rewrittenFiles,
    rewrittenReferences,
    rewrittenInstallNames,
    rewrittenSymlinks
  };
}

async function assertNoHostRReferences(runtimeRoot) {
  if (process.platform !== "darwin") {
    return;
  }

  const offenders = [];
  const binaries = collectMachOBinaries(runtimeRoot);

  for (const binaryPath of binaries) {
    if (offenders.length >= 12) break;

    try {
      const linked = await runCommand("otool", ["-L", binaryPath], { capture: true });
      const deps = parseOtoolLinkedLibraries(linked.stdout);
      for (const dep of deps) {
        if (isHostRFrameworkReference(dep)) {
          offenders.push(`${path.relative(runtimeRoot, binaryPath)} -> ${dep}`);
          if (offenders.length >= 12) break;
        }
      }
    } catch (_) {
      // Ignore non-dylib files.
    }

    if (offenders.length >= 12) break;

    try {
      const installNameRaw = await runCommand("otool", ["-D", binaryPath], { capture: true });
      const installName = parseOtoolInstallName(installNameRaw.stdout);
      if (installName && isHostRFrameworkReference(installName)) {
        offenders.push(`${path.relative(runtimeRoot, binaryPath)} -> ${installName}`);
      }
    } catch (_) {
      // Ignore non-dylib files.
    }
  }

  if (offenders.length < 12) {
    const symlinks = collectSymlinks(runtimeRoot);
    for (const linkPath of symlinks) {
      if (offenders.length >= 12) break;
      let target;
      try {
        target = fs.readlinkSync(linkPath);
      } catch (_) {
        continue;
      }
      if (target && isHostRFrameworkReference(target)) {
        offenders.push(`${path.relative(runtimeRoot, linkPath)} -> ${target}`);
      }
    }
  }

  if (offenders.length > 0) {
    throw new Error(
      `[relocate-runtime] non-relocatable host R references remain:\n  ${offenders.join("\n  ")}`
    );
  }
}

async function validateDarwinRuntimeCompatibility(runtimeRoot, maximumVersion) {
  if (process.platform !== "darwin") {
    return {
      scannedBinaries: 0,
      scannedSymlinks: 0,
      disallowedReferences: 0,
      unsupportedMinos: 0
    };
  }

  const offenders = [];
  let disallowedReferences = 0;
  let unsupportedMinos = 0;

  const binaries = collectMachOBinaries(runtimeRoot);
  for (const binaryPath of binaries) {
    const relPath = path.relative(runtimeRoot, binaryPath);
    const binaryRealPath = path.resolve(binaryPath);

    const linked = await runCommand("otool", ["-L", binaryPath], { capture: true });
    const linkedRefs = parseOtoolLinkedLibraries(linked.stdout).filter((reference) => {
      if (!reference.startsWith("/")) return true;
      return path.resolve(reference) !== binaryRealPath;
    });
    const disallowedLinkedRefs = findDisallowedAbsoluteReferences(linkedRefs);
    disallowedReferences += disallowedLinkedRefs.length;
    for (const ref of disallowedLinkedRefs) {
      if (offenders.length < 20) offenders.push(`${relPath} -> ${ref}`);
    }

    try {
      const installNameRaw = await runCommand("otool", ["-D", binaryPath], { capture: true });
      const installName = parseOtoolInstallName(installNameRaw.stdout);
      const disallowedInstallNames = findDisallowedAbsoluteReferences(installName ? [installName] : []);
      disallowedReferences += disallowedInstallNames.length;
      for (const ref of disallowedInstallNames) {
        if (offenders.length < 20) offenders.push(`${relPath} (install name) -> ${ref}`);
      }
    } catch (_) {
      // Executables generally don't have LC_ID_DYLIB.
    }

    const loadCommands = await runCommand("otool", ["-l", binaryPath], { capture: true });
    const minosValues = parseOtoolMinimumMacOSVersions(loadCommands.stdout);
    const unsupportedVersions = minosValues.filter((version) => compareMacOSVersions(version, maximumVersion) > 0);
    unsupportedMinos += unsupportedVersions.length;
    for (const version of unsupportedVersions) {
      if (offenders.length < 20) offenders.push(`${relPath} (minos ${version} > ${maximumVersion})`);
    }
  }

  const symlinks = collectSymlinks(runtimeRoot);
  for (const linkPath of symlinks) {
    let target = "";
    try {
      target = fs.readlinkSync(linkPath);
    } catch (_) {
      continue;
    }
    if (!target || !path.isAbsolute(target)) continue;
    if (isAllowedAbsoluteSystemReference(target)) continue;
    disallowedReferences += 1;
    if (offenders.length < 20) offenders.push(`${path.relative(runtimeRoot, linkPath)} -> ${target}`);
  }

  if (offenders.length > 0) {
    throw new Error(
      `[validate-runtime] incompatible macOS runtime artifacts detected:\n  ${offenders.join("\n  ")}`
    );
  }

  return {
    scannedBinaries: binaries.length,
    scannedSymlinks: symlinks.length,
    disallowedReferences,
    unsupportedMinos
  };
}

function writePortableRscriptLauncher(runtimeRoot) {
  if (process.platform === "win32") {
    return "";
  }

  const binDir = path.join(runtimeRoot, "bin");
  const execR = path.join(binDir, "exec", "R");
  if (!fs.existsSync(execR)) {
    throw new Error(`cannot create ${PORTABLE_RSCRIPT_LAUNCHER_NAME}: missing ${execR}`);
  }

  const launcherPath = path.join(binDir, PORTABLE_RSCRIPT_LAUNCHER_NAME);
  const launcherScript = [
    "#!/bin/sh",
    "set -e",
    "SCRIPT_DIR=\"$(CDPATH= cd -- \"$(dirname \"$0\")\" && pwd)\"",
    "R_HOME_DIR=\"$(CDPATH= cd -- \"${SCRIPT_DIR}/..\" && pwd)\"",
    "R_ARCH=\"${R_ARCH:-}\"",
    "R_EXEC=\"${R_HOME_DIR}/bin/exec${R_ARCH}/R\"",
    "",
    "if [ ! -x \"${R_EXEC}\" ]; then",
    "  echo \"R launcher error: executable not found: ${R_EXEC}\" >&2",
    "  exit 127",
    "fi",
    "",
    "export R_HOME=\"${R_HOME_DIR}\"",
    "export R_SHARE_DIR=\"${R_HOME_DIR}/share\"",
    "export R_INCLUDE_DIR=\"${R_HOME_DIR}/include\"",
    "export R_DOC_DIR=\"${R_HOME_DIR}/doc\"",
    "",
    "LDPATHS=\"${R_HOME_DIR}/etc${R_ARCH}/ldpaths\"",
    "if [ -f \"${LDPATHS}\" ]; then",
    "  # shellcheck disable=SC1090",
    "  . \"${LDPATHS}\"",
    "fi",
    "",
    "if [ \"$#\" -eq 0 ]; then",
    "  exec \"${R_EXEC}\" --no-echo --no-restore",
    "fi",
    "",
    "case \"$1\" in",
    "  --help|-h)",
    "    echo \"Usage: ${0##*/} [R options] file [args]\"",
    "    echo \"       ${0##*/} [R options] -e expr [-e expr2 ...] [args]\"",
    "    exit 0",
    "    ;;",
    "  --version)",
    "    exec \"${R_EXEC}\" --version",
    "    ;;",
    "esac",
    "",
    "if [ \"$1\" = \"-e\" ] || [ \"$1\" = \"--args\" ] || [ \"${1#--default-packages=}\" != \"$1\" ]; then",
    "  exec \"${R_EXEC}\" --no-echo --no-restore \"$@\"",
    "fi",
    "",
    "SCRIPT_FILE=\"$1\"",
    "shift",
    "exec \"${R_EXEC}\" --no-echo --no-restore \"--file=${SCRIPT_FILE}\" --args \"$@\"",
    ""
  ].join("\n");

  fs.writeFileSync(launcherPath, launcherScript, "utf8");
  fs.chmodSync(launcherPath, 0o755);
  return launcherPath;
}

async function adHocSignDarwinRuntime(runtimeRoot) {
  const binaries = collectMachOBinaries(runtimeRoot).sort();
  let signedCount = 0;
  for (const binaryPath of binaries) {
    await runCommand("codesign", ["--force", "--sign", "-", binaryPath], {
      capture: true
    });
    signedCount += 1;
  }
  return signedCount;
}

async function probePortableLauncher(runtimeRoot) {
  const launcherPath = path.join(runtimeRoot, "bin", PORTABLE_RSCRIPT_LAUNCHER_NAME);
  if (!fs.existsSync(launcherPath)) {
    throw new Error(`portable launcher missing under ${runtimeRoot}`);
  }

  const bundledLib = path.join(runtimeRoot, "library");
  const isolatedUserLib = path.join(runtimeRoot, ".itcsuite-empty-library");
  fs.mkdirSync(isolatedUserLib, { recursive: true });

  await runCommand(
    launcherPath,
    [
      "-e",
      "suppressPackageStartupMessages({library(jsonlite); library(ragg); library(systemfonts); library(textshaping)}); cat('ITCSUITE_RUNTIME_PROBE ok\\n')"
    ],
    {
      cwd: runtimeRoot,
      env: {
        ...process.env,
        R_HOME: runtimeRoot,
        R_LIBS: bundledLib,
        R_LIBS_SITE: bundledLib,
        R_LIBS_USER: isolatedUserLib
      },
      capture: true
    }
  );
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

function buildRuntimeBuildEnv(runtimeRoot, libDir, options = {}) {
  const {
    rHomeOverride = runtimeRoot
  } = options;
  const env = { ...process.env };
  const pathEntries = [];
  const candidates = [
    path.join(rHomeOverride, "bin", "x64"),
    path.join(rHomeOverride, "bin"),
    path.join(rHomeOverride, "Resources", "bin")
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
  env.R_HOME = rHomeOverride;
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

function pruneOptionalMacArtifacts(runtimeRoot) {
  if (process.platform !== "darwin") return [];
  const removed = [];
  for (const relPath of MACOS_OPTIONAL_PRUNE_PATHS) {
    const fullPath = path.join(runtimeRoot, relPath);
    if (!fs.existsSync(fullPath)) continue;
    fs.rmSync(fullPath, { recursive: true, force: true });
    removed.push(relPath);
  }
  return removed;
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
  console.log(`[build-r-runtime] macOS min version gate: ${cfg.macosMinVersion}`);
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
  const hostRscript = resolveBundledRscript(rHomeRealDir);
  if (!hostRscript) {
    throw new Error(`host Rscript missing under ${rHomeRealDir}`);
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

  // Windows packaged R builds are not stable to execute directly from the staged copy
  // during runtime assembly. Use the host Rscript to populate the staged library there.
  const buildRscript = process.platform === "win32" ? hostRscript : bundledRscript;
  const buildREnv = buildRuntimeBuildEnv(cfg.outDir, libDir, {
    rHomeOverride: process.platform === "win32" ? rHomeRealDir : cfg.outDir
  });

  await runCommand(buildRscript, ["--vanilla", "-e", SANITIZE_STAGED_LIBRARY_SCRIPT], {
    env: {
      ...buildREnv,
      ITCSUITE_LIB_DIR: libDir
    }
  });

  await runCommand(buildRscript, ["--vanilla", "-e", INSTALL_SCRIPT], {
    env: {
      ...buildREnv,
      ITCSUITE_LIB_DIR: libDir,
      ITCSUITE_PKG_CSV: requiredPkgs.join(",")
    }
  });

  await runCommand(buildRscript, ["--vanilla", "-e", VERIFY_AND_PRUNE_SCRIPT], {
    env: {
      ...buildREnv,
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

  const removedMacArtifacts = pruneOptionalMacArtifacts(cfg.outDir);
  if (removedMacArtifacts.length > 0) {
    console.log(
      `[build-r-runtime] pruned optional macOS components: ${removedMacArtifacts.join(", ")}`
    );
  }

  const relocationSummary = await relocateMacRuntimeReferences(cfg.outDir, [rHomeRealDir, rHomeDir]);
  if (process.platform === "darwin") {
    console.log(
      `[build-r-runtime] relocated refs: files=${relocationSummary.rewrittenFiles}, refs=${relocationSummary.rewrittenReferences}, ids=${relocationSummary.rewrittenInstallNames}, symlinks=${relocationSummary.rewrittenSymlinks}`
    );
    await assertNoHostRReferences(cfg.outDir);
    console.log("[build-r-runtime] relocation verification: OK");
    const compatibilitySummary = await validateDarwinRuntimeCompatibility(cfg.outDir, cfg.macosMinVersion);
    console.log(
      `[build-r-runtime] macOS compatibility: binaries=${compatibilitySummary.scannedBinaries}, symlinks=${compatibilitySummary.scannedSymlinks}, max_minos=${cfg.macosMinVersion}`
    );
  }

  const launcherPath = writePortableRscriptLauncher(cfg.outDir);
  if (launcherPath) {
    console.log(`[build-r-runtime] portable launcher: ${launcherPath}`);
  }
  if (process.platform === "darwin") {
    const signedCount = await adHocSignDarwinRuntime(cfg.outDir);
    console.log(`[build-r-runtime] ad-hoc signed macOS runtime binaries: ${signedCount}`);
    await probePortableLauncher(cfg.outDir);
    console.log("[build-r-runtime] portable launcher probe: OK");
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
    `macos_min_version=${cfg.macosMinVersion}`,
    `optional_macos_prune_count=${removedMacArtifacts.length}`,
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
