#!/usr/bin/env node

import { spawn, spawnSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(__filename);
const desktopDir = path.resolve(scriptDir, "..");

const DEFAULT_RUNTIME_ROOT = path.join(desktopDir, "resources", "r-runtime");
const DEFAULT_MANIFEST_PATH = path.join(desktopDir, "resources", "r-runtime-manifest.txt");
const HOST_R_FRAMEWORK_RESOURCES_RE = /^\/Library\/Frameworks\/R\.framework\/Versions\/[^/]+\/Resources(?:\/|$)/;
const PORTABLE_RSCRIPT_LAUNCHER_NAME = "itcsuite-rscript";

function usage() {
  console.log(`Usage:
  ${path.basename(__filename)} [options]

Options:
  --runtime-root <dir>               Runtime root path (default: ${DEFAULT_RUNTIME_ROOT})
  --out-dir <dir>                    Alias of --runtime-root
  --profile <release|debug>          Runtime build profile (default: release)
  --manifest <path>                  Runtime package manifest path
  --strict-runtime-manifest [0|1]    Strict manifest mode (default: 1)
  --check-only                       Validate only; do not rebuild on failure
  -h, --help                         Show this help
`);
}

function resolveInputPath(value) {
  if (!value) return value;
  if (path.isAbsolute(value)) return value;
  return path.resolve(process.cwd(), value);
}

function parseArgs(argv) {
  let runtimeRoot = DEFAULT_RUNTIME_ROOT;
  let profile = "release";
  let manifestPath = DEFAULT_MANIFEST_PATH;
  let strictRuntimeManifest = true;
  let checkOnly = false;

  const args = [...argv];
  while (args.length > 0) {
    const arg = args.shift();
    switch (arg) {
      case "--runtime-root":
      case "--out-dir":
        runtimeRoot = args.shift() || "";
        break;
      case "--profile":
        profile = args.shift() || "";
        break;
      case "--manifest":
        manifestPath = args.shift() || "";
        break;
      case "--strict-runtime-manifest": {
        const maybe = args[0];
        if (maybe === "0" || maybe === "1") {
          strictRuntimeManifest = maybe === "1";
          args.shift();
        } else {
          strictRuntimeManifest = true;
        }
        break;
      }
      case "--check-only":
        checkOnly = true;
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

  if (!runtimeRoot) {
    throw new Error("--runtime-root cannot be empty");
  }
  if (!manifestPath) {
    throw new Error("--manifest cannot be empty");
  }
  if (profile !== "release" && profile !== "debug") {
    throw new Error("--profile must be release or debug");
  }

  return {
    runtimeRoot: resolveInputPath(runtimeRoot),
    profile,
    manifestPath: resolveInputPath(manifestPath),
    strictRuntimeManifest,
    checkOnly
  };
}

function resolveBundledRscript(runtimeRoot) {
  const candidates = [
    path.join(runtimeRoot, "bin", "Rscript.exe"),
    path.join(runtimeRoot, "bin", "x64", "Rscript.exe"),
    path.join(runtimeRoot, "bin", "Rscript"),
    path.join(runtimeRoot, "Resources", "bin", "Rscript")
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  return "";
}

function isExecutable(target) {
  if (process.platform === "win32") return true;
  try {
    fs.accessSync(target, fs.constants.X_OK);
    return true;
  } catch (_) {
    return false;
  }
}

function runCaptureSync(command, args) {
  const result = spawnSync(command, args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"]
  });
  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    return "";
  }
  return result.stdout || "";
}

function parseOtoolLinkedLibraries(output) {
  const refs = [];
  const lines = output.split(/\r?\n/).slice(1);
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

function hasHostReferenceInLinkedLibraries(binaryPath) {
  if (!fs.existsSync(binaryPath)) return false;
  let output = "";
  try {
    output = runCaptureSync("otool", ["-L", binaryPath]);
  } catch (_) {
    return false;
  }
  if (!output) return false;
  const refs = parseOtoolLinkedLibraries(output);
  return refs.some((ref) => HOST_R_FRAMEWORK_RESOURCES_RE.test(ref));
}

function hasHostReferenceSymlink(linkPath) {
  if (!fs.existsSync(linkPath)) return false;
  let stat;
  try {
    stat = fs.lstatSync(linkPath);
  } catch (_) {
    return false;
  }
  if (!stat.isSymbolicLink()) return false;
  let target = "";
  try {
    target = fs.readlinkSync(linkPath);
  } catch (_) {
    return false;
  }
  if (!target || !path.isAbsolute(target)) return false;
  return HOST_R_FRAMEWORK_RESOURCES_RE.test(target);
}

function checkRuntime(runtimeRoot) {
  if (!fs.existsSync(runtimeRoot)) {
    return {
      ok: false,
      reason: `runtime root missing: ${runtimeRoot}`
    };
  }
  const stat = fs.lstatSync(runtimeRoot);
  if (stat.isSymbolicLink()) {
    return {
      ok: false,
      reason: `runtime root must be a real directory, but is symlink: ${runtimeRoot}`
    };
  }

  const rscript = resolveBundledRscript(runtimeRoot);
  if (!rscript) {
    return {
      ok: false,
      reason: `Bundled Rscript not found under ${runtimeRoot}`
    };
  }

  if (!isExecutable(rscript)) {
    return {
      ok: false,
      reason: `Bundled Rscript is not executable: ${rscript}`
    };
  }

  if (process.platform === "darwin") {
    const execR = path.join(runtimeRoot, "bin", "exec", "R");
    if (hasHostReferenceInLinkedLibraries(execR)) {
      return {
        ok: false,
        reason: `Bundled runtime still links to host R.framework: ${execR}`
      };
    }

    const libR = path.join(runtimeRoot, "lib", "libR.dylib");
    if (hasHostReferenceInLinkedLibraries(libR)) {
      return {
        ok: false,
        reason: `Bundled runtime still links to host R.framework: ${libR}`
      };
    }

    const libRblasSymlink = path.join(runtimeRoot, "lib", "libRblas.dylib");
    if (hasHostReferenceSymlink(libRblasSymlink)) {
      return {
        ok: false,
        reason: `Bundled runtime has host-absolute symlink: ${libRblasSymlink}`
      };
    }

    const portableLauncher = path.join(runtimeRoot, "bin", PORTABLE_RSCRIPT_LAUNCHER_NAME);
    if (!fs.existsSync(portableLauncher) || !isExecutable(portableLauncher)) {
      return {
        ok: false,
        reason: `Bundled runtime missing portable launcher: ${portableLauncher}`
      };
    }
  }

  return {
    ok: true,
    rscript
  };
}

function runBuildRuntime(cfg) {
  const buildScript = path.join(scriptDir, "build-r-runtime.mjs");
  const args = [
    buildScript,
    cfg.runtimeRoot,
    "--profile",
    cfg.profile,
    "--manifest",
    cfg.manifestPath
  ];
  if (cfg.strictRuntimeManifest) {
    args.push("--strict-runtime-manifest");
  }

  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, args, {
      cwd: desktopDir,
      env: process.env,
      stdio: "inherit"
    });

    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`build-r-runtime failed with code ${code}`));
      }
    });
  });
}

async function main() {
  const cfg = parseArgs(process.argv.slice(2));
  const before = checkRuntime(cfg.runtimeRoot);
  if (before.ok) {
    console.log(`[ensure-r-runtime] runtime ready: ${before.rscript}`);
    return;
  }

  if (cfg.checkOnly) {
    throw new Error(`runtime missing or invalid: ${before.reason}`);
  }

  console.log(`[ensure-r-runtime] runtime missing or invalid: ${before.reason}`);
  console.log("[ensure-r-runtime] rebuilding bundled runtime...");
  await runBuildRuntime(cfg);

  const after = checkRuntime(cfg.runtimeRoot);
  if (!after.ok) {
    throw new Error(`runtime verification failed after rebuild: ${after.reason}`);
  }

  console.log(`[ensure-r-runtime] runtime ready: ${after.rscript}`);
}

main().catch((error) => {
  console.error(`[ensure-r-runtime] ERROR: ${error.message}`);
  process.exit(1);
});
