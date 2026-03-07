#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const desktopDir = path.resolve(scriptDir, "..");
const repoRoot = path.resolve(desktopDir, "..");
const runtimeRoot = process.env.ITCSUITE_RUNTIME_ROOT || path.join(desktopDir, "resources", "r-runtime");
const smokeScript = path.join(scriptDir, "runtime-graphics-smoke.R");

function resolveRscript(runtimeDir) {
  const candidates = [
    path.join(runtimeDir, "bin", "itcsuite-rscript"),
    path.join(runtimeDir, "bin", "Rscript.exe"),
    path.join(runtimeDir, "bin", "x64", "Rscript.exe"),
    path.join(runtimeDir, "bin", "Rscript"),
    path.join(runtimeDir, "Resources", "bin", "Rscript")
  ];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  return "";
}

async function main() {
  if (!fs.existsSync(runtimeRoot)) {
    throw new Error(`Bundled runtime missing: ${runtimeRoot}. Build the runtime first.`);
  }
  if (!fs.existsSync(smokeScript)) {
    throw new Error(`Graphics smoke script missing: ${smokeScript}`);
  }

  const rscript = resolveRscript(runtimeRoot);
  if (!rscript) {
    throw new Error(`Bundled Rscript missing under ${runtimeRoot}`);
  }

  const outDir = path.join(os.tmpdir(), `itcsuite-graphics-smoke-${Date.now()}`);
  fs.mkdirSync(outDir, { recursive: true });
  const isolatedUserLib = path.join(outDir, "r-library-empty");
  fs.mkdirSync(isolatedUserLib, { recursive: true });
  const bundledLib = path.join(runtimeRoot, "library");

  const env = {
    ...process.env,
    R_HOME: runtimeRoot,
    R_LIBS: bundledLib,
    R_LIBS_SITE: bundledLib,
    R_LIBS_USER: isolatedUserLib,
    ITCSUITE_RUNTIME_ROOT: runtimeRoot,
    PATH: [
      path.join(runtimeRoot, "bin", "x64"),
      path.join(runtimeRoot, "bin"),
      path.join(runtimeRoot, "Resources", "bin"),
      process.env.PATH || ""
    ].filter(Boolean).join(path.delimiter)
  };

  await new Promise((resolve, reject) => {
    const child = spawn(
      rscript,
      [smokeScript, "--repo-root", repoRoot, "--out-dir", outDir],
      {
        cwd: repoRoot,
        env,
        stdio: ["ignore", "pipe", "pipe"]
      }
    );

    const timeout = setTimeout(() => {
      try {
        child.kill("SIGKILL");
      } catch (_) {
        // Ignore signal failures.
      }
    }, 120000);

    child.stdout.on("data", (chunk) => {
      process.stdout.write(chunk.toString("utf8"));
    });

    child.stderr.on("data", (chunk) => {
      process.stderr.write(chunk.toString("utf8"));
    });

    child.on("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });

    child.on("close", (code) => {
      clearTimeout(timeout);
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Graphics smoke exited with code ${code}`));
      }
    });
  });
}

main().catch((error) => {
  console.error(`Graphics smoke failed: ${error.message}`);
  process.exit(1);
});
