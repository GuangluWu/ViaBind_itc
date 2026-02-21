import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

function parseMajor(version) {
  const major = Number(String(version).split(".")[0]);
  return Number.isFinite(major) ? major : NaN;
}

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const desktopRoot = path.resolve(scriptDir, "..");
const projectPackagePath = path.join(desktopRoot, "package.json");
const localElectronPackagePath = path.join(desktopRoot, "node_modules", "electron", "package.json");
const localElectronBinPath = path.join(
  desktopRoot,
  "node_modules",
  ".bin",
  process.platform === "win32" ? "electron.cmd" : "electron"
);

const fixHint = "Run `npm install --include=dev` in desktop/ to restore local Electron.";

if (!fs.existsSync(localElectronPackagePath) || !fs.existsSync(localElectronBinPath)) {
  console.error("Local Electron is missing. Refusing to fall back to global Electron.");
  console.error(fixHint);
  process.exit(1);
}

let projectPackage;
let localElectronPackage;
try {
  projectPackage = readJson(projectPackagePath);
  localElectronPackage = readJson(localElectronPackagePath);
} catch (error) {
  console.error(`Failed to read package metadata: ${error && error.message ? error.message : String(error)}`);
  process.exit(1);
}

const expectedRange = String(projectPackage?.devDependencies?.electron ?? "");
const expectedMajorMatch = expectedRange.match(/(\d+)(?:\.\d+){0,2}/);
const expectedMajor = expectedMajorMatch ? Number(expectedMajorMatch[1]) : NaN;
const localVersion = String(localElectronPackage?.version ?? "");
const localMajor = parseMajor(localVersion);

if (Number.isFinite(expectedMajor) && Number.isFinite(localMajor) && localMajor !== expectedMajor) {
  console.error(
    `Local Electron version ${localVersion} does not match expected major ${expectedMajor} (range: ${expectedRange}).`
  );
  console.error(fixHint);
  process.exit(1);
}
