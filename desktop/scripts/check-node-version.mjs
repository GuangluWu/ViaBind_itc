import process from "node:process";

const version = process.versions.node;
const major = Number(version.split(".")[0]);
const supportedMajors = new Set([20, 22, 24]);

if (!Number.isFinite(major) || !supportedMajors.has(major)) {
  console.error(
    `Unsupported Node.js version ${version}. Use Node 20.x/22.x/24.x LTS for CaloriPath desktop.`
  );
  process.exit(1);
}
