#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DESKTOP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
OUT_DIR="${1:-${DESKTOP_DIR}/resources/r-runtime}"
R_BIN="${R_BIN:-R}"

REQUIRED_PKGS=(
  shiny plotly DT writexl ggplot2 patchwork readxl jsonlite
  rootSolve tidyverse DEoptim MASS gridExtra later digest colourpicker
)

echo "[build-r-runtime] desktop: ${DESKTOP_DIR}"
echo "[build-r-runtime] out: ${OUT_DIR}"

if ! command -v "${R_BIN}" >/dev/null 2>&1; then
  echo "[build-r-runtime] ERROR: ${R_BIN} not found in PATH"
  exit 1
fi

R_HOME_DIR="$(${R_BIN} RHOME)"
if [[ ! -d "${R_HOME_DIR}" ]]; then
  echo "[build-r-runtime] ERROR: invalid RHOME: ${R_HOME_DIR}"
  exit 1
fi

echo "[build-r-runtime] R_HOME=${R_HOME_DIR}"

mkdir -p "${OUT_DIR}"

if command -v rsync >/dev/null 2>&1; then
  rsync -a --delete "${R_HOME_DIR}/" "${OUT_DIR}/"
else
  rm -rf "${OUT_DIR}"
  mkdir -p "${OUT_DIR}"
  cp -R "${R_HOME_DIR}/"* "${OUT_DIR}/"
fi

RSCRIPT_BIN="${OUT_DIR}/bin/Rscript"
if [[ ! -x "${RSCRIPT_BIN}" ]]; then
  echo "[build-r-runtime] ERROR: Rscript missing after copy: ${RSCRIPT_BIN}"
  exit 1
fi

LIB_DIR="${OUT_DIR}/library"
mkdir -p "${LIB_DIR}"

PKG_CSV="$(IFS=,; echo "${REQUIRED_PKGS[*]}")"

ITCSUITE_LIB_DIR="${LIB_DIR}" \
ITCSUITE_PKG_CSV="${PKG_CSV}" \
"${RSCRIPT_BIN}" --vanilla -e '
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
cat(sprintf("Installed/verified %d packages in %s\n", length(pkgs), lib_dir))
'

echo "[build-r-runtime] done"
