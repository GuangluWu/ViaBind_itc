#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DESKTOP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
OUT_DIR=""
R_BIN="${R_BIN:-R}"
PROFILE="release"
STRICT_RUNTIME_MANIFEST=0
SYMBOLS_OUT=""
MANIFEST_PATH="${DESKTOP_DIR}/resources/r-runtime-manifest.txt"
DEFAULT_OUT_DIR="${DESKTOP_DIR}/resources/r-runtime"

FALLBACK_PKGS=(
  shiny plotly DT writexl ggplot2 patchwork readxl jsonlite
  rootSolve tidyverse DEoptim MASS gridExtra later digest colourpicker
)

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") [out_dir] [options]

Options:
  [out_dir]                    Optional runtime output dir (default: ${DEFAULT_OUT_DIR})
  --out-dir <dir>              Runtime output dir
  --profile <release|debug>    Build profile (default: release)
  --manifest <path>            Runtime package manifest path
  --strict-runtime-manifest    Fail if manifest package is missing
  --symbols-out <dir>          Archive dSYM files to this dir before pruning
  -h, --help                   Show this help
USAGE
}

if [[ $# -gt 0 && "${1}" != --* ]]; then
  OUT_DIR="$1"
  shift
fi

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out-dir)
      OUT_DIR="${2:-}"
      shift 2
      ;;
    --profile)
      PROFILE="${2:-}"
      shift 2
      ;;
    --manifest)
      MANIFEST_PATH="${2:-}"
      shift 2
      ;;
    --strict-runtime-manifest)
      STRICT_RUNTIME_MANIFEST=1
      shift
      ;;
    --symbols-out)
      SYMBOLS_OUT="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "[build-r-runtime] ERROR: unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ -z "${OUT_DIR}" ]]; then
  OUT_DIR="${DEFAULT_OUT_DIR}"
fi

if [[ "${PROFILE}" != "release" && "${PROFILE}" != "debug" ]]; then
  echo "[build-r-runtime] ERROR: --profile must be release or debug" >&2
  exit 1
fi

read_manifest_packages() {
  local manifest_path="$1"
  local line=""
  local trimmed=""
  local pkg_list=()
  if [[ ! -f "${manifest_path}" ]]; then
    return 1
  fi
  while IFS= read -r line || [[ -n "${line}" ]]; do
    trimmed="$(printf "%s" "${line}" | sed -E 's/[[:space:]]*#.*$//; s/^[[:space:]]+//; s/[[:space:]]+$//')"
    if [[ -n "${trimmed}" ]]; then
      pkg_list+=("${trimmed}")
    fi
  done < "${manifest_path}"
  printf "%s\n" "${pkg_list[@]}"
  return 0
}

size_kb() {
  local target="$1"
  du -sk "${target}" 2>/dev/null | awk '{print $1 + 0}'
}

echo "[build-r-runtime] desktop: ${DESKTOP_DIR}"
echo "[build-r-runtime] out: ${OUT_DIR}"
echo "[build-r-runtime] profile: ${PROFILE}"
echo "[build-r-runtime] manifest: ${MANIFEST_PATH}"
echo "[build-r-runtime] strict manifest: ${STRICT_RUNTIME_MANIFEST}"

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

REQUIRED_PKGS=()
if MANIFEST_VALUES="$(read_manifest_packages "${MANIFEST_PATH}")"; then
  while IFS= read -r manifest_pkg || [[ -n "${manifest_pkg}" ]]; do
    if [[ -n "${manifest_pkg}" ]]; then
      REQUIRED_PKGS+=("${manifest_pkg}")
    fi
  done <<< "${MANIFEST_VALUES}"
else
  if [[ "${STRICT_RUNTIME_MANIFEST}" -eq 1 ]]; then
    echo "[build-r-runtime] ERROR: manifest missing: ${MANIFEST_PATH}" >&2
    exit 1
  fi
  echo "[build-r-runtime] WARN: manifest missing, fallback package list will be used."
  REQUIRED_PKGS=("${FALLBACK_PKGS[@]}")
fi

if [[ "${#REQUIRED_PKGS[@]}" -eq 0 ]]; then
  echo "[build-r-runtime] ERROR: no packages provided by manifest/fallback list." >&2
  exit 1
fi

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

ITCSUITE_LIB_DIR="${LIB_DIR}" \
ITCSUITE_MANIFEST_PATH="${MANIFEST_PATH}" \
ITCSUITE_STRICT_RUNTIME_MANIFEST="${STRICT_RUNTIME_MANIFEST}" \
"${RSCRIPT_BIN}" --vanilla -e '
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
  cat(sprintf("Pruned %d non-manifest packages from runtime library.\n", length(drop)))
} else {
  cat("No non-manifest packages to prune.\n")
}
'

pre_prune_kb="$(size_kb "${OUT_DIR}")"

if [[ -n "${SYMBOLS_OUT}" ]]; then
  if find "${OUT_DIR}" -type d -name "*.dSYM" -print -quit | grep -q .; then
    mkdir -p "${SYMBOLS_OUT}"
    DSYM_LIST_FILE="$(mktemp "${TMPDIR:-/tmp}/itcsuite-dsym-list.XXXXXX")"
    find "${OUT_DIR}" -type d -name "*.dSYM" | sed "s#^${OUT_DIR}/##" > "${DSYM_LIST_FILE}"
    SYMBOLS_ARCHIVE="${SYMBOLS_OUT}/r-runtime-dsym-$(date +%Y%m%d-%H%M%S).tar.gz"
    tar -czf "${SYMBOLS_ARCHIVE}" -C "${OUT_DIR}" -T "${DSYM_LIST_FILE}"
    rm -f "${DSYM_LIST_FILE}"
    echo "[build-r-runtime] symbols archive: ${SYMBOLS_ARCHIVE}"
  else
    echo "[build-r-runtime] no .dSYM found; symbols archive skipped."
  fi
fi

if [[ "${PROFILE}" == "release" ]]; then
  find "${OUT_DIR}" -type d -name "*.dSYM" -prune -exec rm -rf {} +

  find "${OUT_DIR}" -type d \( \
    -name help -o -name html -o -name doc -o -name docs -o -name demo -o -name demos -o \
    -name examples -o -name tests -o -name testthat -o -name vignettes -o -name man \
  \) -prune -exec rm -rf {} +

  if [[ -d "${LIB_DIR}" ]]; then
    while IFS= read -r staged_dir || [[ -n "${staged_dir}" ]]; do
      staged_name="$(basename "${staged_dir}")"
      if [[ "${staged_name}" =~ ^file[0-9a-f]{6,}$ ]]; then
        rm -rf "${staged_dir}"
      fi
    done < <(find "${LIB_DIR}" -mindepth 1 -maxdepth 1 -type d -name "file*")
  fi
fi

post_prune_kb="$(size_kb "${OUT_DIR}")"
saved_kb=$(( pre_prune_kb - post_prune_kb ))
if (( saved_kb < 0 )); then
  saved_kb=0
fi

REPORT_PATH="${OUT_DIR}/runtime-size-report.txt"
{
  echo "profile=${PROFILE}"
  echo "manifest=${MANIFEST_PATH}"
  echo "strict_runtime_manifest=${STRICT_RUNTIME_MANIFEST}"
  echo "size_before_prune_mb=$(awk -v kb="${pre_prune_kb}" 'BEGIN { printf "%.1f", kb/1024 }')"
  echo "size_after_prune_mb=$(awk -v kb="${post_prune_kb}" 'BEGIN { printf "%.1f", kb/1024 }')"
  echo "size_saved_mb=$(awk -v kb="${saved_kb}" 'BEGIN { printf "%.1f", kb/1024 }')"
  echo
  echo "top_paths:"
  du -h -d 2 "${OUT_DIR}" | sort -hr | head -n 80
} > "${REPORT_PATH}"

echo "[build-r-runtime] size report: ${REPORT_PATH}"
echo "[build-r-runtime] done"
