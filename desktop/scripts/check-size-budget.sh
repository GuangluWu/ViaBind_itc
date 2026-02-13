#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DESKTOP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

APP_PATH="${DESKTOP_DIR}/dist/mac-arm64/ViaBind.app"
RUNTIME_MAX_MB=360
APP_MAX_MB=560

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") [options]

Options:
  --app <path>             Path to .app bundle
  --runtime-max-mb <int>   Runtime budget in MB (default: 360)
  --app-max-mb <int>       .app budget in MB (default: 560)
  -h, --help               Show this help
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --app)
      APP_PATH="${2:-}"
      shift 2
      ;;
    --runtime-max-mb)
      RUNTIME_MAX_MB="${2:-}"
      shift 2
      ;;
    --app-max-mb)
      APP_MAX_MB="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "[size-budget] ERROR: unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ ! -d "${APP_PATH}" ]]; then
  echo "[size-budget] ERROR: app path not found: ${APP_PATH}" >&2
  exit 1
fi

if ! [[ "${RUNTIME_MAX_MB}" =~ ^[0-9]+$ && "${APP_MAX_MB}" =~ ^[0-9]+$ ]]; then
  echo "[size-budget] ERROR: budgets must be non-negative integers." >&2
  exit 1
fi

CONTENTS_PATH="${APP_PATH}/Contents"
RESOURCES_PATH="${CONTENTS_PATH}/Resources"
RUNTIME_PATH="${RESOURCES_PATH}/r-runtime"
FRAMEWORKS_PATH="${CONTENTS_PATH}/Frameworks"
SUITE_PATH="${RESOURCES_PATH}/itcsuite"

if [[ ! -d "${RUNTIME_PATH}" ]]; then
  echo "[size-budget] ERROR: runtime path missing: ${RUNTIME_PATH}" >&2
  exit 1
fi

app_mb="$(du -sm "${APP_PATH}" | awk '{print $1 + 0}')"
runtime_mb="$(du -sm "${RUNTIME_PATH}" | awk '{print $1 + 0}')"
frameworks_mb=0
suite_mb=0

if [[ -d "${FRAMEWORKS_PATH}" ]]; then
  frameworks_mb="$(du -sm "${FRAMEWORKS_PATH}" | awk '{print $1 + 0}')"
fi

if [[ -d "${SUITE_PATH}" ]]; then
  suite_mb="$(du -sm "${SUITE_PATH}" | awk '{print $1 + 0}')"
fi

echo "[size-budget] app: ${APP_PATH}"
echo "[size-budget] ViaBind.app = ${app_mb}MB (budget ${APP_MAX_MB}MB)"
echo "[size-budget] r-runtime = ${runtime_mb}MB (budget ${RUNTIME_MAX_MB}MB)"
echo "[size-budget] Frameworks = ${frameworks_mb}MB"
echo "[size-budget] itcsuite = ${suite_mb}MB"

failed=0
if (( app_mb > APP_MAX_MB )); then
  echo "[size-budget] ERROR: app size exceeds budget by $((app_mb - APP_MAX_MB))MB" >&2
  failed=1
fi
if (( runtime_mb > RUNTIME_MAX_MB )); then
  echo "[size-budget] ERROR: runtime size exceeds budget by $((runtime_mb - RUNTIME_MAX_MB))MB" >&2
  failed=1
fi

if (( failed != 0 )); then
  echo "[size-budget] top paths under Contents/Resources:"
  du -h -d 2 "${RESOURCES_PATH}" | sort -hr | head -n 40
  exit 1
fi

echo "[size-budget] PASS"
