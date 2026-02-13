#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DESKTOP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${DESKTOP_DIR}/.." && pwd)"

VERSION=""
SKIP_TESTS=0
SKIP_SMOKE=0
SKIP_DIST=0
SKIP_NPM_INSTALL=0
SKIP_RUNTIME_BUILD=0
SKIP_SIZE_CHECK=0
R_LIBS_USER_ARG=""
RUNTIME_PROFILE="release"
RUNTIME_SYMBOLS_OUT=""
STRICT_RUNTIME_MANIFEST=1
RUNTIME_OUT_DIR=""

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") --version <x.y.z> [options]

Options:
  --version <x.y.z>    Target desktop package version (required)
  --skip-tests         Skip R strict regression (tests/run_all.R --strict)
  --skip-smoke         Skip desktop smoke test (npm run smoke)
  --skip-dist          Skip packaging step (npm run dist)
  --skip-npm-install   Skip npm install in desktop/
  --skip-runtime-build Skip bundled runtime build step
  --skip-size-check    Skip size budget check after packaging
  --r-libs-user <dir>  Override R_LIBS_USER for test execution
  --runtime-profile <release|debug>
                       Runtime build profile (default: release)
  --runtime-symbols-out <dir>
                       Output dir for archived runtime dSYM symbols
  --strict-runtime-manifest <0|1>
                       Whether runtime build should fail on manifest mismatch (default: 1)
  --runtime-out-dir <dir>
                       Runtime output dir (default: desktop/resources/r-runtime)
  -h, --help           Show this help

Examples:
  $(basename "$0") --version 0.1.1
  $(basename "$0") --version 0.1.2 --skip-tests --runtime-profile release
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version)
      VERSION="${2:-}"
      shift 2
      ;;
    --skip-tests)
      SKIP_TESTS=1
      shift
      ;;
    --skip-smoke)
      SKIP_SMOKE=1
      shift
      ;;
    --skip-dist)
      SKIP_DIST=1
      shift
      ;;
    --skip-npm-install)
      SKIP_NPM_INSTALL=1
      shift
      ;;
    --skip-runtime-build)
      SKIP_RUNTIME_BUILD=1
      shift
      ;;
    --skip-size-check)
      SKIP_SIZE_CHECK=1
      shift
      ;;
    --r-libs-user)
      R_LIBS_USER_ARG="${2:-}"
      shift 2
      ;;
    --runtime-profile)
      RUNTIME_PROFILE="${2:-}"
      shift 2
      ;;
    --runtime-symbols-out)
      RUNTIME_SYMBOLS_OUT="${2:-}"
      shift 2
      ;;
    --strict-runtime-manifest)
      STRICT_RUNTIME_MANIFEST="${2:-}"
      shift 2
      ;;
    --runtime-out-dir)
      RUNTIME_OUT_DIR="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ -z "$VERSION" ]]; then
  echo "Missing required --version argument." >&2
  usage
  exit 1
fi

if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Invalid version format: $VERSION" >&2
  echo "Expected format like 0.1.1 or 1.2.3-beta.1" >&2
  exit 1
fi

if ! command -v npm >/dev/null 2>&1; then
  echo "npm not found in PATH." >&2
  exit 1
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found in PATH." >&2
  exit 1
fi

if [[ "${RUNTIME_PROFILE}" != "release" && "${RUNTIME_PROFILE}" != "debug" ]]; then
  echo "Invalid --runtime-profile value: ${RUNTIME_PROFILE}" >&2
  echo "Expected release or debug" >&2
  exit 1
fi

if [[ "${STRICT_RUNTIME_MANIFEST}" != "0" && "${STRICT_RUNTIME_MANIFEST}" != "1" ]]; then
  echo "Invalid --strict-runtime-manifest value: ${STRICT_RUNTIME_MANIFEST}" >&2
  echo "Expected 0 or 1" >&2
  exit 1
fi

if command -v git >/dev/null 2>&1; then
  if [[ -n "$(git -C "$REPO_ROOT" status --porcelain)" ]]; then
    echo "[release] Warning: git worktree has uncommitted changes."
  fi
fi

if [[ -z "$R_LIBS_USER_ARG" && -d "$REPO_ROOT/.r-lib" ]]; then
  R_LIBS_USER_ARG="$REPO_ROOT/.r-lib"
fi

echo "[release] repo: $REPO_ROOT"
echo "[release] desktop: $DESKTOP_DIR"
echo "[release] target version: $VERSION"
echo "[release] runtime profile: $RUNTIME_PROFILE"
echo "[release] strict runtime manifest: $STRICT_RUNTIME_MANIFEST"
if [[ -n "$R_LIBS_USER_ARG" ]]; then
  echo "[release] R_LIBS_USER: $R_LIBS_USER_ARG"
fi

if [[ -z "$RUNTIME_OUT_DIR" ]]; then
  RUNTIME_OUT_DIR="$DESKTOP_DIR/resources/r-runtime"
fi

cd "$DESKTOP_DIR"

if [[ "$SKIP_NPM_INSTALL" -eq 0 ]]; then
  echo "[release] npm install"
  npm install
else
  echo "[release] skip npm install"
fi

if [[ "$SKIP_TESTS" -eq 0 ]]; then
  echo "[release] run strict regression"
  if [[ -n "$R_LIBS_USER_ARG" ]]; then
    R_LIBS_USER="$R_LIBS_USER_ARG" Rscript "$REPO_ROOT/tests/run_all.R" --strict
  else
    Rscript "$REPO_ROOT/tests/run_all.R" --strict
  fi
else
  echo "[release] skip strict regression"
fi

if [[ "$SKIP_SMOKE" -eq 0 ]]; then
  echo "[release] run desktop smoke"
  npm run smoke
else
  echo "[release] skip desktop smoke"
fi

if [[ "$SKIP_RUNTIME_BUILD" -eq 0 ]]; then
  echo "[release] build bundled runtime"
  BUILD_RUNTIME_CMD=(
    bash "$DESKTOP_DIR/scripts/build-r-runtime.sh"
    "$RUNTIME_OUT_DIR"
    --profile "$RUNTIME_PROFILE"
    --manifest "$DESKTOP_DIR/resources/r-runtime-manifest.txt"
  )
  if [[ "$STRICT_RUNTIME_MANIFEST" -eq 1 ]]; then
    BUILD_RUNTIME_CMD+=(--strict-runtime-manifest)
  fi
  if [[ -n "$RUNTIME_SYMBOLS_OUT" ]]; then
    BUILD_RUNTIME_CMD+=(--symbols-out "$RUNTIME_SYMBOLS_OUT")
  fi
  "${BUILD_RUNTIME_CMD[@]}"
else
  echo "[release] skip bundled runtime build"
fi

CURRENT_VERSION="$(node -p "require('./package.json').version")"
if [[ "$CURRENT_VERSION" != "$VERSION" ]]; then
  echo "[release] bump version: $CURRENT_VERSION -> $VERSION"
  npm version --no-git-tag-version "$VERSION"
else
  echo "[release] version already $VERSION"
fi

if [[ "$SKIP_DIST" -eq 0 ]]; then
  if [[ ! -x "$DESKTOP_DIR/node_modules/.bin/electron-builder" ]]; then
    echo "[release] ERROR: electron-builder missing in desktop/node_modules." >&2
    echo "[release] Run 'npm install --include=dev' in $DESKTOP_DIR, or rerun without --skip-npm-install." >&2
    exit 1
  fi
  echo "[release] build dist artifacts"
  npm run dist
else
  echo "[release] skip dist build"
fi

if [[ "$SKIP_SIZE_CHECK" -eq 0 && "$SKIP_DIST" -eq 0 ]]; then
  echo "[release] run size budget check"
  bash "$DESKTOP_DIR/scripts/check-size-budget.sh"
else
  echo "[release] skip size budget check"
fi

if [[ -d "$DESKTOP_DIR/dist" ]]; then
  echo "[release] artifacts:"
  find "$DESKTOP_DIR/dist" -maxdepth 1 \( -name "*.dmg" -o -name "*.zip" \) -type f | sort
fi

echo "[release] done"
