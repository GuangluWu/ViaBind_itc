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
R_LIBS_USER_ARG=""

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
  --r-libs-user <dir>  Override R_LIBS_USER for test execution
  -h, --help           Show this help

Examples:
  $(basename "$0") --version 0.1.1
  $(basename "$0") --version 0.1.2 --skip-tests
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
    --r-libs-user)
      R_LIBS_USER_ARG="${2:-}"
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
if [[ -n "$R_LIBS_USER_ARG" ]]; then
  echo "[release] R_LIBS_USER: $R_LIBS_USER_ARG"
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

CURRENT_VERSION="$(node -p "require('./package.json').version")"
if [[ "$CURRENT_VERSION" != "$VERSION" ]]; then
  echo "[release] bump version: $CURRENT_VERSION -> $VERSION"
  npm version --no-git-tag-version "$VERSION"
else
  echo "[release] version already $VERSION"
fi

if [[ "$SKIP_DIST" -eq 0 ]]; then
  echo "[release] build dist artifacts"
  npm run dist
else
  echo "[release] skip dist build"
fi

if [[ -d "$DESKTOP_DIR/dist" ]]; then
  echo "[release] artifacts:"
  find "$DESKTOP_DIR/dist" -maxdepth 1 \( -name "*.dmg" -o -name "*.zip" \) -type f | sort
fi

echo "[release] done"
