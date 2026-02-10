#!/usr/bin/env bash
set -euo pipefail

ROOT="/Users/guanglu/Documents/myScript/ITCSuite"
APP_DIR="$ROOT/ITCSuiteWeb"

cd "$APP_DIR"
exec Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=3838)'
