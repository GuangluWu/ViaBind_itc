# ViaBind Desktop (Electron)

Electron shell for ViaBind using a local Shiny backend.

## Prerequisites (dev)

- Node.js 24.x LTS (recommended). Node 20.x/22.x/24.x are supported. Node 25 is not supported.
- R 4.3+ with required packages (or bundled runtime for packaged builds)

## Local development

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/desktop
npm install
npm run dev
```

If your default node is not a supported LTS version, use explicit binaries:

```bash
/usr/local/bin/node -v
/usr/local/bin/npm install
/usr/local/bin/npm run smoke
```

Environment overrides:

- `ITCSUITE_RSCRIPT`: absolute path to `Rscript` for dev startup.
- `ITCSUITE_REPO_ROOT`: override repo root in dev mode.
- `ITCSUITE_USE_BUNDLED_R=1`: force dev mode to use `resources/r-runtime`.

## Build bundled R runtime (macOS)

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/desktop
./scripts/build-r-runtime.sh
```

This generates `resources/r-runtime`.

## Package app (macOS)

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/desktop
npm run dist
```

Build outputs land in `dist/`.

## Smoke test

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/desktop
npm run smoke
```
