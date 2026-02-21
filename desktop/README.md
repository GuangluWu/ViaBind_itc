# ViaBind Desktop (Electron)

Electron shell for ViaBind using a local Shiny backend.

## Prerequisites (dev/build)

- Node.js 24.x LTS (recommended). Node 20.x/22.x/24.x are supported. Node 25 is not supported.
- R 4.3+ on build machines (used to prepare bundled runtime). End users do not need local R.

## Local development

```bash
cd desktop
npm install
npm run dev
```

`npm run dev` now validates that local `node_modules` Electron is present before launch, and will fail fast instead of using a global Electron binary.

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

## Build bundled R runtime (cross-platform)

```bash
cd desktop
node scripts/build-r-runtime.mjs --strict-runtime-manifest
```

This generates `resources/r-runtime`.

`npm run dist` and `npm run dist:win` will auto-run runtime validation/build, so this manual step is optional.

For CI strict gate (validate only, no auto-build), run:

```bash
cd desktop
npm run ensure:r-runtime:check
```

## Package app (macOS)

```bash
cd desktop
npm run dist
```

`npm run dist` ensures `resources/r-runtime` is present and valid before packaging.
Place learning/sample files under the repo-root `Examples/` folder before release; packaging will bundle it into app resources as `itcsuite/Examples`.
Build outputs land in `dist/`.

## Package app (Windows x64, for CI runners)

```bash
cd desktop
npm run dist:win
```

`npm run dist:win` also ensures `resources/r-runtime` before packaging.

## Backend smoke test (headless, CI-friendly)

```bash
cd desktop
npm run smoke:backend
```

## Smoke test

```bash
cd desktop
npm run smoke
```
