# ITCSuiteWeb

Single-app Shiny workflow for ITC data:

1. Step 1: `.itc` parse + baseline + integration
2. Step 2: simulation fitting with progress + cancel signal
3. Step 3: publication plot + bundle export

## Start

From repository root:

```bash
Rscript -e 'shiny::runApp("ITCSuiteWeb")'
```

Or from this directory:

```bash
Rscript -e 'shiny::runApp(".")'
```

## Desktop launch entrypoint

Electron uses:

```bash
Rscript ITCSuiteWeb/scripts/launch_shiny.R --repo-root . --app-dir ITCSuiteWeb --host 127.0.0.1 --port 0
```

- `--port 0` means auto-select an available local port.
- On success, script prints `ITCSUITE_READY {"port":...}` to stdout.
- On failure, script prints `ITCSUITE_ERROR {...}` and exits non-zero.

## Architecture Notes

- This app hosts legacy modules from sibling directories:
  - `ITCprocessor`
  - `ITCsimfit`
  - `ITCgraph`
- Unified state object in app: `values$bundle` (`itc_bundle_v1`)
- Legacy export compatibility can be toggled in UI

## Testing & PR Gate

Run the full strict suite from repository root:

```bash
Rscript tests/run_all.R --strict
```

Required PR checks:

1. `unit`
2. `smoke`
3. `golden`

See `tests/README.md` for suite-level commands and golden regression details.
