# ITCSuiteWeb

Single-app Shiny workflow for ITC data:

1. Step 1: `.itc` parse + baseline + integration
2. Step 2: simulation fitting with progress + cancel signal
3. Step 3: publication plot + bundle export

## Start

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb
Rscript -e 'shiny::runApp(".")'
```

## Architecture Notes

- Core computation is sourced from `/Users/guanglu/Documents/myScript/ITCSuite/itcCore/R`
- Unified state object in app: `values$bundle` (`itc_bundle_v1`)
- Legacy export compatibility can be toggled in UI

## Testing & PR Gate

Run the full strict suite from repository root:

```bash
Rscript /Users/guanglu/Documents/myScript/ITCSuite/tests/run_all.R --strict
```

Required PR checks:

1. `unit`
2. `smoke`
3. `golden`

See `/Users/guanglu/Documents/myScript/ITCSuite/tests/README.md` for suite-level commands and golden regression details.
