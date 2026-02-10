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
