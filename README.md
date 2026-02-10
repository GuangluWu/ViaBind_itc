# ITCSuite Monorepo

This repository now contains a unified Web workflow and a shared core layer:

- `/Users/guanglu/Documents/myScript/ITCSuite/itcCore`: shared core API and bundle adapters
- `/Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb`: single Shiny app with 3-step workflow
- `/Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor`: legacy processor module
- `/Users/guanglu/Documents/myScript/ITCSuite/ITCsimfit`: legacy fitter module
- `/Users/guanglu/Documents/myScript/ITCSuite/ITCgraph`: legacy graph module

## New Unified API (`itcCore`)

- `parse_itc(path)`
- `process_itc(raw_itc, baseline_cfg, integration_cfg)`
- `fit_itc(processed_itc, fit_cfg, progress_cb, is_cancelled)`
- `build_plot_data(processed_itc, fit_result, plot_cfg)`
- `export_bundle(bundle, path, format = "xlsx")`
- `import_bundle(path)`

Schema: `itc_bundle_v1`

Required sheets/fields:

- `meta`
- `power_corrected`
- `integration`
- `fit_params`
- `simulation`
- `audit`

Compatibility adapters are included for legacy `integration_rev` / `meta_rev` inputs.

## Run Unified App

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb
Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=3838)'
```

## Logs and Metrics

Runtime logs are written as JSONL:

- `/Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb/logs/itcsuiteweb.jsonl`

In-app metrics tab shows import/fit durations and error rate.

## Testing

Local smoke + regression scripts:

- `/Users/guanglu/Documents/myScript/ITCSuite/scripts/run_smoke_tests.R`
- `/Users/guanglu/Documents/myScript/ITCSuite/itcCore/tests/test_bundle_roundtrip.R`
- `/Users/guanglu/Documents/myScript/ITCSuite/itcCore/tests/test_pipeline_minimal.R`

Run all:

```bash
Rscript /Users/guanglu/Documents/myScript/ITCSuite/scripts/run_smoke_tests.R
```
