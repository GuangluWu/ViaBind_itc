# ITCsimfit Module Boundaries

## Layered Structure

1. `Entrypoint`:
- `ITCsimfit/app.R`
- `ITCsimfit/server.R`
- `ITCsimfit/ui.R`

2. `Infrastructure`:
- `ITCsimfit/R/infrastructure/logging.R`
- `ITCsimfit/R/infrastructure/errors.R`
- `ITCsimfit/R/utils.R`

3. `Domain / Simulation`:
- `ITCsimfit/R/core_logic.R`
- `ITCsimfit/R/fitting.R`
- `ITCsimfit/R/weighting.R`
- `ITCsimfit/R/error_analysis.R`
- `ITCsimfit/R/visualization.R`
- `ITCsimfit/R/bridge_step1_import.R`

4. `Server Runtime`:
- `ITCsimfit/R/server/server_main.R`
- `ITCsimfit/R/server/body/01_ui_i18n.R`
- `ITCsimfit/R/server/body/02_runtime_core.R`
- `ITCsimfit/R/server/body/03_snapshot_export.R`
- `ITCsimfit/R/server/body/ui_i18n/01_bridge_i18n_setup.R`
- `ITCsimfit/R/server/body/ui_i18n/02_ui_outputs_report.R`
- `ITCsimfit/R/server/body/runtime_core/01_bridge_state_inputs.R`
- `ITCsimfit/R/server/body/runtime_core/02_simulation_fitting.R`
- `ITCsimfit/R/server/body/runtime_core/03_plots_diagnostics.R`
- `ITCsimfit/R/server/body/snapshot_export/01_snapshot_management.R`
- `ITCsimfit/R/server/body/snapshot_export/02_export_bridge.R`

## Dependency Direction
Allowed only in one direction:

`Entrypoint -> Server Runtime -> Domain -> Infrastructure`

Disallowed:
- `Infrastructure -> Domain`
- `Domain -> Server Runtime`
- Cross-module mutable global state without explicit wrapper.

## Runtime Contracts

1. Error handling
- Public interface: `handle_error()`, `safe_execute()`.
- Standardized backend: `itc_error()`, `format_itc_error()`, `itc_try()`.

2. Logging
- Public interface: `log_info()`, `log_warning()`.
- Standardized backend: `itc_log()`, `itc_log_info()`, `itc_log_warn()`, `itc_log_error()`.

3. Server entry
- `server.R` only loads runtime implementation and validates exported `server` function.
- `R/server/server_main.R` orchestrates ordered loading of body chunks in one server environment.
- `R/server/body/01_ui_i18n.R` is a loader; implementation lives in `R/server/body/ui_i18n/*.R`.
- `R/server/body/02_runtime_core.R` is a loader; runtime implementation resides in `R/server/body/runtime_core/*.R`.
- `R/server/body/03_snapshot_export.R` is a loader; implementation lives in `R/server/body/snapshot_export/*.R`.

## Naming Conventions (New Files)
- File names: `snake_case.R`.
- Public helpers: clear verb/object naming (`build_*`, `resolve_*`, `validate_*`).
- Shared interfaces live under `R/infrastructure/`.
