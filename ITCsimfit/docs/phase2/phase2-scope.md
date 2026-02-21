# Phase 2 Scope Freeze (No Functional Change)

## Objective
Improve maintainability of ITCsimfit code while preserving behavior and outputs.

## In Scope
- Split oversized entrypoint (`server.R`) into thin entry + implementation module.
- Standardize module layout and naming conventions for new files.
- Introduce unified logging/error handling interfaces.
- Add reproducible environment setup (`renv.lock` + bootstrap/check scripts).
- Add lint/style rules and execution scripts.

## Out of Scope
- Any algorithm or UI behavior change.
- Parameter default change.
- Data contract/schema change with ITCprocessor/ITCgraph.

## Acceptance Criteria
- `server.R` is a thin loader and no longer contains business logic.
- Existing tests/scripts continue to run without behavior regression.
- `renv.lock` exists and environment can be restored by script.
- Lint/style commands are documented and runnable.

## Baseline Verification Commands
Run from `ITCsimfit`:

```bash
Rscript tests/test_core_logic.R
Rscript tests/test_fitting.R
Rscript tests/test_server_improvements.R
Rscript tests/test_server_chunk_loading.R
```
