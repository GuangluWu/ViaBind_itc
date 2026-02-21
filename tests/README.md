# ITCSuite Test & Regression Guide

## One-Command Entry

Run from repository root:

```bash
Rscript tests/run_all.R --strict
```

This command runs all required suites:

- `unit`: unit tests and legacy regression scripts
- `smoke`: app launch + bridge smoke checks (includes Step2 -> Step3 tab revisit range persistence E2E via `shinytest2`)
- `golden`: deterministic integration regression against `tests/golden/expected/*.csv`

`run_unit` also executes several legacy scripts in optional mode for visibility.
Optional failures are reported in logs but do not fail the required PR gate.

## Individual Suite Commands

```bash
Rscript tests/run_unit.R --strict
Rscript tests/run_smoke.R --strict
Rscript tests/run_golden.R --strict
```

`run_smoke.R --strict` requires `shinytest2`.

## PR Gate (Required)

PR must pass all required suites:

1. `unit`
2. `smoke`
3. `golden`

If any suite fails, gate fails.

## Golden Regression Rules

- Manifest: `tests/golden/manifest.csv`
- Case runner: `tests/golden/run_golden.R`
- Expected outputs: `tests/golden/expected/*_integration.csv`
- Required comparison columns:
  - `Injection`
  - `Ratio_App`
  - `heat_cal_mol`
- Optional per-case tolerances in manifest:
  - `ratio_abs_tol`
  - `heat_abs_tol`

## Failure Output Contract

Failure output includes:

1. Suite/test file or case id
2. Mismatched field name
3. Actual value vs expected value
4. Tolerance used

This output is designed for direct local reproduction.
