# Lint / Style Rules

## Goals
- Keep syntax and style consistent for newly touched modules.
- Avoid mass-formatting legacy files in Phase 2.

## Active Lint Rules
Configured in `ITCsimfit/.lintr`.

- `line_length_linter(120)`
- `assignment_linter` (`<-` and `=` in named arguments)
- `object_usage_linter`
- `spaces_left_parentheses_linter`
- `trailing_whitespace_linter`

Legacy exclusion:
- `R/server/server_main.R` (kept as-is to avoid behavior risk in this phase)

## Style Formatting
Use `styler` via script:

```bash
Rscript scripts/run_style.R
```

## Lint Command

```bash
Rscript scripts/run_lint.R
```
