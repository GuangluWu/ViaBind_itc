#!/usr/bin/env Rscript

root <- "/Users/guanglu/Documents/myScript/ITCSuite"
cat("Running smoke tests in", root, "\n")

scripts <- c(
  file.path(root, "itcCore", "tests", "test_pipeline_minimal.R"),
  file.path(root, "itcCore", "tests", "test_bundle_roundtrip.R"),
  file.path(root, "itcCore", "tests", "test_legacy_import.R")
)

failed <- 0
for (s in scripts) {
  cat("\n==>", s, "\n")
  status <- system2("Rscript", c(s))
  if (status != 0) {
    failed <- failed + 1
  }
}

cat("\n==> Golden regression\n")
golden_status <- system2("Rscript", c(file.path(root, "scripts", "run_golden_regression.R"), "check"))
if (golden_status != 0) {
  failed <- failed + 1
}

if (failed > 0) {
  stop(sprintf("Smoke tests failed: %d", failed))
}

cat("\nAll smoke tests passed.\n")
