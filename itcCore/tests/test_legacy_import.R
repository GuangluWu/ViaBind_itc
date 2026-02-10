#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grep("^--file=", args_full)]
if (length(file_arg) == 0) {
  script_path <- normalizePath("itcCore/tests/test_legacy_import.R", mustWork = FALSE)
} else {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = FALSE)

core_dir <- file.path(root, "itcCore", "R")
for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

legacy_file <- file.path(root, "ITCgraph", "TestData", "test_itc_data.xlsx")
if (!file.exists(legacy_file)) stop("Legacy xlsx not found")

bundle <- import_bundle(legacy_file)
stopifnot(validate_itc_bundle(bundle))
stopifnot(nrow(bundle$integration) > 0)
stopifnot(all(c("Ratio_App", "heat_cal_mol") %in% names(bundle$integration)))

cat("test_legacy_import: PASS\n")
