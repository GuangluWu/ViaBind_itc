#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grep("^--file=", args_full)]
if (length(file_arg) == 0) {
  script_path <- normalizePath("itcCore/tests/test_bundle_roundtrip.R", mustWork = FALSE)
} else {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = FALSE)

core_dir <- file.path(root, "itcCore", "R")
for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

sample_file <- file.path(root, "ITCprocessor", "ada2cb7c.itc")
raw <- parse_itc(sample_file)
processed <- process_itc(raw,
  baseline_cfg = list(baseline_duration = 20, baseline_offset = 5, spar = 0.1),
  integration_cfg = list(limit_integration = TRUE, integration_window = 15, start_offset = 0)
)
bundle <- create_bundle_from_processed(raw, processed, source_file = basename(sample_file))

out <- tempfile(fileext = ".xlsx")
export_bundle(bundle, out, compat_legacy = TRUE)
back <- import_bundle(out)
sheets <- readxl::excel_sheets(out)

stopifnot(validate_itc_bundle(back))
stopifnot(nrow(back$integration) == nrow(bundle$integration))
stopifnot(all(c("meta", "power_original", "power_corrected", "integration", "fit_params", "simulation", "audit") %in% names(back)))
stopifnot(nrow(back$power_original) == nrow(bundle$power_original))
stopifnot(length(sheets) > 0 && identical(sheets[1], "power_original"))

cat("test_bundle_roundtrip: PASS\n")
