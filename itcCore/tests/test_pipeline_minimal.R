#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grep("^--file=", args_full)]
if (length(file_arg) == 0) {
  script_path <- normalizePath("itcCore/tests/test_pipeline_minimal.R", mustWork = FALSE)
} else {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = FALSE)

core_dir <- file.path(root, "itcCore", "R")
for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

sample_file <- file.path(root, "ITCprocessor", "ada2cb7c.itc")
if (!file.exists(sample_file)) stop("Sample .itc file not found")

raw <- parse_itc(sample_file)
stopifnot(nrow(raw$data) > 0)
stopifnot(length(raw$injections) > 0)

processed <- process_itc(raw,
  baseline_cfg = list(baseline_duration = 20, baseline_offset = 5, spar = 0.1),
  integration_cfg = list(limit_integration = TRUE, integration_window = 15, start_offset = 0)
)

stopifnot(length(processed$corrected_power) == nrow(raw$data))
stopifnot("heat_cal_mol" %in% names(processed$integration))

bundle <- create_bundle_from_processed(raw, processed, source_file = basename(sample_file))
stopifnot(validate_itc_bundle(bundle))

cat("test_pipeline_minimal: PASS\n")
