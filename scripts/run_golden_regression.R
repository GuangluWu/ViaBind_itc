#!/usr/bin/env Rscript

root <- "/Users/guanglu/Documents/myScript/ITCSuite"
core_dir <- file.path(root, "itcCore", "R")
for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

manifest <- read.csv(file.path(root, "tests", "golden", "manifest.csv"), stringsAsFactors = FALSE)
mode <- if (length(commandArgs(trailingOnly = TRUE)) > 0) commandArgs(trailingOnly = TRUE)[1] else "check"

max_ratio_err <- 0.02
max_h_err <- 200

failures <- character(0)
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  sample_path <- file.path(root, row$itc_path)
  expected_file <- file.path(root, "tests", "golden", "expected", paste0(row$id, "_integration.csv"))

  raw <- parse_itc(sample_path)
  processed <- process_itc(
    raw,
    baseline_cfg = list(
      baseline_duration = row$baseline_duration,
      baseline_offset = row$baseline_offset,
      spar = row$spar
    ),
    integration_cfg = list(
      limit_integration = as.logical(row$limit_integration),
      integration_window = row$integration_window,
      start_offset = row$start_offset,
      include_injection0 = FALSE
    )
  )

  actual <- processed$integration[, c("Injection", "Ratio_App", "heat_cal_mol")]

  if (mode == "bootstrap") {
    write.csv(actual, expected_file, row.names = FALSE)
    cat("Bootstrapped", row$id, "->", expected_file, "\n")
    next
  }

  if (!file.exists(expected_file)) {
    failures <- c(failures, paste(row$id, "missing expected file"))
    next
  }

  exp <- read.csv(expected_file, stringsAsFactors = FALSE)
  n <- min(nrow(exp), nrow(actual))
  if (n == 0) {
    failures <- c(failures, paste(row$id, "no comparable rows"))
    next
  }

  e_ratio <- abs(actual$Ratio_App[1:n] - exp$Ratio_App[1:n])
  e_h <- abs(actual$heat_cal_mol[1:n] - exp$heat_cal_mol[1:n])
  ratio_ref <- pmax(abs(exp$Ratio_App[1:n]), 1e-8)
  h_ref <- pmax(abs(exp$heat_cal_mol[1:n]), 1)

  ratio_ok <- all((e_ratio / ratio_ref) <= max_ratio_err, na.rm = TRUE)
  h_ok <- all((e_h <= max_h_err) | ((e_h / h_ref) <= max_ratio_err), na.rm = TRUE)

  if (!ratio_ok || !h_ok) {
    failures <- c(failures, sprintf("%s regression threshold exceeded", row$id))
  }
}

if (mode == "bootstrap") {
  cat("Bootstrap complete.\n")
  quit(status = 0)
}

if (length(failures) > 0) {
  cat("Golden regression failed:\n")
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  quit(status = 1)
}

cat("Golden regression passed.\n")
