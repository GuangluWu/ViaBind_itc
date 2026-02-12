#!/usr/bin/env Rscript

script_arg <- commandArgs()
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
source(file.path(dirname(script_dir), "lib", "runner_utils.R"))

setwd(normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE))
repo_root <- detect_repo_root()
setwd(repo_root)

source(file.path(repo_root, "ITCprocessor", "R", "data_parser.R"), local = FALSE)
source(file.path(repo_root, "ITCprocessor", "R", "baseline.R"), local = FALSE)
source(file.path(repo_root, "ITCprocessor", "R", "integration.R"), local = FALSE)

manifest_path <- file.path(repo_root, "tests", "golden", "manifest.csv")
if (!file.exists(manifest_path)) {
  stop("Missing manifest file: ", manifest_path)
}

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
required_cols <- c(
  "id", "itc_path", "baseline_duration", "baseline_offset", "spar",
  "limit_integration", "integration_window", "start_offset"
)
missing_cols <- setdiff(required_cols, names(manifest))
if (length(missing_cols) > 0) {
  stop("Manifest missing required columns: ", paste(missing_cols, collapse = ", "))
}

as_num <- function(x, default = NA_real_) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(v)) return(default)
  v
}

as_flag <- function(x, default = FALSE) {
  raw <- as.character(x)[1]
  if (is.na(raw) || !nzchar(raw)) return(default)
  raw <- tolower(trimws(raw))
  if (raw %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (raw %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
}

build_integration_case <- function(row_data) {
  itc_file <- file.path(repo_root, as.character(row_data$itc_path)[1])
  if (!file.exists(itc_file)) stop("Missing ITC file: ", itc_file)

  rd <- read_itc(itc_file)
  df <- rd$data
  injections <- rd$injections
  inj_times <- rd$injection_times

  baseline <- SegmentedBaseline(
    time = df$Time,
    power = df$Power,
    injection_indices = injections,
    injection_times = inj_times,
    baseline_duration = as_num(row_data$baseline_duration, default = 20),
    baseline_offset = as_num(row_data$baseline_offset, default = 5),
    spar = as_num(row_data$spar, default = 0.1)
  )

  corrected <- df$Power - baseline
  use_limit <- as_flag(row_data$limit_integration, default = TRUE)
  window_val <- if (isTRUE(use_limit)) as_num(row_data$integration_window, default = 15) else NULL

  int_res <- integrate_peaks(
    time = df$Time,
    corrected_power = corrected,
    injection_indices = injections,
    integration_window = window_val,
    start_offset = as_num(row_data$start_offset, default = 0)
  )

  int_res <- int_res[int_res$Injection > 0, , drop = FALSE]
  vol_ul <- rd$injection_volumes_ul
  if (is.null(vol_ul)) vol_ul <- rep(NA_real_, length(rd$injections))
  int_res$V_titrate_uL <- NA_real_
  for (r in seq_len(nrow(int_res))) {
    j <- int_res$Injection[r]
    if (j + 1L <= length(vol_ul)) int_res$V_titrate_uL[r] <- vol_ul[j + 1L]
  }

  syringe_mM <- rd$params$syringe_conc_mM
  denom <- int_res$V_titrate_uL * syringe_mM
  int_res$heat_cal_mol <- ifelse(
    !is.na(denom) & denom > 0,
    1000 * int_res$Heat_ucal / denom,
    NA_real_
  )

  V_cell <- rd$params$cell_volume_mL
  H_0 <- rd$params$cell_conc_mM
  G_0 <- rd$params$syringe_conc_mM

  V_titrate_mL <- int_res$V_titrate_uL / 1000
  H_app <- rep(NA_real_, nrow(int_res))
  G_app <- rep(NA_real_, nrow(int_res))
  if (!is.na(V_cell) && V_cell > 0 && !is.na(H_0) && !is.na(G_0)) {
    for (r in seq_len(nrow(int_res))) {
      v <- V_titrate_mL[r]
      if (is.na(v)) next
      if (r == 1L) {
        H_app[r] <- H_0 * (V_cell - v) / V_cell
        G_app[r] <- G_0 * v / V_cell
      } else {
        H_app[r] <- H_app[r - 1L] * (V_cell - v) / V_cell
        G_app[r] <- G_app[r - 1L] * (V_cell - v) / V_cell + G_0 * v / V_cell
      }
    }
  }
  int_res$Ratio_App <- ifelse(!is.na(H_app) & H_app != 0, G_app / H_app, NA_real_)

  int_res[, c("Injection", "Ratio_App", "heat_cal_mol"), drop = FALSE]
}

compare_case <- function(case_id, actual_df, expected_df, ratio_tol, heat_tol) {
  required <- c("Injection", "Ratio_App", "heat_cal_mol")
  missing_expected <- setdiff(required, names(expected_df))
  if (length(missing_expected) > 0) {
    stop("Expected CSV missing columns: ", paste(missing_expected, collapse = ", "))
  }

  actual <- actual_df[, required, drop = FALSE]
  expected <- expected_df[, required, drop = FALSE]
  actual <- actual[order(actual$Injection), , drop = FALSE]
  expected <- expected[order(expected$Injection), , drop = FALSE]

  if (nrow(actual) != nrow(expected)) {
    stop(sprintf("Row count mismatch for %s: actual=%d expected=%d", case_id, nrow(actual), nrow(expected)))
  }

  if (!identical(as.integer(actual$Injection), as.integer(expected$Injection))) {
    stop(sprintf("Injection sequence mismatch for %s", case_id))
  }

  ratio_diff <- abs(as.numeric(actual$Ratio_App) - as.numeric(expected$Ratio_App))
  heat_diff <- abs(as.numeric(actual$heat_cal_mol) - as.numeric(expected$heat_cal_mol))

  bad_ratio <- which(!is.finite(ratio_diff) | ratio_diff > ratio_tol)
  bad_heat <- which(!is.finite(heat_diff) | heat_diff > heat_tol)
  if (length(bad_ratio) == 0 && length(bad_heat) == 0) {
    return(list(ok = TRUE, message = "matched"))
  }

  msg_lines <- c(sprintf("Case %s mismatch:", case_id))
  if (length(bad_ratio) > 0) {
    i <- bad_ratio[1]
    msg_lines <- c(
      msg_lines,
      sprintf(
        "  Ratio_App diff at Injection=%d: actual=%0.12g expected=%0.12g diff=%0.12g tol=%0.12g",
        actual$Injection[i], actual$Ratio_App[i], expected$Ratio_App[i], ratio_diff[i], ratio_tol
      )
    )
  }
  if (length(bad_heat) > 0) {
    i <- bad_heat[1]
    msg_lines <- c(
      msg_lines,
      sprintf(
        "  heat_cal_mol diff at Injection=%d: actual=%0.12g expected=%0.12g diff=%0.12g tol=%0.12g",
        actual$Injection[i], actual$heat_cal_mol[i], expected$heat_cal_mol[i], heat_diff[i], heat_tol
      )
    )
  }

  list(ok = FALSE, message = paste(msg_lines, collapse = "\n"))
}

tests_passed <- 0L
tests_failed <- 0L

default_ratio_tol <- 1e-9
default_heat_tol <- 1e-6

cat("Golden regression started\n")
for (i in seq_len(nrow(manifest))) {
  row_data <- manifest[i, , drop = FALSE]
  case_id <- as.character(row_data$id)[1]
  expected_path <- file.path(repo_root, "tests", "golden", "expected", paste0(case_id, "_integration.csv"))
  cat(sprintf("\n[CASE] %s\n", case_id))

  if (!file.exists(expected_path)) {
    cat("  FAIL: missing expected file:", expected_path, "\n")
    tests_failed <- tests_failed + 1L
    next
  }

  ratio_tol <- if ("ratio_abs_tol" %in% names(row_data)) as_num(row_data$ratio_abs_tol, default_ratio_tol) else default_ratio_tol
  heat_tol <- if ("heat_abs_tol" %in% names(row_data)) as_num(row_data$heat_abs_tol, default_heat_tol) else default_heat_tol

  result <- tryCatch({
    actual_df <- build_integration_case(row_data)
    expected_df <- read.csv(expected_path, stringsAsFactors = FALSE)
    compare_case(case_id, actual_df, expected_df, ratio_tol = ratio_tol, heat_tol = heat_tol)
  }, error = function(e) {
    list(ok = FALSE, message = conditionMessage(e))
  })

  if (isTRUE(result$ok)) {
    tests_passed <- tests_passed + 1L
    cat("  PASS\n")
  } else {
    tests_failed <- tests_failed + 1L
    cat("  FAIL\n")
    cat(paste0("  ", gsub("\n", "\n  ", result$message)), "\n")
  }
}

cat("\n============================================================\n")
cat("Golden Summary\n")
cat("============================================================\n")
cat(sprintf("Passed: %d\n", tests_passed))
cat(sprintf("Failed: %d\n", tests_failed))
cat(sprintf("Total : %d\n", tests_passed + tests_failed))
cat("============================================================\n")

invisible(list(
  passed = tests_passed,
  failed = tests_failed,
  total = tests_passed + tests_failed
))
