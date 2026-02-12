#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_arg <- commandArgs()
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "lib", "runner_utils.R"))
strict_mode <- parse_strict_flag(args)

setwd(normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE))
repo_root <- detect_repo_root()
setwd(repo_root)
Sys.setenv(ITCSUITE_REPO_ROOT = repo_root)
on.exit(Sys.unsetenv("ITCSUITE_REPO_ROOT"), add = TRUE)

cat(sprintf("Running unit tests (strict=%s)\n", tolower(as.character(strict_mode))))

results <- list()
required_keys <- character(0)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Unit runner requires package `testthat`.")
}

results[["ITCSuiteWeb bridge contract"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-bridge-contract.R"),
  label = "ITCSuiteWeb/test-bridge-contract.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb bridge contract")

root_testthat_dir <- file.path(repo_root, "tests", "testthat")
if (dir.exists(root_testthat_dir)) {
  results[["Root testthat suite"]] <- run_testthat_dir(
    root_testthat_dir,
    label = "tests/testthat"
  )
  required_keys <- c(required_keys, "Root testthat suite")
}

required_legacy_scripts <- c(
  "tests/test_core_logic.R",
  "tests/test_fitting.R",
  "tests/test_server_improvements.R"
)

optional_legacy_scripts <- c(
  "tests/test_constants_utils.R",
  "tests/test_core_logic_improvements.R",
  "tests/test_error_i18n.R",
  "tests/test_performance.R"
)

for (rel in required_legacy_scripts) {
  key <- paste0("ITCsimfit/", rel)
  results[[key]] <- run_legacy_script(
    script_path = file.path(repo_root, "ITCsimfit", rel),
    working_dir = file.path(repo_root, "ITCsimfit"),
    label = key
  )
  required_keys <- c(required_keys, key)
}

for (rel in optional_legacy_scripts) {
  key <- paste0("ITCsimfit/", rel, " (optional)")
  results[[key]] <- run_legacy_script(
    script_path = file.path(repo_root, "ITCsimfit", rel),
    working_dir = file.path(repo_root, "ITCsimfit"),
    label = key
  )
}

print_runner_summary(results)

ok <- all(vapply(required_keys, function(k) isTRUE(results[[k]]$ok), logical(1)))
quit(save = "no", status = if (ok) 0 else 1)
