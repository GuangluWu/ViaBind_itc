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

# Prefer repo-local library when available (e.g., .r-lib/testthat, .r-lib/shinytest2).
project_lib <- normalizePath(file.path(repo_root, ".r-lib"), winslash = "/", mustWork = FALSE)
if (dir.exists(project_lib)) {
  .libPaths(unique(c(project_lib, .libPaths())))
  Sys.setenv(R_LIBS_USER = project_lib)
}

Sys.setenv(ITCSUITE_REPO_ROOT = repo_root)
on.exit(Sys.unsetenv("ITCSUITE_REPO_ROOT"), add = TRUE)

cat(sprintf("Running smoke tests (strict=%s)\n", tolower(as.character(strict_mode))))

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Smoke runner requires package `testthat`.")
}

namespace_error_message <- function(pkg) {
  tryCatch({
    suppressPackageStartupMessages(loadNamespace(pkg))
    ""
  }, error = function(e) conditionMessage(e))
}

if (isTRUE(strict_mode) && !requireNamespace("shinytest2", quietly = TRUE)) {
  shinytest2_err <- namespace_error_message("shinytest2")
  processx_err <- namespace_error_message("processx")
  if (nzchar(processx_err) && grepl("Operation not permitted", processx_err, fixed = TRUE)) {
    stop(
      paste0(
        "Strict smoke mode requires package `shinytest2`, but dependency `processx` ",
        "cannot load in the current environment (Operation not permitted). ",
        "Try running outside a restricted sandbox, or run smoke without --strict."
      )
    )
  }
  if (nzchar(shinytest2_err)) {
    stop("Strict smoke mode requires package `shinytest2`. Load error: ", shinytest2_err)
  }
  stop("Strict smoke mode requires package `shinytest2`.")
}

Sys.setenv(ITCSUITE_STRICT_SMOKE = if (isTRUE(strict_mode)) "true" else "false")
on.exit(Sys.unsetenv("ITCSUITE_STRICT_SMOKE"), add = TRUE)
if (isTRUE(strict_mode)) {
  Sys.setenv(NOT_CRAN = "true")
  on.exit(Sys.unsetenv("NOT_CRAN"), add = TRUE)
}

results <- list()
results[["ITCSuiteWeb app smoke"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-app-smoke.R"),
  label = "ITCSuiteWeb/test-app-smoke.R"
)

print_runner_summary(results)
ok <- all(vapply(results, function(x) isTRUE(x$ok), logical(1)))
quit(save = "no", status = if (ok) 0 else 1)
