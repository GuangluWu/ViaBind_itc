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

# Prefer repo-local library when available.
project_lib <- normalizePath(file.path(repo_root, ".r-lib"), winslash = "/", mustWork = FALSE)
if (dir.exists(project_lib)) {
  .libPaths(unique(c(project_lib, .libPaths())))
  Sys.setenv(R_LIBS_USER = project_lib)
}

cat(sprintf("Running golden regression (strict=%s)\n", tolower(as.character(strict_mode))))

rscript_bin <- file.path(R.home("bin"), "Rscript")
golden_script <- file.path(repo_root, "tests", "golden", "run_golden.R")
status <- system2(rscript_bin, args = c(golden_script), stdout = "", stderr = "")
result <- list(ok = as.integer(status) == 0L, detail = sprintf("exit %d", as.integer(status)))

results <- list("Golden regression" = result)
print_runner_summary(results)
quit(save = "no", status = if (isTRUE(result$ok)) 0 else 1)
