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

# Prefer repo-local library when available for child runners.
project_lib <- normalizePath(file.path(repo_root, ".r-lib"), winslash = "/", mustWork = FALSE)
if (dir.exists(project_lib)) {
  .libPaths(unique(c(project_lib, .libPaths())))
  Sys.setenv(R_LIBS_USER = project_lib)
}

cat(sprintf("Running all suites (strict=%s)\n", tolower(as.character(strict_mode))))

rscript_bin <- file.path(R.home("bin"), "Rscript")
runner_files <- c(
  unit = file.path(repo_root, "tests", "run_unit.R"),
  smoke = file.path(repo_root, "tests", "run_smoke.R"),
  golden = file.path(repo_root, "tests", "run_golden.R")
)

statuses <- c()
for (nm in names(runner_files)) {
  cmd_args <- c(runner_files[[nm]])
  if (isTRUE(strict_mode)) cmd_args <- c(cmd_args, "--strict")
  cat(sprintf("\n[RUNNER] %s\n", nm))
  status <- system2(rscript_bin, args = cmd_args, stdout = "", stderr = "")
  statuses[[nm]] <- status
}

cat("\n============================================================\n")
cat("Top-Level Summary\n")
cat("============================================================\n")
for (nm in names(statuses)) {
  s <- statuses[[nm]]
  cat(sprintf("- %s: exit %d\n", nm, as.integer(s)))
}
cat("============================================================\n")

ok <- all(as.integer(statuses) == 0L)
quit(save = "no", status = if (ok) 0 else 1)
