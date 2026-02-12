#!/usr/bin/env Rscript

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (basename(project_dir) == "scripts") {
  project_dir <- dirname(project_dir)
}
setwd(project_dir)

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Package `renv` is required. Install it first.")
}

cat("Rebuilding renv.lock in:", project_dir, "\n")
renv::snapshot(lockfile = "renv.lock", prompt = FALSE, force = TRUE)
cat("Done.\n")
