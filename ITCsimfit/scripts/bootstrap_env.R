#!/usr/bin/env Rscript

required_pkgs <- c(
  "shiny", "rootSolve", "tidyverse", "DT", "DEoptim", "readxl", "writexl",
  "MASS", "gridExtra", "digest", "lintr", "styler", "testthat"
)

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (basename(project_dir) == "scripts") {
  project_dir <- dirname(project_dir)
}
setwd(project_dir)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

lock_exists <- file.exists("renv.lock")
if (lock_exists) {
  cat("renv.lock found. Running renv::restore() ...\n")
  renv::restore(lockfile = "renv.lock", prompt = FALSE)
} else {
  cat("No renv.lock found. Installing declared dependencies ...\n")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  cat("Creating renv.lock via renv::snapshot() ...\n")
  renv::snapshot(lockfile = "renv.lock", prompt = FALSE, force = TRUE)
}

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages after bootstrap: ", paste(missing, collapse = ", "))
}

cat("Environment bootstrap finished.\n")
