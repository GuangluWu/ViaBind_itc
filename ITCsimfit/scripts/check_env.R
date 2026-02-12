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

cat("Project:", project_dir, "\n")
cat("renv.lock:", if (file.exists("renv.lock")) "present" else "missing", "\n\n")

missing <- character(0)
for (pkg in required_pkgs) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  if (!installed) {
    missing <- c(missing, pkg)
    cat(sprintf("[MISSING] %s\n", pkg))
  } else {
    version <- as.character(utils::packageVersion(pkg))
    cat(sprintf("[OK] %s (%s)\n", pkg, version))
  }
}

if (length(missing) > 0) {
  cat("\nMissing packages:", paste(missing, collapse = ", "), "\n")
  quit(save = "no", status = 1)
}

cat("\nEnvironment check passed.\n")
