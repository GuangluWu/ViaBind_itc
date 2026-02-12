#!/usr/bin/env Rscript

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("Package `lintr` is required. Run scripts/bootstrap_env.R first.")
}

target_patterns <- c(
  "app.R",
  "global.R",
  "server.R",
  "R/utils.R",
  "R/infrastructure/*.R",
  "scripts/*.R"
)
targets <- unique(unlist(lapply(target_patterns, Sys.glob), use.names = FALSE))
targets <- targets[file.exists(targets)]

all_lints <- list()
for (path in targets) {
  res <- lintr::lint(path)
  if (length(res) > 0) {
    all_lints[[path]] <- res
  }
}

if (length(all_lints) > 0) {
  for (nm in names(all_lints)) {
    cat("\n==", nm, "==\n")
    print(all_lints[[nm]])
  }
  quit(save = "no", status = 1)
}

cat("Lint passed for targets:\n")
cat(paste0("- ", targets, collapse = "\n"), "\n")
