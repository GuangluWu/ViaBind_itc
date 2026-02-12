#!/usr/bin/env Rscript

if (!requireNamespace("styler", quietly = TRUE)) {
  stop("Package `styler` is required. Run scripts/bootstrap_env.R first.")
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

if (length(targets) < 1) {
  cat("No files matched formatting targets.\n")
  quit(save = "no", status = 0)
}

for (f in targets) {
  styler::style_file(f, style = styler::tidyverse_style(indent_by = 2))
  cat("Styled:", f, "\n")
}
