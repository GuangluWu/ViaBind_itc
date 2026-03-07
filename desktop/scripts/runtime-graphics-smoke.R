#!/usr/bin/env Rscript

parse_args <- function(args) {
  out <- list(repo_root = NULL, out_dir = NULL)
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    val <- if (i < length(args)) args[[i + 1L]] else ""
    if (key == "--repo-root") {
      out$repo_root <- val
    } else if (key == "--out-dir") {
      out$out_dir <- val
    } else {
      stop(sprintf("Unknown argument: %s", key), call. = FALSE)
    }
    i <- i + 2L
  }
  out
}

ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Required package missing: %s", pkg), call. = FALSE)
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (is.null(args$repo_root) || !nzchar(args$repo_root)) {
  stop("Missing --repo-root", call. = FALSE)
}
if (is.null(args$out_dir) || !nzchar(args$out_dir)) {
  stop("Missing --out-dir", call. = FALSE)
}

repo_root <- normalizePath(args$repo_root, winslash = "/", mustWork = TRUE)
out_dir <- normalizePath(args$out_dir, winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
runtime_root <- normalizePath(Sys.getenv("ITCSUITE_RUNTIME_ROOT", unset = ""), winslash = "/", mustWork = FALSE)
bundled_lib <- if (nzchar(runtime_root)) normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = FALSE) else ""

for (pkg in c("jsonlite", "shiny", "ggplot2", "patchwork", "ragg", "systemfonts", "textshaping")) {
  ensure_package(pkg)
}

if (nzchar(bundled_lib) && !bundled_lib %in% normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)) {
  stop(sprintf("Bundled runtime library missing from .libPaths(): %s", bundled_lib), call. = FALSE)
}

for (pkg in c("jsonlite", "shiny", "ggplot2", "patchwork", "ragg", "systemfonts", "textshaping")) {
  pkg_dir <- normalizePath(find.package(pkg), winslash = "/", mustWork = TRUE)
  if (nzchar(bundled_lib) && !startsWith(pkg_dir, bundled_lib)) {
    stop(sprintf("Package %s resolved outside bundled runtime: %s", pkg, pkg_dir), call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(shiny)
  library(ggplot2)
  library(patchwork)
  library(ragg)
  library(systemfonts)
  library(textshaping)
})

options(shiny.useragg = TRUE)

graph_dir <- file.path(repo_root, "ITCgraph")
if (!dir.exists(graph_dir)) {
  stop(sprintf("Missing ITCgraph dir: %s", graph_dir), call. = FALSE)
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(graph_dir)
source("global.R", local = FALSE)
source("R/plotting.R", local = FALSE)

power_data <- data.frame(
  Time_s = seq(0, 3600, length.out = 240),
  Power_corrected_ucal_s = -18 * exp(-seq(0, 5, length.out = 240)) + sin(seq(0, 10, length.out = 240))
)
integration_data <- data.frame(
  Ratio_App = seq(0.1, 2.0, length.out = 18),
  heat_cal_mol = seq(-12000, -3000, length.out = 18) + sin(seq(0, 3, length.out = 18)) * 450
)
simulation_data <- data.frame(
  Ratio_App = seq(0.1, 2.0, length.out = 180),
  dQ_App = approx(
    x = integration_data$Ratio_App,
    y = integration_data$heat_cal_mol + seq(-250, 250, length.out = 18),
    xout = seq(0.1, 2.0, length.out = 180)
  )$y
)

step3_figure <- create_itc_figure(
  power_data = power_data,
  integration_data = integration_data,
  simulation_data = simulation_data,
  params = modifyList(PLOT_DEFAULTS, list(export_width = 4, export_height = 6, export_dpi = 300))
)

step2_data <- data.frame(
  Fitted = seq(-12, -3, length.out = 24),
  Residual = c(seq(-0.9, 0.6, length.out = 12), seq(0.55, -0.4, length.out = 12)),
  Inj = seq_len(24)
)
step2_plot <- ggplot(step2_data, aes(x = Fitted, y = Residual)) +
  geom_point(color = "#2980b9", size = 2) +
  geom_hline(yintercept = 0, color = "#c0392b", linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#2c3e50", linewidth = 0.7) +
  labs(title = "Step2 Graphics Smoke", x = "Fitted heat", y = "Residual") +
  theme_minimal(base_size = 13)

preview_step2 <- file.path(out_dir, "step2-preview.png")
preview_step3 <- file.path(out_dir, "step3-preview.png")
export_png <- file.path(out_dir, "step3-export.png")
export_tiff <- file.path(out_dir, "step3-export.tiff")
export_pdf <- file.path(out_dir, "step3-export.pdf")

shiny:::plotPNG(function() print(step2_plot), filename = preview_step2, width = 960, height = 720, res = 144, bg = "white")
shiny:::plotPNG(function() print(step3_figure), filename = preview_step3, width = 960, height = 1440, res = 144, bg = "white")

ggplot2::ggsave(export_png, plot = step3_figure, device = "png", width = 4, height = 6, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave(export_tiff, plot = step3_figure, device = "tiff", width = 4, height = 6, units = "in", dpi = 300, compression = "lzw", bg = "white")
ggplot2::ggsave(export_pdf, plot = step3_figure, device = "pdf", width = 4, height = 6, units = "in", dpi = 300)

files <- c(preview_step2, preview_step3, export_png, export_tiff, export_pdf)
missing <- files[!file.exists(files)]
if (length(missing) > 0L) {
  stop(sprintf("Missing output files: %s", paste(basename(missing), collapse = ", ")), call. = FALSE)
}

sizes <- file.info(files)$size
if (any(!is.finite(sizes) | sizes <= 0)) {
  stop("Graphics smoke produced empty output files", call. = FALSE)
}

payload <- jsonlite::toJSON(
  list(out_dir = out_dir, files = basename(files), r_home = normalizePath(R.home(), winslash = "/", mustWork = FALSE), lib_paths = normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)),
  auto_unbox = TRUE
)
cat(sprintf("ITCSUITE_GRAPHICS_SMOKE %s\n", payload))
