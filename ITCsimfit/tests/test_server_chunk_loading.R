# ==============================================================================
# test_server_chunk_loading.R - server 分块加载 smoke test
# ==============================================================================

cat("测试 server 分块加载...\n\n")

resolve_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    cwd,
    dirname(cwd),
    file.path(cwd, "..")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  for (d in candidates) {
    if (dir.exists(file.path(d, "ITCsimfit")) && dir.exists(file.path(d, "ITCSuiteWeb"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot locate repo root.")
}

assert_true <- function(condition, test_name = "") {
  if (isTRUE(condition)) {
    cat("✓", test_name, "\n")
    return(invisible(TRUE))
  }
  cat("✗", test_name, "\n")
  stop(test_name)
}

repo_root <- resolve_repo_root()
simfit_dir <- file.path(repo_root, "ITCsimfit")
web_dir <- file.path(repo_root, "ITCSuiteWeb")

chunk_files <- c(
  file.path(simfit_dir, "R/server/body/01_ui_i18n.R"),
  file.path(simfit_dir, "R/server/body/02_runtime_core.R"),
  file.path(simfit_dir, "R/server/body/03_snapshot_export.R"),
  file.path(simfit_dir, "R/server/body/ui_i18n/01_bridge_i18n_setup.R"),
  file.path(simfit_dir, "R/server/body/ui_i18n/02_ui_outputs_report.R"),
  file.path(simfit_dir, "R/server/body/runtime_core/01_bridge_state_inputs.R"),
  file.path(simfit_dir, "R/server/body/runtime_core/02_simulation_fitting.R"),
  file.path(simfit_dir, "R/server/body/runtime_core/03_plots_diagnostics.R"),
  file.path(simfit_dir, "R/server/body/snapshot_export/01_snapshot_management.R"),
  file.path(simfit_dir, "R/server/body/snapshot_export/02_export_bridge.R")
)

assert_true(all(file.exists(chunk_files)), "所有 server chunk 文件存在")

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

# 场景1：直接在 ITCsimfit 内加载
setwd(simfit_dir)
source("global.R")
source("server.R")

assert_true(exists("server", mode = "function", inherits = FALSE), "ITCsimfit server 函数可用")

shiny::testServer(server, {
  stopifnot(exists("resolve_bridge_channel", inherits = TRUE))
  stopifnot(exists("perform_fitting", inherits = TRUE))
  stopifnot(exists("build_fitting_report_text", inherits = TRUE))
  stopifnot(exists("build_fit_export_bundle", inherits = TRUE))
})
assert_true(TRUE, "ITCsimfit testServer 分块作用域可见")

# 场景2：作为 ITCSuiteWeb 子模块加载
setwd(web_dir)
source("app.R")

assert_true(exists("simfit_legacy", inherits = FALSE), "ITCSuiteWeb 已加载 simfit_legacy")
assert_true(is.function(simfit_legacy$server), "simfit_legacy$server 是函数")

shiny::testServer(simfit_legacy$server, {
  stopifnot(exists("resolve_bridge_channel", inherits = TRUE))
  stopifnot(exists("perform_fitting", inherits = TRUE))
  stopifnot(exists("build_fitting_report_text", inherits = TRUE))
  stopifnot(exists("build_fit_export_bundle", inherits = TRUE))
})
assert_true(TRUE, "ITCSuiteWeb 宿主下 server 分块作用域可见")

cat("\n✓ server 分块加载 smoke test 通过\n")
