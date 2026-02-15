repo_root <- itcsuite_repo_root()

read_src <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

testthat::test_that("host app defines home tab and itcsuite_home interface", {
  src <- read_src(file.path(repo_root, "ITCSuiteWeb", "app.R"))
  testthat::expect_true(grepl("selected\\s*=\\s*\"home\"", src, perl = TRUE))
  testthat::expect_true(grepl("value\\s*=\\s*\"home\"", src, perl = TRUE))
  testthat::expect_true(grepl("session\\$userData\\$itcsuite_home\\s*<-\\s*list", src, perl = TRUE))
  testthat::expect_true(grepl("session\\$userData\\$itcsuite_desktop\\s*<-\\s*list", src, perl = TRUE))
  testthat::expect_true(grepl("itcsuite_desktop_open_file", src, perl = TRUE))
  testthat::expect_true(grepl("itcsuite_desktop_open_file_result", src, perl = TRUE))
  testthat::expect_true(grepl("register_restore_handler", src, perl = TRUE))
  testthat::expect_true(grepl("source\\(\"R/home_recent_store\\.R\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_load\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_save\\(", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"legacy_processor_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"legacy_simfit_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"legacy_graph_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
})

testthat::test_that("step1 registers restore handler and reports recent import", {
  src <- read_src(file.path(repo_root, "ITCprocessor", "app.R"))
  testthat::expect_true(grepl("home_register_restore\\(\"step1\"", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent_export\\(", src, perl = TRUE))
  testthat::expect_true(grepl("import_type\\s*=\\s*\"itc\"", src, perl = TRUE))
  testthat::expect_true(grepl("step1_import_input", src, perl = TRUE))
  testthat::expect_true(grepl("step1_import_summary_ui", src, perl = TRUE))
  testthat::expect_true(grepl("step1_desktop_pick_file", src, perl = TRUE))
})

testthat::test_that("step2 registers restore handler and reuses import state function", {
  src <- read_src(file.path(
    repo_root,
    "ITCsimfit",
    "R",
    "server",
    "body",
    "runtime_core",
    "01_bridge_state_inputs.R"
  ))
  testthat::expect_true(grepl("home_register_restore\\(\"step2\"", src, perl = TRUE))
  testthat::expect_true(grepl("apply_imported_xlsx_state\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("import_step2_xlsx\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent_export\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("desktop_pick_exp_file", src, perl = TRUE))

  ui_src <- read_src(file.path(
    repo_root,
    "ITCsimfit",
    "R",
    "server",
    "body",
    "ui_i18n",
    "02_ui_outputs_report.R"
  ))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"active_paths_checkbox\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", ui_src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"factor_G_input\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", ui_src, perl = TRUE))
})

testthat::test_that("step3 registers restore handler and reports import/export to home", {
  src <- read_src(file.path(repo_root, "ITCgraph", "server.R"))
  testthat::expect_true(grepl("home_register_restore\\(\"step3\"", src, perl = TRUE))
  testthat::expect_true(grepl("record_step3_recent_import", src, perl = TRUE))
  testthat::expect_true(grepl("record_step3_recent_export", src, perl = TRUE))
  testthat::expect_true(grepl("step3_import_input", src, perl = TRUE))
  testthat::expect_true(grepl("step3_desktop_pick_file", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_point_color_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_line_linetype_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_no_dim_range_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
})
