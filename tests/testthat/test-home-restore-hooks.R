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
  testthat::expect_true(grepl("source\\(\"R/home_sleep_restore_store\\.R\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_load\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_save\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_sleep_restore_store_load\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_sleep_restore_store_save\\(", src, perl = TRUE))
  testthat::expect_true(grepl("existing_steps <- if \\(is\\.list\\(existing\\$steps\\)\\) existing\\$steps else list\\(\\)", src, perl = TRUE))
  testthat::expect_true(grepl("step_norm\\s*%in%\\s*c\\(\"step1\",\\s*\"step2\",\\s*\"step3\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("for \\(step in c\\(\"step1\", \"step2\", \"step3\"\\)\\)", src, perl = TRUE))
  testthat::expect_true(grepl("allow_existing_reuse <- identical\\(source_event_norm, \"autosave\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("allow_suspend_reuse <- identical\\(source_event_norm, \"suspend\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("has_step2\\s*=\\s*\"step2\" %in% names\\(steps\\)", src, perl = TRUE))
  testthat::expect_true(grepl("reused_step2\\s*=\\s*isTRUE\\(reused_existing\\$step2\\)", src, perl = TRUE))
  testthat::expect_true(grepl("reused_step3\\s*=\\s*isTRUE\\(reused_existing\\$step3\\)", src, perl = TRUE))
  testthat::expect_true(grepl("pending_from_existing <- isTRUE\\([\\s\\S]*identical\\(existing_source_event, \"suspend\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("pending_restore <- isTRUE\\(pending_from_existing\\)", src, perl = TRUE))
  testthat::expect_true(grepl("request_sleep_restore_snapshot\\(reason = \"periodic_autosave\", source_event = \"autosave\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("source_event_norm\\s*<-\\s*normalize_home_scalar_chr\\(state\\$source_event", src, perl = TRUE))
  testthat::expect_true(grepl("if \\(!identical\\(source_event_norm, \"suspend\"\\)\\) return\\(invisible\\(FALSE\\)\\)", src, perl = TRUE))
  testthat::expect_true(grepl("restored_step2\\s*=\\s*\"step2\" %in% applied_steps", src, perl = TRUE))
  testthat::expect_true(grepl("should_startup_consume_pending_sleep_restore\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("should_resume_consume_autosave_fallback\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("consume_resume_autosave_fallback\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("if \\(!isTRUE\\(restored\\) && identical\\(event\\$type, \"resume\"\\)\\)", src, perl = TRUE))
  testthat::expect_true(grepl("consume_resume_autosave_fallback\\(event_type\\s*=\\s*\"resume\",\\s*trigger\\s*=\\s*trigger\\)", src, perl = TRUE))
  testthat::expect_true(grepl("consume_pending_sleep_restore\\(event_type\\s*=\\s*\"resume\",\\s*trigger\\s*=\\s*\"startup_pending\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("paste0\\(ev\\$type, \"\\\\|\", format\\(ev\\$ts, scientific = FALSE, trim = TRUE\\)\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outcome\\s*=\\s*\"deferred_no_steps_applied\"", src, perl = TRUE))
  testthat::expect_true(grepl("consume_pending_sleep_restore\\([\\s\\S]*trigger\\s*=\\s*paste0\\(normalize_home_scalar_chr\\(trigger, default = \"power_event\"\\), \"_retry\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("shiny::isolate\\([\\s\\S]*request_sleep_restore_snapshot\\(reason = \"periodic_autosave\", source_event = \"autosave\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("function isDesktopRuntime\\(\\)", src, perl = TRUE))
  testthat::expect_true(grepl("\\$\\(document\\)\\.on\\('shiny:disconnected', function\\(\\) \\{\\s*if \\(isDesktopRuntime\\(\\)\\) return;", src, perl = TRUE))
  testthat::expect_true(grepl("session\\$userData\\$itcsuite_sleep_restore\\s*<-\\s*list", src, perl = TRUE))
  testthat::expect_true(grepl("observeEvent\\(input\\$itcsuite_power_event", src, perl = TRUE))
  testthat::expect_true(grepl("request_sleep_restore_snapshot\\(reason = trigger, source_event = \"suspend\"\\)\\s*\\n\\s*tryCatch\\(session\\$flush\\(\\)", src, perl = TRUE))
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
  testthat::expect_true(grepl("sleep_restore_register\\(", src, perl = TRUE))
  testthat::expect_true(grepl("collect_step1_sleep_restore_snapshot", src, perl = TRUE))
  testthat::expect_true(grepl("apply_step1_sleep_restore_snapshot", src, perl = TRUE))
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
  testthat::expect_true(grepl("session_sleep_restore\\s*<-\\s*tryCatch", src, perl = TRUE))
  testthat::expect_true(grepl("sleep_restore_register\\(", src, perl = TRUE))
  testthat::expect_true(grepl("collect_step2_sleep_restore_snapshot", src, perl = TRUE))
  testthat::expect_true(grepl("apply_step2_sleep_restore_snapshot", src, perl = TRUE))
  testthat::expect_true(grepl("pending_snapshot <- tryCatch\\(step2_sleep_restore_pending_snapshot\\(\\)", src, perl = TRUE))
  testthat::expect_true(grepl("step2_sleep_restore_pending_snapshot\\(snapshot_norm\\)", src, perl = TRUE))
  testthat::expect_true(grepl("required_ids <- c\\([\\s\\S]*\"active_paths\"[\\s\\S]*\"fit_params\"[\\s\\S]*\"heat_offset\"", src, perl = TRUE))
  testthat::expect_true(grepl("replay_step2_sleep_restore_ui_state\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("replay_step2_sleep_restore_ui_state\\(snapshot_norm,\\s*passes\\s*=\\s*8L\\)", src, perl = TRUE))
  testthat::expect_true(grepl("safe_output_options\\(\\s*\"dynamic_fit_params_ui\"\\s*,\\s*suspend_when_hidden\\s*=\\s*FALSE\\s*\\)", src, perl = TRUE))
  testthat::expect_true(grepl("observe\\(\\{[\\s\\S]*pending <- tryCatch\\(step2_sleep_restore_pending_snapshot\\(\\)", src, perl = TRUE))
  testthat::expect_true(grepl("consume_pending_step2_sleep_restore\\(trigger = \"pending_poll\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("infer_step2_data_source_kind\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("should_allow_step1_payload_sync\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("if \\(!isTRUE\\(should_allow_step1_payload_sync\\(\\)\\)\\) return\\(invisible\\(FALSE\\)\\)", src, perl = TRUE))
  testthat::expect_true(grepl("is_step1_payload_sync_pending\\s*<-\\s*function\\(payload = get_latest_step1_payload\\(\\)\\) \\{\\s*if \\(is.null\\(payload\\) \\|\\| !is.list\\(payload\\)\\) return\\(FALSE\\)\\s*if \\(!isTRUE\\(should_allow_step1_payload_sync\\(\\)\\)\\) return\\(FALSE\\)", src, perl = TRUE))

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
  testthat::expect_true(grepl("sleep_restore_register\\(", src, perl = TRUE))
  testthat::expect_true(grepl("collect_step3_sleep_restore_snapshot", src, perl = TRUE))
  testthat::expect_true(grepl("apply_step3_sleep_restore_snapshot", src, perl = TRUE))
  testthat::expect_true(grepl("step3_import_input", src, perl = TRUE))
  testthat::expect_true(grepl("step3_desktop_pick_file", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_point_color_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_line_linetype_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
  testthat::expect_true(grepl("outputOptions\\(output,\\s*\"bot_no_dim_range_ui\",\\s*suspendWhenHidden\\s*=\\s*FALSE\\)", src, perl = TRUE))
})
