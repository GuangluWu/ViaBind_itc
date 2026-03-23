repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"))

testthat::test_that("meta_rev overrides meta for shared numeric keys", {
  meta_df <- data.frame(
    parameter = c("H_cell_0_mM", "Temp_K"),
    value = c("20", "300"),
    stringsAsFactors = FALSE
  )
  meta_rev_df <- data.frame(
    parameter = c("H_cell_0_mM"),
    value = c("35"),
    stringsAsFactors = FALSE
  )
  merged <- extract_meta_numeric_map_with_rev_priority(meta_rev_df, meta_df)
  testthat::expect_equal(merged[["H_cell_0_mM"]], 35)
  testthat::expect_equal(merged[["Temp_K"]], 300)
})

testthat::test_that("meta fallback works when meta_rev missing", {
  meta_df <- data.frame(
    parameter = c("G_syringe_mM", "n_inj"),
    value = c("600", "18"),
    stringsAsFactors = FALSE
  )
  merged <- extract_meta_numeric_map_with_rev_priority(NULL, meta_df)
  testthat::expect_equal(merged[["G_syringe_mM"]], 600)
  testthat::expect_equal(merged[["n_inj"]], 18)
})

testthat::test_that("fit_params parser restores active paths and thermo params", {
  fit_params <- data.frame(
    parameter = c(
      "ActivePaths", "logK1", "H1_cal_mol", "logK7", "H7_cal_mol", "fH", "fG", "V_init_uL", "Offset_cal",
      "FitRangeStart_Inj", "FitRangeEnd_Inj"
    ),
    value = c("rxn_D, rxn_E,invalid,rxn_D", "7.1", "-6200", "4.8", "-1800", "0.9", "1.1", "0.35", "-12", "2", "18"),
    stringsAsFactors = FALSE
  )
  fp_map <- extract_fit_params_map(fit_params)
  restore <- extract_simfit_restore_params(fp_map)
  paths <- parse_active_paths_from_fit_params(fp_map)

  testthat::expect_equal(paths, c("rxn_D", "rxn_T", "rxn_E"))
  testthat::expect_equal(restore$logK1, 7.1)
  testthat::expect_equal(restore$H1, -6200)
  testthat::expect_equal(restore$logK7, 4.8)
  testthat::expect_equal(restore$H7, -1800)
  testthat::expect_equal(restore$fH, 0.9)
  testthat::expect_equal(restore$fG, 1.1)
  testthat::expect_equal(restore$V_init_uL, 0.35)
  testthat::expect_equal(restore$Offset_cal, -12)
  testthat::expect_equal(restore$FitRangeStart_Inj, 2)
  testthat::expect_equal(restore$FitRangeEnd_Inj, 18)
})

testthat::test_that("fit_bounds parser restores per-parameter bounds", {
  fit_bounds <- data.frame(
    param = c("logK1", "H1"),
    lower = c(0.5, -20000),
    upper = c(12, -5000),
    stringsAsFactors = FALSE
  )
  out <- extract_fit_bounds_map(fit_bounds)

  testthat::expect_equal(out$logK1$lower, 0.5)
  testthat::expect_equal(out$logK1$upper, 12)
  testthat::expect_equal(out$H1$lower, -20000)
  testthat::expect_equal(out$H1$upper, -5000)
})

testthat::test_that("snapshot fit_bounds parser groups rows by snapshot id", {
  snapshot_fit_bounds <- data.frame(
    `_snapshot_row_id` = c("snap_1", "snap_1", "snap_2"),
    param = c("logK1", "H1", "logK1"),
    lower = c(0.5, -20000, 1.5),
    upper = c(12, -5000, 9.5),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- extract_snapshot_fit_bounds_map(snapshot_fit_bounds)

  testthat::expect_equal(sort(names(out)), c("snap_1", "snap_2"))
  testthat::expect_equal(out$snap_1$H1$lower, -20000)
  testthat::expect_equal(out$snap_2$logK1$upper, 9.5)
})

testthat::test_that("missing ActivePaths falls back to base model", {
  fit_params <- data.frame(
    parameter = c("logK1", "H1_cal_mol"),
    value = c("6.5", "-5000"),
    stringsAsFactors = FALSE
  )
  fp_map <- extract_fit_params_map(fit_params)
  restore <- extract_simfit_restore_params(fp_map)
  paths <- parse_active_paths_from_fit_params(fp_map)
  testthat::expect_equal(paths, character(0))
  testthat::expect_true(is.na(restore$FitRangeStart_Inj))
  testthat::expect_true(is.na(restore$FitRangeEnd_Inj))
})

testthat::test_that("integration_rev is preferred over integration", {
  sheets <- list(
    integration = data.frame(Injection = 1, Ratio_App = 0.1, stringsAsFactors = FALSE),
    integration_rev = data.frame(Injection = 1, Ratio_App = 0.2, stringsAsFactors = FALSE)
  )
  preferred <- get_preferred_integration_sheet(sheets)
  testthat::expect_equal(preferred$Ratio_App[[1]], 0.2)
})

testthat::test_that("resolve_first_injection_targets step1 uses meta V_pre first", {
  meta_vals <- c(V_pre_uL = 0.42)
  int_df <- data.frame(V_titrate_uL = c(0.31, 2.0), stringsAsFactors = FALSE)

  out <- resolve_first_injection_targets(
    mode = "step1",
    meta_vals = meta_vals,
    int_df = int_df,
    default_v_pre = 0.3
  )

  testthat::expect_true(isTRUE(out$has_source))
  testthat::expect_equal(out$source_tag, "meta_v_pre")
  testthat::expect_equal(out$v_pre_target, 0.42)
  testthat::expect_equal(out$v_init_target, 0.42)
})

testthat::test_that("resolve_first_injection_targets import prefers fit_params for both targets", {
  meta_vals <- c(V_pre_uL = 0.41)
  fp_restore <- list(V_pre_uL = 0.36, V_init_uL = 0.35)
  int_df <- data.frame(V_titrate_uL = c(0.33, 2.0), stringsAsFactors = FALSE)

  out <- resolve_first_injection_targets(
    mode = "import",
    meta_vals = meta_vals,
    fp_restore = fp_restore,
    int_df = int_df,
    default_v_pre = 0.3
  )

  testthat::expect_true(isTRUE(out$has_source))
  testthat::expect_equal(out$source_tag, "fit_v_pre")
  testthat::expect_equal(out$source_tag_v_pre, "fit_v_pre")
  testthat::expect_equal(out$source_tag_v_init, "fit_v_init")
  testthat::expect_equal(out$v_pre_target, 0.36)
  testthat::expect_equal(out$v_init_target, 0.35)
})

testthat::test_that("resolve_first_injection_targets import V_init falls back to fit V_pre", {
  meta_vals <- numeric(0)
  fp_restore <- list(V_pre_uL = 0.37, V_init_uL = NA_real_)
  int_df <- data.frame(stringsAsFactors = FALSE)

  out <- resolve_first_injection_targets(
    mode = "import",
    meta_vals = meta_vals,
    fp_restore = fp_restore,
    int_df = int_df,
    default_v_pre = 0.3
  )

  testthat::expect_true(isTRUE(out$has_source))
  testthat::expect_equal(out$source_tag_v_pre, "fit_v_pre")
  testthat::expect_equal(out$source_tag_v_init, "fit_v_pre")
  testthat::expect_equal(out$v_pre_target, 0.37)
  testthat::expect_equal(out$v_init_target, 0.37)
})

testthat::test_that("resolve_first_injection_targets import falls back to meta then integration then default", {
  out_meta <- resolve_first_injection_targets(
    mode = "import",
    meta_vals = c(V_pre_uL = 0.44),
    fp_restore = NULL,
    int_df = data.frame(V_titrate_uL = c(0.31, 2.0), stringsAsFactors = FALSE),
    default_v_pre = 0.3
  )
  testthat::expect_equal(out_meta$v_pre_target, 0.44)
  testthat::expect_equal(out_meta$v_init_target, 0.44)

  out_int <- resolve_first_injection_targets(
    mode = "import",
    meta_vals = numeric(0),
    fp_restore = NULL,
    int_df = data.frame(V_titrate_uL = c(0.31, 2.0), stringsAsFactors = FALSE),
    default_v_pre = 0.3
  )
  testthat::expect_equal(out_int$v_pre_target, 0.31)
  testthat::expect_equal(out_int$v_init_target, 0.31)

  out_default <- resolve_first_injection_targets(
    mode = "import",
    meta_vals = numeric(0),
    fp_restore = NULL,
    int_df = data.frame(stringsAsFactors = FALSE),
    default_v_pre = 0.3
  )
  testthat::expect_equal(out_default$v_pre_target, 0.3)
  testthat::expect_equal(out_default$v_init_target, 0.3)
})

testthat::test_that("validate_fit_range_restore_request accepts legal range", {
  out <- validate_fit_range_restore_request(
    start = 2,
    end = 18,
    available_max = 20,
    saved_n_inj = 20
  )
  testthat::expect_true(isTRUE(out$ok))
  testthat::expect_equal(out$reason, "ok")
  testthat::expect_equal(out$start, 2)
  testthat::expect_equal(out$end, 18)
  testthat::expect_equal(out$available_max, 20)
})

testthat::test_that("validate_fit_range_restore_request rejects out-of-bounds and reversed range", {
  out_over <- validate_fit_range_restore_request(
    start = 2,
    end = 25,
    available_max = 20,
    saved_n_inj = 20
  )
  testthat::expect_false(isTRUE(out_over$ok))
  testthat::expect_equal(out_over$reason, "range_out_of_bounds")

  out_rev <- validate_fit_range_restore_request(
    start = 12,
    end = 5,
    available_max = 20,
    saved_n_inj = 20
  )
  testthat::expect_false(isTRUE(out_rev$ok))
  testthat::expect_equal(out_rev$reason, "range_out_of_bounds")
})

testthat::test_that("validate_fit_range_restore_request rejects n_inj semantic mismatch", {
  out <- validate_fit_range_restore_request(
    start = 2,
    end = 18,
    available_max = 20,
    saved_n_inj = 18
  )
  testthat::expect_false(isTRUE(out$ok))
  testthat::expect_equal(out$reason, "n_inj_mismatch")
})

testthat::test_that("extract_step2_import_diagnostics restores full diagnostics payload", {
  sheets <- list(
    error_analysis = data.frame(
      Parameter = c("logK1", "H1"),
      Value = c(6.5, -5000),
      SE = c(0.012, 120),
      stringsAsFactors = FALSE
    ),
    error_reliability = data.frame(
      metric = c("n_data", "n_params", "dof", "reliability_color", "note"),
      value = c("20", "5", "15", "#27ae60", "stable"),
      stringsAsFactors = FALSE
    ),
    residuals = data.frame(
      Inj = c(1, 2),
      Fitted = c(-100, -90),
      Residual = c(1.2, -0.8),
      stringsAsFactors = FALSE
    ),
    correlation_matrix = data.frame(
      Parameter = c("logK1", "H1"),
      logK1 = c(1, 0.22),
      H1 = c(0.22, 1),
      stringsAsFactors = FALSE
    ),
    report = data.frame(
      line = c(2, 1, 3),
      text = c("line-2", "line-1", "line-3"),
      stringsAsFactors = FALSE
    )
  )

  out <- extract_step2_import_diagnostics(sheets)

  testthat::expect_true(isTRUE(out$has_error_analysis))
  testthat::expect_equal(nrow(out$error_analysis), 2)
  testthat::expect_equal(out$error_analysis_info$n_data, 20)
  testthat::expect_equal(out$error_analysis_info$n_params, 5)
  testthat::expect_equal(out$error_analysis_info$dof, 15)
  testthat::expect_equal(out$error_analysis_info$reliability_color, "#27ae60")
  testthat::expect_equal(out$error_analysis_info$note, "stable")
  testthat::expect_equal(nrow(out$residuals_data), 2)
  testthat::expect_equal(nrow(out$correlation_matrix_df), 2)
  testthat::expect_equal(out$current_report, "line-1\nline-2\nline-3")
})

testthat::test_that("extract_step2_import_diagnostics report fallback uses first column", {
  sheets <- list(
    report = data.frame(
      content = c("alpha", "beta"),
      stringsAsFactors = FALSE
    )
  )

  out <- extract_step2_import_diagnostics(sheets)
  testthat::expect_equal(out$current_report, "alpha\nbeta")
  testthat::expect_false(isTRUE(out$has_error_analysis))
})

testthat::test_that("extract_step2_import_diagnostics keeps non-numeric reliability values", {
  sheets <- list(
    error_reliability = data.frame(
      metric = c("dof", "reliability_color", "remark"),
      value = c("9", "#f39c12", "moderate"),
      stringsAsFactors = FALSE
    )
  )

  out <- extract_step2_import_diagnostics(sheets)
  testthat::expect_equal(out$error_analysis_info$dof, 9)
  testthat::expect_equal(out$error_analysis_info$reliability_color, "#f39c12")
  testthat::expect_equal(out$error_analysis_info$remark, "moderate")
})

testthat::test_that("extract_step2_import_diagnostics handles missing or empty sheets", {
  out_missing <- extract_step2_import_diagnostics(list())
  testthat::expect_false(isTRUE(out_missing$has_error_analysis))
  testthat::expect_null(out_missing$error_analysis)
  testthat::expect_null(out_missing$error_analysis_info)
  testthat::expect_null(out_missing$residuals_data)
  testthat::expect_null(out_missing$correlation_matrix_df)
  testthat::expect_null(out_missing$current_report)

  out_empty <- extract_step2_import_diagnostics(list(
    error_analysis = data.frame(Parameter = character(0), SE = numeric(0), stringsAsFactors = FALSE),
    error_reliability = data.frame(metric = character(0), value = character(0), stringsAsFactors = FALSE),
    residuals = data.frame(stringsAsFactors = FALSE),
    correlation_matrix = data.frame(stringsAsFactors = FALSE),
    report = data.frame(text = character(0), stringsAsFactors = FALSE)
  ))
  testthat::expect_false(isTRUE(out_empty$has_error_analysis))
  testthat::expect_null(out_empty$error_analysis)
  testthat::expect_null(out_empty$error_analysis_info)
  testthat::expect_null(out_empty$residuals_data)
  testthat::expect_null(out_empty$correlation_matrix_df)
  testthat::expect_null(out_empty$current_report)
})
