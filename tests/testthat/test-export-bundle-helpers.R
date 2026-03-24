repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "export_bundle_helpers.R"))
source(file.path(repo_root, "ITCsimfit", "R", "species_dist_helpers.R"))

make_safe_input <- function(values) {
  function(name) {
    if (!name %in% names(values)) return(NA)
    values[[name]]
  }
}

testthat::test_that("build_fit_params_df keeps inactive path parameters as NA", {
  safe_inp <- make_safe_input(list(
    logK1 = 6,
    H1 = -5000,
    logK2 = 4,
    H2 = -2000,
    factor_H = 1,
    factor_G = 1,
    V_init_val = 0,
    heat_offset = 5,
    H_cell_0 = 30,
    G_syringe = 600,
    V_cell = 0.2,
    V_inj = 1.5,
    n_inj = 20,
    fit_data_range = c(2, 18),
    V_pre = 0.4,
    Temp = 298.15
  ))
  df <- export_bridge_build_fit_params_df(
    safe_inp = safe_inp,
    active_paths_save = c("rxn_D"),
    rss_info = list(rss = 123.456, method = "unweighted")
  )
  testthat::expect_true(all(c("parameter", "value") %in% names(df)))
  testthat::expect_equal(df$value[df$parameter == "logK3"], NA_character_)
  testthat::expect_equal(df$value[df$parameter == "logK2"], "4")
  testthat::expect_equal(df$value[df$parameter == "logK7"], NA_character_)
  testthat::expect_equal(df$value[df$parameter == "RSS_method"], "unweighted")
  testthat::expect_equal(df$value[df$parameter == "n_inj"], "20")
  testthat::expect_equal(df$value[df$parameter == "FitRangeStart_Inj"], "2")
  testthat::expect_equal(df$value[df$parameter == "FitRangeEnd_Inj"], "18")
})

testthat::test_that("build_fit_params_df normalizes dependency-backed active paths", {
  safe_inp <- make_safe_input(list(
    logK1 = 6,
    H1 = -5000,
    logK3 = 5,
    H3 = -2500,
    logK7 = 4,
    H7 = -1200,
    factor_H = 1,
    factor_G = 1,
    V_init_val = 0,
    heat_offset = 0,
    H_cell_0 = 30,
    G_syringe = 600,
    V_cell = 0.2,
    V_inj = 1.5,
    n_inj = 20,
    fit_data_range = c(2, 18),
    V_pre = 0.4,
    Temp = 298.15
  ))
  df <- export_bridge_build_fit_params_df(
    safe_inp = safe_inp,
    active_paths_save = c("rxn_E"),
    rss_info = list(rss = 10, method = "unweighted")
  )

  testthat::expect_equal(df$value[df$parameter == "logK3"], "5")
  testthat::expect_equal(df$value[df$parameter == "logK7"], "4")
  testthat::expect_equal(df$value[df$parameter == "ActivePaths"], "rxn_T,rxn_E")
})

testthat::test_that("build_integration_rev exports required columns", {
  exp_df <- data.frame(
    Inj = c(1, 2),
    Ratio_Raw = c(0.1, 0.2),
    Heat_Raw = c(-100, -80),
    V_inj_uL = c(2, 2),
    stringsAsFactors = FALSE
  )
  out <- export_bridge_build_integration_rev(exp_df)
  testthat::expect_equal(names(out), c("Injection", "Ratio_App", "heat_cal_mol", "V_titrate_uL"))
  testthat::expect_equal(out$Injection, c(1, 2))
})

testthat::test_that("build_integration_rev keeps optional Heat_ucal when present", {
  exp_df <- data.frame(
    Inj = c(1, 2),
    Ratio_Raw = c(0.1, 0.2),
    Heat_Raw = c(-100, -80),
    V_inj_uL = c(1, 2),
    Heat_ucal = c(-0.1, -0.16),
    stringsAsFactors = FALSE
  )
  out <- export_bridge_build_integration_rev(exp_df)
  testthat::expect_equal(
    names(out),
    c("Injection", "Ratio_App", "heat_cal_mol", "V_titrate_uL", "Heat_ucal")
  )
  testthat::expect_equal(out$V_titrate_uL, c(1, 2))
  testthat::expect_equal(out$Heat_ucal, c(-0.1, -0.16))
})

testthat::test_that("build_meta_rev updates existing keys and appends new keys", {
  meta_cached <- data.frame(
    parameter = c("Temp_K", "H_cell_0_mM"),
    value = c("300", "20"),
    stringsAsFactors = FALSE
  )
  meta_updates <- list(
    Temp_K = 298.15,
    H_cell_0_mM = 30,
    G_syringe_mM = 600
  )
  out <- export_bridge_build_meta_rev(meta_cached, meta_updates)
  testthat::expect_equal(out$value[out$parameter == "Temp_K"], "298.15")
  testthat::expect_equal(out$value[out$parameter == "H_cell_0_mM"], "30")
  testthat::expect_equal(out$value[out$parameter == "G_syringe_mM"], "600")
})

testthat::test_that("build_meta_rev keeps unit column stable when appending generated_by", {
  meta_cached <- data.frame(
    parameter = c("Temp_K", "H_cell_0_mM"),
    value = c("300", "20"),
    unit = c("K", "mM"),
    stringsAsFactors = FALSE
  )
  out <- export_bridge_build_meta_rev(
    meta_cached,
    list(
      generated_by = list(value = "ViaBind v9.9.9: ITCsimfit", unit = "")
    )
  )
  testthat::expect_equal(names(out), c("parameter", "value", "unit"))
  testthat::expect_equal(out$unit[out$parameter == "Temp_K"], "K")
  testthat::expect_equal(out$unit[out$parameter == "generated_by"], "")
  testthat::expect_equal(out$value[out$parameter == "generated_by"], "ViaBind v9.9.9: ITCsimfit")
})

testthat::test_that("build_params_export_sheets keeps snapshots before meta", {
  snapshots <- data.frame(logK1 = 6, H1_cal_mol = -5000, stringsAsFactors = FALSE)
  sheets <- export_bridge_build_params_export_sheets(
    export_df = snapshots,
    module_name = "ITCsimfit",
    version = "1.2.3"
  )
  testthat::expect_equal(names(sheets), c("snapshots", "meta"))
  testthat::expect_equal(sheets$snapshots, snapshots)
  testthat::expect_true(all(c("parameter", "value") %in% names(sheets$meta)))
  testthat::expect_equal(sheets$meta$parameter[1], "generated_by")
  testthat::expect_equal(sheets$meta$value[1], "ViaBind v1.2.3: ITCsimfit")
})

testthat::test_that("build_fit_bounds helpers emit workbook-friendly tables", {
  fit_bounds <- list(
    logK1 = c(lower = 0.5, upper = 12),
    H1 = list(lower = -20000, upper = -5000)
  )
  fit_bounds_df <- export_bridge_build_fit_bounds_df(fit_bounds)
  snapshot_bounds_df <- export_bridge_build_snapshot_fit_bounds_df(
    snapshot_fit_bounds_by_row_id = list(
      snap_1 = fit_bounds
    )
  )
  sheets <- export_bridge_build_params_export_sheets(
    export_df = data.frame(`_snapshot_row_id` = "snap_1", logK1 = 6, stringsAsFactors = FALSE),
    module_name = "ITCsimfit",
    version = "1.2.3",
    snapshot_fit_bounds_df = snapshot_bounds_df
  )

  testthat::expect_equal(names(fit_bounds_df), c("param", "lower", "upper"))
  testthat::expect_equal(fit_bounds_df$param, c("logK1", "H1"))
  testthat::expect_equal(names(snapshot_bounds_df), c("_snapshot_row_id", "param", "lower", "upper"))
  testthat::expect_true(all(snapshot_bounds_df$`_snapshot_row_id` == "snap_1"))
  testthat::expect_equal(names(sheets), c("snapshots", "snapshot_fit_bounds", "meta"))
})

testthat::test_that("order_sheets keeps expected order and report at end", {
  sheets <- list(
    fit_params = data.frame(a = 1),
    fit_bounds = data.frame(a = 6),
    custom_x = data.frame(a = 2),
    simulation = data.frame(a = 3),
    species_dist = data.frame(a = 7),
    report = data.frame(a = 4),
    meta_rev = data.frame(a = 5)
  )
  ordered <- export_bridge_order_sheets(sheets)
  testthat::expect_equal(names(ordered), c("meta_rev", "simulation", "species_dist", "fit_params", "fit_bounds", "custom_x", "report"))
})

testthat::test_that("build_species_dist_export_df keeps base-model workbook columns in English", {
  sim <- data.frame(
    Inj = c(1, 2),
    Ratio_App = c(0.1, 0.2),
    H_pct = c(0.9, 0.8),
    M_pct = c(0.1, 0.2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- build_species_dist_export_df(sim = sim, active_paths = character(0))

  testthat::expect_equal(names(out), c("Injection", "Simulated_GH_Ratio", "H", "H1G1"))
  testthat::expect_equal(out$H, c(0.9, 0.8))
  testthat::expect_equal(out$H1G1, c(0.1, 0.2))
})

testthat::test_that("build_species_dist_export_df expands dependency-backed species in fixed order", {
  sim <- data.frame(
    Inj = 1:2,
    Ratio_App = c(0.1, 0.2),
    H_pct = c(0.50, 0.40),
    M_pct = c(0.10, 0.10),
    D_pct = c(0.05, 0.06),
    T_pct = c(0.07, 0.08),
    E_pct = c(0.09, 0.10),
    B_pct = c(0.06, 0.07),
    F_pct = c(0.08, 0.09),
    U_pct = c(0.05, 0.10),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- build_species_dist_export_df(
    sim = sim,
    active_paths = c("rxn_E", "rxn_B", "rxn_F", "rxn_U")
  )

  testthat::expect_equal(
    names(out),
    c("Injection", "Simulated_GH_Ratio", "H", "H1G1", "H1G2", "H2G2", "H3G2", "H2G1", "H2G3", "H1G1(U)")
  )
})

testthat::test_that("build_species_dist_plot returns ggplot with current species selection", {
  sim <- data.frame(
    Inj = 1:3,
    Ratio_App = c(0.1, 0.2, 0.3),
    H_pct = c(0.70, 0.60, 0.50),
    M_pct = c(0.20, 0.20, 0.20),
    D_pct = c(0.10, 0.20, 0.30),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  plot_obj <- build_species_dist_plot(sim = sim, active_paths = c("rxn_D"), lang = "en")

  testthat::expect_s3_class(plot_obj, "ggplot")
  testthat::expect_equal(levels(plot_obj$data$Species), c("H", "H1G1", "H1G2"))
})

testthat::test_that("resolve_step2_plot_source returns stable source label", {
  bridge_case <- export_bridge_resolve_step2_plot_source(
    manual_exp_source = "step1_bridge",
    imported_xlsx_filename = "sample.xlsx",
    imported_xlsx_file_path = "bridge://step1/1"
  )
  testthat::expect_equal(bridge_case$source, "bridge")
  testthat::expect_equal(bridge_case$source_label, "sample.xlsx")

  file_case <- export_bridge_resolve_step2_plot_source(
    manual_exp_source = "",
    imported_xlsx_filename = "fit.xlsx",
    imported_xlsx_file_path = "/tmp/fit.xlsx"
  )
  testthat::expect_equal(file_case$source, "file")
  testthat::expect_equal(file_case$source_label, "fit.xlsx")
})

testthat::test_that("sanitize_base_name prefers cleaned base_name first", {
  out <- export_bridge_sanitize_base_name(
    base_name = "sample_processed_20260214_0900",
    fallback_name = "ignored.xlsx"
  )
  testthat::expect_equal(out, "sample")
})

testthat::test_that("sanitize_base_name falls back to filename then ITC", {
  from_file <- export_bridge_sanitize_base_name(
    base_name = "",
    fallback_name = "demo_fitted_20260214_0900.xlsx"
  )
  testthat::expect_equal(from_file, "demo")

  from_default <- export_bridge_sanitize_base_name(
    base_name = "",
    fallback_name = ""
  )
  testthat::expect_equal(from_default, "ITC")
})

testthat::test_that("build_params_snapshot_filename uses required format", {
  now <- as.POSIXct("2026-02-14 10:11:12", tz = "UTC")
  file_name <- export_bridge_build_params_snapshot_filename(
    base_name = "abc",
    fallback_name = "unused.xlsx",
    now = now
  )
  testthat::expect_equal(file_name, "abc_FitParams_20260214_101112.xlsx")
})

testthat::test_that("read_viabind_version prefers ITCSUITE_APP_VERSION", {
  old_env <- Sys.getenv("ITCSUITE_APP_VERSION", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ITCSUITE_APP_VERSION")
    } else {
      Sys.setenv(ITCSUITE_APP_VERSION = old_env)
    }
  }, add = TRUE)

  Sys.setenv(ITCSUITE_APP_VERSION = "8.8.8")
  testthat::expect_equal(
    export_bridge_read_viabind_version(default_version = "x.x.x", cwd = file.path(tempdir(), "missing")),
    "8.8.8"
  )
  testthat::expect_equal(
    export_bridge_build_version_signature(module_name = "ITCsimfit", default_version = "x.x.x"),
    "ViaBind v8.8.8: ITCsimfit"
  )
})
