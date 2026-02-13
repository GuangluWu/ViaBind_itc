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
    parameter = c("ActivePaths", "logK1", "H1_cal_mol", "fH", "fG", "V_init_uL", "Offset_cal"),
    value = c("rxn_D, rxn_T,invalid,rxn_D", "7.1", "-6200", "0.9", "1.1", "0.35", "-12"),
    stringsAsFactors = FALSE
  )
  fp_map <- extract_fit_params_map(fit_params)
  restore <- extract_simfit_restore_params(fp_map)
  paths <- parse_active_paths_from_fit_params(fp_map)

  testthat::expect_equal(paths, c("rxn_D", "rxn_T"))
  testthat::expect_equal(restore$logK1, 7.1)
  testthat::expect_equal(restore$H1, -6200)
  testthat::expect_equal(restore$fH, 0.9)
  testthat::expect_equal(restore$fG, 1.1)
  testthat::expect_equal(restore$V_init_uL, 0.35)
  testthat::expect_equal(restore$Offset_cal, -12)
})

testthat::test_that("missing ActivePaths falls back to base model", {
  fit_params <- data.frame(
    parameter = c("logK1", "H1_cal_mol"),
    value = c("6.5", "-5000"),
    stringsAsFactors = FALSE
  )
  paths <- parse_active_paths_from_fit_params(extract_fit_params_map(fit_params))
  testthat::expect_equal(paths, character(0))
})

testthat::test_that("integration_rev is preferred over integration", {
  sheets <- list(
    integration = data.frame(Injection = 1, Ratio_App = 0.1, stringsAsFactors = FALSE),
    integration_rev = data.frame(Injection = 1, Ratio_App = 0.2, stringsAsFactors = FALSE)
  )
  preferred <- get_preferred_integration_sheet(sheets)
  testthat::expect_equal(preferred$Ratio_App[[1]], 0.2)
})
