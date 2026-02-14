repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"))

testthat::test_that("build_current_vinj_ul rebuilds first and following injections", {
  vinj <- build_current_vinj_ul(n = 4, v_pre = 1.5, v_inj = 2.5)
  testthat::expect_equal(vinj, c(1.5, 2.5, 2.5, 2.5))
})

testthat::test_that("calc_heat_cal_mol_from_ucal handles invalid denominator", {
  out <- calc_heat_cal_mol_from_ucal(
    heat_ucal = c(10, 20),
    vinj_ul = c(1, 2),
    g_syringe = 0
  )
  testthat::expect_true(all(is.na(out)))
})

testthat::test_that("build_step2_exp_df_from_integration prefers Heat_ucal over heat_cal_mol", {
  int_df <- data.frame(
    Ratio_App = c(0.1, 0.2),
    Heat_ucal = c(10, 20),
    heat_cal_mol = c(999, 999),
    V_titrate_uL = c(8, 8),
    stringsAsFactors = FALSE
  )
  out <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 5
  )
  testthat::expect_equal(out$V_inj_uL, c(1, 2))
  testthat::expect_equal(out$Heat_Raw, c(2000, 2000))
  testthat::expect_equal(out$Heat_ucal, c(10, 20))
})

testthat::test_that("build_step2_exp_df_from_integration falls back to heat_cal_mol when Heat_ucal missing", {
  int_df <- data.frame(
    Ratio_App = c(0.1, 0.2),
    heat_cal_mol = c(-100, -80),
    V_titrate_uL = c(2, 2),
    stringsAsFactors = FALSE
  )
  out <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 5
  )
  testthat::expect_equal(out$Heat_Raw, c(-100, -80))
})

testthat::test_that("Heat_Raw changes with G_syringe, V_pre, and V_inj", {
  int_df <- data.frame(
    Ratio_App = c(0.1, 0.2, 0.3),
    Heat_ucal = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  base <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 5
  )
  g_changed <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 10
  )
  vpre_changed <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 2,
    v_inj = 2,
    g_syringe = 5
  )
  vinj_changed <- build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = 1,
    v_inj = 4,
    g_syringe = 5
  )

  testthat::expect_false(isTRUE(all.equal(base$Heat_Raw, g_changed$Heat_Raw)))
  testthat::expect_false(isTRUE(all.equal(base$Heat_Raw[1], vpre_changed$Heat_Raw[1])))
  testthat::expect_equal(base$Heat_Raw[1], vinj_changed$Heat_Raw[1])
  testthat::expect_false(isTRUE(all.equal(base$Heat_Raw[2:3], vinj_changed$Heat_Raw[2:3])))
})
