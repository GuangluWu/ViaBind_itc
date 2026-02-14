repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"))

testthat::test_that("calc_heat_ucal_from_cal_mol converts correctly", {
  out <- calc_heat_ucal_from_cal_mol(
    heat_cal_mol = c(1000, -500),
    vinj_ul = c(2, 4),
    g_syringe = 5
  )
  testthat::expect_equal(out, c(10, -10))
})

testthat::test_that("calc_heat_ucal_from_cal_mol returns NA for invalid volume or concentration", {
  out <- calc_heat_ucal_from_cal_mol(
    heat_cal_mol = c(1000, 1000, 1000),
    vinj_ul = c(2, 0, -1),
    g_syringe = 5
  )
  testthat::expect_equal(is.finite(out), c(TRUE, FALSE, FALSE))

  out_bad_g <- calc_heat_ucal_from_cal_mol(
    heat_cal_mol = c(1000, 1000),
    vinj_ul = c(2, 2),
    g_syringe = 0
  )
  testthat::expect_true(all(is.na(out_bad_g)))
})

testthat::test_that("build_sim_to_exp_exp_df backfills Heat_ucal from current V_pre/V_inj/G", {
  sim_df <- data.frame(
    Inj = c(1, 2, 3),
    Ratio_App = c(0.1, 0.2, 0.3),
    dQ_App = c(2000, 1000, 500),
    stringsAsFactors = FALSE
  )
  exp_df <- build_sim_to_exp_exp_df(
    sim_df = sim_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 5
  )
  testthat::expect_equal(exp_df$V_inj_uL, c(1, 2, 2))
  testthat::expect_equal(exp_df$Heat_ucal, c(10, 10, 5))
  testthat::expect_equal(exp_df$Heat_Raw, sim_df$dQ_App)
})

testthat::test_that("sim->exp Heat_ucal keeps Heat_Raw stable for same params and dynamic for changed params", {
  sim_df <- data.frame(
    Inj = c(1, 2, 3),
    Ratio_App = c(0.1, 0.2, 0.3),
    dQ_App = c(2000, 1000, 500),
    stringsAsFactors = FALSE
  )
  exp_df <- build_sim_to_exp_exp_df(
    sim_df = sim_df,
    v_pre = 1,
    v_inj = 2,
    g_syringe = 5
  )

  same_heat <- calc_heat_cal_mol_from_ucal(
    heat_ucal = exp_df$Heat_ucal,
    vinj_ul = build_current_vinj_ul(n = nrow(exp_df), v_pre = 1, v_inj = 2),
    g_syringe = 5
  )
  g_changed <- calc_heat_cal_mol_from_ucal(
    heat_ucal = exp_df$Heat_ucal,
    vinj_ul = build_current_vinj_ul(n = nrow(exp_df), v_pre = 1, v_inj = 2),
    g_syringe = 10
  )
  vpre_changed <- calc_heat_cal_mol_from_ucal(
    heat_ucal = exp_df$Heat_ucal,
    vinj_ul = build_current_vinj_ul(n = nrow(exp_df), v_pre = 2, v_inj = 2),
    g_syringe = 5
  )
  vinj_changed <- calc_heat_cal_mol_from_ucal(
    heat_ucal = exp_df$Heat_ucal,
    vinj_ul = build_current_vinj_ul(n = nrow(exp_df), v_pre = 1, v_inj = 4),
    g_syringe = 5
  )

  testthat::expect_equal(same_heat, exp_df$Heat_Raw)
  testthat::expect_false(isTRUE(all.equal(g_changed, exp_df$Heat_Raw)))
  testthat::expect_false(isTRUE(all.equal(vpre_changed[1], exp_df$Heat_Raw[1])))
  testthat::expect_equal(vinj_changed[1], exp_df$Heat_Raw[1])
  testthat::expect_false(isTRUE(all.equal(vinj_changed[2:3], exp_df$Heat_Raw[2:3])))
})

testthat::test_that("sim->exp resolves V_pre target from V_init", {
  out <- resolve_sim_to_exp_vpre_target(0.55)
  testthat::expect_true(isTRUE(out$ok))
  testthat::expect_equal(out$target_v_pre, 0.55)
  testthat::expect_true(is.na(out$error_key))
})

testthat::test_that("sim->exp blocks when V_init is invalid", {
  out_null <- resolve_sim_to_exp_vpre_target(NULL)
  testthat::expect_false(isTRUE(out_null$ok))
  testthat::expect_equal(out_null$error_key, "sim_to_exp_invalid_v_init")

  out_bad <- resolve_sim_to_exp_vpre_target("abc")
  testthat::expect_false(isTRUE(out_bad$ok))
  testthat::expect_equal(out_bad$error_key, "sim_to_exp_invalid_v_init")
})
