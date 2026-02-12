repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "constants.R"), local = FALSE)
source(file.path(repo_root, "ITCsimfit", "R", "utils.R"), local = FALSE)
source(file.path(repo_root, "ITCsimfit", "R", "core_logic.R"), local = FALSE)
source(file.path(repo_root, "ITCsimfit", "R", "fitting.R"), local = FALSE)

if (!requireNamespace("rootSolve", quietly = TRUE)) {
  stop("Package `rootSolve` is required for fitting core regression tests.")
}

base_fit_params <- function() {
  list(
    V_cell = 0.2,
    V_inj = 0.0015,
    n_inj = 10,
    H_cell_0 = 30,
    G_syringe = 600,
    logK1 = 6,
    H1 = -5000,
    fH = 1.0,
    fG = 1.0,
    Offset = 0,
    V_init = 0,
    V_pre = 0
  )
}

testthat::test_that("calculate_simulation keeps Ratio_App and dQ_App mapping stable", {
  p <- base_fit_params()
  p$fH <- 0.8
  p$fG <- 1.2
  p$Offset <- 15

  res <- calculate_simulation(p, c("rxn_M"))
  testthat::expect_true(all(c("Ratio_App", "dQ_App", "Ratio", "dQ") %in% names(res)))
  testthat::expect_equal(res$Ratio_App, res$Ratio * (p$fH / p$fG), tolerance = 1e-10)
  testthat::expect_equal(res$dQ_App, (res$dQ * p$fG) + p$Offset, tolerance = 1e-10)
})

testthat::test_that("calculate_simulation default active_paths is stable", {
  p <- base_fit_params()
  res_null <- calculate_simulation(p, NULL)
  res_empty <- calculate_simulation(p, character(0))
  testthat::expect_equal(nrow(res_null), nrow(res_empty))
  testthat::expect_equal(res_null$dQ_App, res_empty$dQ_App, tolerance = 1e-10)
  testthat::expect_equal(res_null$Ratio_App, res_empty$Ratio_App, tolerance = 1e-10)
})

testthat::test_that("calculate_simulation validates invalid parameter payload", {
  testthat::expect_error(calculate_simulation(NULL, c("rxn_M")))
  testthat::expect_error(calculate_simulation("bad", c("rxn_M")))
})

testthat::test_that("calculate_simulation returns finite physically valid core outputs", {
  p <- base_fit_params()
  res <- calculate_simulation(p, c("rxn_M", "rxn_D"))
  testthat::expect_true(is.data.frame(res))
  testthat::expect_true(nrow(res) == p$n_inj)
  testthat::expect_true(all(is.finite(res$dQ_App)))
  testthat::expect_true(all(is.finite(res$Ratio_App)))
  testthat::expect_true(all(res$Ratio_App > 0))
})

