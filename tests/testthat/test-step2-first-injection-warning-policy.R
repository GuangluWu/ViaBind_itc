repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"))

testthat::test_that("init with equal V_pre and V_init does not warn", {
  out <- should_warn_v_pre_change(
    v_pre = 0.3,
    v_init = 0.3,
    is_programmatic = FALSE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("step1 bridge sync does not warn", {
  out <- should_warn_v_pre_change(
    v_pre = 0.42,
    v_init = 0.42,
    is_programmatic = TRUE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("step2 import keeps mismatch without warning because it is programmatic", {
  out <- should_warn_v_pre_change(
    v_pre = 0.36,
    v_init = 0.35,
    is_programmatic = TRUE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("remove expt implies no warning when values unchanged", {
  out <- should_warn_v_pre_change(
    v_pre = 0.31,
    v_init = 0.31,
    is_programmatic = FALSE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("sim->expt V_pre<-V_init update does not warn", {
  out <- should_warn_v_pre_change(
    v_pre = 0.28,
    v_init = 0.28,
    is_programmatic = TRUE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("snapshot restore mismatch does not warn because it is programmatic", {
  out <- should_warn_v_pre_change(
    v_pre = 0.40,
    v_init = 0.35,
    is_programmatic = TRUE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_false(isTRUE(out))
})

testthat::test_that("manual V_pre change warns only when V_pre and V_init differ", {
  out_diff <- should_warn_v_pre_change(
    v_pre = 0.45,
    v_init = 0.35,
    is_programmatic = FALSE,
    is_step1_sync_pending = FALSE
  )
  out_same <- should_warn_v_pre_change(
    v_pre = 0.35,
    v_init = 0.35,
    is_programmatic = FALSE,
    is_step1_sync_pending = FALSE
  )
  testthat::expect_true(isTRUE(out_diff))
  testthat::expect_false(isTRUE(out_same))
})
