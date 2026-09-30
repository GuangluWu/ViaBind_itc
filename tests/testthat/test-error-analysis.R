repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "weighting.R"))
source(file.path(repo_root, "ITCsimfit", "R", "error_analysis.R"))

testthat::test_that("Jacobian NLS uncertainty matches the analytic intercept model", {
  y <- seq(-1, 1, length.out = 20)
  y <- y - mean(y)
  par_opt <- c(Offset = mean(y))
  residual_fun <- function(par) par[["Offset"]] - y

  out <- calculate_parameter_uncertainty(
    residual_fun,
    par_opt,
    lower_b = c(Offset = -1500),
    upper_b = c(Offset = 1500)
  )

  expected_se <- sqrt(sum((y - mean(y))^2) / (length(y) - 1) / length(y))
  testthat::expect_equal(out$SE, expected_se, tolerance = 1e-8)
  testthat::expect_equal(attr(out, "diagnostics")$method, "nls_jacobian")
  testthat::expect_equal(attr(out, "diagnostics")$status, "good")

  rss_fun <- function(par) sum(residual_fun(par)^2)
  compatible <- calculate_hessian_ci_robust(rss_fun, par_opt, length(y), rss_fun(par_opt))
  testthat::expect_equal(compatible$SE, expected_se, tolerance = 1e-5)
})

testthat::test_that("zero-valued optima use a numerically meaningful step", {
  y <- c(-2, -1, 0, 1, 2)
  residual_fun <- function(par) par[[1]] - y

  fd <- calculate_residual_jacobian(
    residual_fun,
    c(Offset = 0),
    lower_b = -1500,
    upper_b = 1500
  )
  out <- calculate_parameter_uncertainty(
    residual_fun,
    c(Offset = 0),
    lower_b = -1500,
    upper_b = 1500
  )

  testthat::expect_gt(fd$steps[[1]], 1e-6)
  testthat::expect_equal(as.numeric(fd$jacobian[, 1]), rep(1, length(y)), tolerance = 1e-8)
  testthat::expect_true(is.finite(out$SE) && out$SE > 0)
})

testthat::test_that("rank-deficient models return NA uncertainty instead of zero", {
  y <- seq(-1, 1, length.out = 12)
  residual_fun <- function(par) par[[1]] + par[[2]] - y

  out <- calculate_parameter_uncertainty(residual_fun, c(a = 0, b = 0))
  diagnostics <- attr(out, "diagnostics")

  testthat::expect_true(all(is.na(out$SE)))
  testthat::expect_equal(diagnostics$status, "invalid")
  testthat::expect_match(diagnostics$warning_codes, "rank_deficient")
})

testthat::test_that("boundary optima suppress symmetric confidence intervals", {
  y <- rep(-1, 10)
  residual_fun <- function(par) par[[1]] - y

  out <- calculate_parameter_uncertainty(
    residual_fun,
    c(nonnegative = 0),
    lower_b = c(nonnegative = 0),
    upper_b = c(nonnegative = 10)
  )
  diagnostics <- attr(out, "diagnostics")

  testthat::expect_true(is.finite(out$SE))
  testthat::expect_true(is.na(out$CI_Lower) && is.na(out$CI_Upper))
  testthat::expect_equal(diagnostics$status, "low")
  testthat::expect_match(diagnostics$warning_codes, "boundary_parameters")
})

testthat::test_that("Wald intervals crossing hard bounds are suppressed", {
  y <- c(-10, 10, -10, 10)
  residual_fun <- function(par) par[[1]] - y

  out <- calculate_parameter_uncertainty(
    residual_fun,
    c(bounded = 0.5),
    lower_b = c(bounded = 0),
    upper_b = c(bounded = 1)
  )
  diagnostics <- attr(out, "diagnostics")

  testthat::expect_true(is.finite(out$SE))
  testthat::expect_true(is.na(out$CI_Lower) && is.na(out$CI_Upper))
  testthat::expect_equal(diagnostics$status, "low")
  testthat::expect_match(diagnostics$warning_codes, "interval_crosses_bounds")
})

testthat::test_that("weighted and Huber fits use sandwich covariance", {
  x <- seq(-1, 1, length.out = 30)
  y <- 2 + 0.5 * x + sin(seq_along(x)) * 0.05
  residual_fun <- function(par) par[[1]] + par[[2]] * x - y
  par_opt <- stats::coef(stats::lm(y ~ x))
  names(par_opt) <- c("intercept", "slope")
  weights <- seq(0.5, 1.5, length.out = length(y))

  weighted <- calculate_parameter_uncertainty(
    residual_fun,
    par_opt,
    weights = weights
  )
  robust <- calculate_parameter_uncertainty(
    residual_fun,
    par_opt,
    weights = weights,
    use_huber = TRUE,
    huber_delta = 0.1
  )

  testthat::expect_equal(attr(weighted, "diagnostics")$method, "weighted_sandwich")
  testthat::expect_equal(attr(robust, "diagnostics")$method, "weighted_huber_sandwich")
  testthat::expect_true(all(is.finite(weighted$SE)))
  testthat::expect_true(all(is.finite(robust$SE)))
  testthat::expect_match(attr(robust, "diagnostics")$warning_codes, "sandwich_approximation")
})
