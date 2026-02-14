resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    p <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(p, "ITCSuiteWeb")) && dir.exists(file.path(p, "tests"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }

  cur <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:8) {
    if (dir.exists(file.path(cur, "ITCSuiteWeb")) && dir.exists(file.path(cur, "tests"))) {
      return(normalizePath(cur, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Cannot resolve repository root.")
}

repo_root <- resolve_repo_root()
web_dir <- file.path(repo_root, "ITCSuiteWeb")

source(file.path(web_dir, "R", "home_recent_helpers.R"), local = FALSE)

testthat::test_that("home_detect_import_type resolves by extension and naming rule", {
  testthat::expect_equal(home_detect_import_type("abc.itc"), "itc")
  testthat::expect_equal(home_detect_import_type("abc.txt"), "itc")
  testthat::expect_equal(home_detect_import_type("x_processed_20260214_0910.xlsx"), "processed_xlsx")
  testthat::expect_equal(home_detect_import_type("x_fitted_20260214_0910.xlsx"), "fitted_xlsx")
})

testthat::test_that("home_detect_import_type resolves by sheet content", {
  sheets_processed <- list(
    integration = data.frame(Ratio_App = 0.1, Heat_ucal = 10)
  )
  sheets_fitted <- list(
    simulation = data.frame(Ratio_App = 0.1, dQ_App = -8)
  )
  testthat::expect_equal(home_detect_import_type("unknown.xlsx", sheets = sheets_processed), "processed_xlsx")
  testthat::expect_equal(home_detect_import_type("unknown.xlsx", sheets = sheets_fitted), "fitted_xlsx")
})

testthat::test_that("home_target_step_from_import_type maps itc to step1 and others to step2", {
  testthat::expect_equal(home_target_step_from_import_type("itc"), "step1")
  testthat::expect_equal(home_target_step_from_import_type("processed_xlsx"), "step2")
  testthat::expect_equal(home_target_step_from_import_type("fitted_xlsx"), "step2")
})

testthat::test_that("home_trim_recent_records sorts desc and trims overflow", {
  records <- list(
    list(id = "r1", imported_at = "2026-02-14T10:00:00.000Z", restore_payload_key = "k1"),
    list(id = "r2", imported_at = "2026-02-14T10:00:03.000Z", restore_payload_key = "k2"),
    list(id = "r3", imported_at = "2026-02-14T10:00:01.000Z", restore_payload_key = "k3")
  )
  out <- home_trim_recent_records(records, max_records = 2L)
  testthat::expect_equal(length(out$records), 2L)
  testthat::expect_equal(out$records[[1]]$id, "r2")
  testthat::expect_equal(out$records[[2]]$id, "r3")
  testthat::expect_true("k1" %in% out$dropped_payload_keys)
})

testthat::test_that("home_parse_imported_at keeps hh:mm:ss for ISO timestamps with Z suffix", {
  ts_num <- home_parse_imported_at("2026-02-14T11:28:34.123Z")
  testthat::expect_true(is.finite(ts_num))
  rendered <- format(as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
  testthat::expect_identical(rendered, "11:28:34")
})
