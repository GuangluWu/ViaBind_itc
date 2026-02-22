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

source(file.path(web_dir, "R", "home_desktop_helpers.R"), local = FALSE)

testthat::test_that("home_desktop_capability_open_file only accepts list capability", {
  testthat::expect_false(home_desktop_capability_open_file(NULL))
  testthat::expect_false(home_desktop_capability_open_file(list(open_file = FALSE)))
  testthat::expect_true(home_desktop_capability_open_file(list(open_file = TRUE)))
  testthat::expect_false(home_desktop_capability_open_file("true"))
})

testthat::test_that("home_desktop_capability_export_diagnostics reads list capability", {
  testthat::expect_false(home_desktop_capability_export_diagnostics(NULL))
  testthat::expect_false(home_desktop_capability_export_diagnostics(list(export_diagnostics = FALSE)))
  testthat::expect_true(home_desktop_capability_export_diagnostics(list(export_diagnostics = TRUE)))
  testthat::expect_false(home_desktop_capability_export_diagnostics("true"))
})

testthat::test_that("home_desktop_sanitize_filters keeps valid extensions and falls back", {
  filters <- home_desktop_sanitize_filters(
    list(
      list(name = "Excel", extensions = c(".xlsx", "XLSX", "bad-ext!")),
      list(name = "Text", extensions = c("txt", "txt"))
    ),
    fallback_name = "Fallback",
    fallback_exts = c("csv")
  )
  testthat::expect_equal(length(filters), 2L)
  testthat::expect_equal(filters[[1]]$extensions, "xlsx")
  testthat::expect_equal(filters[[2]]$extensions, "txt")

  fallback <- home_desktop_sanitize_filters(NULL, fallback_name = "Fallback", fallback_exts = c("csv"))
  testthat::expect_equal(fallback[[1]]$name, "Fallback")
  testthat::expect_equal(fallback[[1]]$extensions, "csv")
})

testthat::test_that("home_desktop_pending_register and take are one-shot", {
  pending <- new.env(parent = emptyenv())
  testthat::expect_true(home_desktop_pending_register(pending, "req_1", list(on_selected = function(...) NULL)))
  first_take <- home_desktop_pending_take(pending, "req_1")
  testthat::expect_true(is.list(first_take))
  testthat::expect_null(home_desktop_pending_take(pending, "req_1"))
})

testthat::test_that("home_desktop_normalize_open_file_result normalizes scalar fields", {
  out <- home_desktop_normalize_open_file_result(list(
    request_id = "  req_2  ",
    purpose = " step2_import ",
    canceled = FALSE,
    file_path = " /tmp/a.xlsx ",
    file_name = " a.xlsx ",
    error = NULL
  ))
  testthat::expect_equal(out$request_id, "req_2")
  testthat::expect_equal(out$purpose, "step2_import")
  testthat::expect_false(out$canceled)
  testthat::expect_equal(out$file_path, "/tmp/a.xlsx")
  testthat::expect_equal(out$file_name, "a.xlsx")
  testthat::expect_equal(out$error, "")
})

testthat::test_that("home_desktop_normalize_export_diagnostics_result normalizes fields", {
  out <- home_desktop_normalize_export_diagnostics_result(list(
    request_id = "  req_diag_1 ",
    ok = TRUE,
    file_path = " /tmp/diag.zip ",
    error = NULL
  ))
  testthat::expect_equal(out$request_id, "req_diag_1")
  testthat::expect_true(out$ok)
  testthat::expect_equal(out$file_path, "/tmp/diag.zip")
  testthat::expect_equal(out$error, "")
})

testthat::test_that("home_desktop_next_request_id increments sequence", {
  r1 <- home_desktop_next_request_id(0L, purpose = "step1_import")
  r2 <- home_desktop_next_request_id(r1$next_seq, purpose = "step1_import")
  testthat::expect_equal(r1$next_seq, 1L)
  testthat::expect_equal(r2$next_seq, 2L)
  testthat::expect_true(grepl("^desktop_step1_import_", r1$request_id))
})
