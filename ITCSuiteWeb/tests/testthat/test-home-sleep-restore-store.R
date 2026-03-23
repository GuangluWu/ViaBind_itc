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

with_temp_sleep_restore_store <- function(expr) {
  expr_sub <- substitute(expr)
  tmp_root <- tempfile("itcsuite-sleep-restore-")
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

  has_old <- "ITCSUITE_USER_DATA_DIR" %in% names(Sys.getenv())
  old_val <- Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = "")
  on.exit({
    if (isTRUE(has_old)) {
      Sys.setenv(ITCSUITE_USER_DATA_DIR = old_val)
    } else {
      Sys.unsetenv("ITCSUITE_USER_DATA_DIR")
    }
    unlink(tmp_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  Sys.setenv(ITCSUITE_USER_DATA_DIR = tmp_root)
  eval(expr_sub, envir = parent.frame())
}

repo_root <- resolve_repo_root()
web_dir <- file.path(repo_root, "ITCSuiteWeb")
source(file.path(web_dir, "R", "home_sleep_restore_store.R"), local = FALSE)

testthat::test_that("sleep restore store save/load round trip works", {
  with_temp_sleep_restore_store({
    state <- list(
      schema_version = home_sleep_restore_store_schema(),
      pending_restore = TRUE,
      saved_at = "2026-02-23T10:00:00Z",
      source_event = "suspend",
      lang = "zh",
      active_tab = "step3",
      steps = list(
        step1 = list(
          source_path = "/tmp/a.itc",
          display_name = "a.itc",
          params = list(duration = 20, offset = 5, zoom_baseline = TRUE)
        ),
        step2 = list(
          source_path = "/tmp/step2.xlsx",
          file_name = "step2.xlsx",
          source_kind = "import",
          exp_data_disabled = FALSE,
          params = list(
            active_paths = c("rxn_D", "rxn_T"),
            fit_params = c("logK1", "H1", "logK2", "H2"),
            fit_data_range = c(2, 15),
            path_view_mode = "graph",
            enable_error_analysis = TRUE
          ),
          fit_bounds = list(
            logK1 = c(lower = 0.5, upper = 12.0),
            H1 = list(lower = -50000, upper = 50000)
          ),
          snapshot_table = list(
            rows = data.frame(Name = c("snapA", "snapB"), RSS = c("1e-4", "2e-4"), stringsAsFactors = FALSE),
            checked_ids = c("snap_1", "snap_2"),
            active_row_id = "snap_2",
            row_seq = 12,
            fit_bounds_by_row_id = list(
              snap_1 = list(logK1 = c(lower = 0.5, upper = 12)),
              snap_2 = list(H1 = list(lower = -20000, upper = -5000))
            )
          ),
          diagnostics = list(
            error_analysis = data.frame(Parameter = c("logK1", "H1"), SE = c(0.2, 100), stringsAsFactors = FALSE),
            residuals_data = matrix(c(1, 2, 3, 4), nrow = 2),
            correlation_matrix = list(A = c(1, 0.1), B = c(0.1, 1)),
            residual_subtab = "res3",
            current_report = "REPORT"
          ),
          manual_exp_data = c(0.1, 0.2, 0.3),
          was_fitting = TRUE
        ),
        step3 = list(
          source_path = "/tmp/b.xlsx",
          file_name = "b.xlsx",
          settings = list(top_xlab = "time", export_dpi = 300),
          sheets = list(
            integration = data.frame(Ratio_App = c(0.1, 0.2), heat_cal_mol = c(-1.2, -1.1))
          )
        )
      ),
      restored_at = ""
    )
    ok <- home_sleep_restore_store_save(state)
    testthat::expect_true(isTRUE(ok))
    testthat::expect_true(file.exists(home_sleep_restore_store_path()))

    loaded <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    testthat::expect_equal(loaded$schema_version, home_sleep_restore_store_schema())
    testthat::expect_true(isTRUE(loaded$pending_restore))
    testthat::expect_equal(loaded$steps$step2$snapshot_table$fit_bounds_by_row_id$snap_1$logK1$lower, 0.5)
    testthat::expect_equal(loaded$steps$step2$snapshot_table$fit_bounds_by_row_id$snap_2$H1$upper, -5000)
    testthat::expect_equal(loaded$lang, "zh")
    testthat::expect_equal(loaded$active_tab, "step3")
    testthat::expect_equal(loaded$steps$step1$display_name, "a.itc")
    testthat::expect_equal(loaded$steps$step1$params$duration, 20)
    testthat::expect_equal(loaded$steps$step2$source_kind, "import")
    testthat::expect_equal(loaded$steps$step2$params$path_view_mode, "graph")
    testthat::expect_equal(length(loaded$steps$step2$params$active_paths), 2)
    testthat::expect_true(is.data.frame(loaded$steps$step2$diagnostics$residuals_data))
    testthat::expect_true(isTRUE(loaded$steps$step2$was_fitting))
    testthat::expect_equal(loaded$steps$step3$file_name, "b.xlsx")
    testthat::expect_equal(nrow(loaded$steps$step3$sheets$integration), 2)
  })
})

testthat::test_that("sleep restore store keeps home active tab", {
  state <- home_sleep_restore_store_normalize_state(list(
    schema_version = home_sleep_restore_store_schema(),
    pending_restore = TRUE,
    saved_at = "2026-02-23T10:00:00Z",
    source_event = "suspend",
    lang = "en",
    active_tab = "home",
    steps = list(),
    restored_at = ""
  ))
  testthat::expect_equal(state$active_tab, "home")
})

testthat::test_that("sleep restore store keeps step2 active tab", {
  state <- home_sleep_restore_store_normalize_state(list(
    schema_version = home_sleep_restore_store_schema(),
    pending_restore = TRUE,
    saved_at = "2026-02-23T10:00:00Z",
    source_event = "suspend",
    lang = "en",
    active_tab = "step2",
    steps = list(),
    restored_at = ""
  ))
  testthat::expect_equal(state$active_tab, "step2")
})

testthat::test_that("sleep restore store falls back to default on schema mismatch", {
  with_temp_sleep_restore_store({
    path <- home_sleep_restore_store_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(schema_version = "bad.schema", pending_restore = TRUE), path)

    loaded <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    testthat::expect_equal(loaded$schema_version, home_sleep_restore_store_schema())
    testthat::expect_false(isTRUE(loaded$pending_restore))
    testthat::expect_equal(loaded$active_tab, "")
  })
})

testthat::test_that("sleep restore store falls back to default on corrupted file", {
  with_temp_sleep_restore_store({
    path <- home_sleep_restore_store_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("not an rds", path, useBytes = TRUE)

    loaded <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    testthat::expect_equal(loaded$schema_version, home_sleep_restore_store_schema())
    testthat::expect_false(isTRUE(loaded$pending_restore))
    testthat::expect_equal(loaded$steps, list())
  })
})

testthat::test_that("sleep restore pending flag can be consumed once", {
  with_temp_sleep_restore_store({
    state <- list(
      schema_version = home_sleep_restore_store_schema(),
      pending_restore = TRUE,
      saved_at = "2026-02-23T10:00:00Z",
      source_event = "suspend",
      active_tab = "step1",
      steps = list(step1 = list(source_path = "/tmp/a.itc", display_name = "a.itc", params = list(duration = 20))),
      restored_at = ""
    )
    testthat::expect_true(isTRUE(home_sleep_restore_store_save(state)))

    loaded <- home_sleep_restore_store_load()
    testthat::expect_true(isTRUE(loaded$pending_restore))
    loaded$pending_restore <- FALSE
    loaded$restored_at <- "2026-02-23T10:00:03Z"
    testthat::expect_true(isTRUE(home_sleep_restore_store_save(loaded)))

    loaded2 <- home_sleep_restore_store_load()
    testthat::expect_false(isTRUE(loaded2$pending_restore))
    testthat::expect_equal(loaded2$restored_at, "2026-02-23T10:00:03Z")
  })
})

testthat::test_that("sleep restore autosave pending flag is normalized off", {
  state <- home_sleep_restore_store_normalize_state(list(
    schema_version = home_sleep_restore_store_schema(),
    pending_restore = TRUE,
    saved_at = "2026-02-23T10:00:00Z",
    source_event = "autosave",
    active_tab = "step3",
    steps = list(step3 = list(source_path = "/tmp/a.xlsx", file_name = "a.xlsx", settings = list())),
    restored_at = ""
  ))

  testthat::expect_equal(state$source_event, "autosave")
  testthat::expect_false(isTRUE(state$pending_restore))
})
