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

with_temp_recent_store <- function(expr) {
  expr_sub <- substitute(expr)
  tmp_root <- tempfile("itcsuite-home-recent-")
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

source(file.path(web_dir, "R", "home_recent_helpers.R"), local = FALSE)
source(file.path(web_dir, "R", "home_recent_store.R"), local = FALSE)

testthat::test_that("home recent store save/load round trip works", {
  with_temp_recent_store({
    state <- list(
      schema_version = home_recent_store_schema(),
      next_seq = 3L,
      import_records = list(
        list(id = "r3", imported_at = "2026-02-14T10:00:03Z", source_path = "/tmp/c.xlsx"),
        list(id = "r2", imported_at = "2026-02-14T10:00:02Z", source_path = "/tmp/b.xlsx"),
        list(id = "r1", imported_at = "2026-02-14T10:00:01Z", source_path = "/tmp/a.xlsx")
      ),
      updated_at = "2026-02-14T10:00:03Z"
    )
    ok <- home_recent_store_save(state, max_records = 200L)
    testthat::expect_true(isTRUE(ok))
    testthat::expect_true(file.exists(home_recent_store_path()))

    loaded <- home_recent_store_load(max_records = 200L, warn_fn = function(...) NULL)
    testthat::expect_equal(loaded$schema_version, home_recent_store_schema())
    testthat::expect_equal(loaded$next_seq, 3L)
    testthat::expect_equal(length(loaded$import_records), 3L)
    testthat::expect_equal(loaded$import_records[[1]]$id, "r3")
  })
})

testthat::test_that("home recent store falls back to default on corrupted file", {
  with_temp_recent_store({
    path <- home_recent_store_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("not an rds", path, useBytes = TRUE)

    loaded <- home_recent_store_load(max_records = 200L, warn_fn = function(...) NULL)
    testthat::expect_equal(loaded$schema_version, home_recent_store_schema())
    testthat::expect_equal(loaded$next_seq, 0L)
    testthat::expect_equal(length(loaded$import_records), 0L)
  })
})

testthat::test_that("home recent store falls back to default on schema mismatch", {
  with_temp_recent_store({
    path <- home_recent_store_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(schema_version = "bad.schema", next_seq = 99L, import_records = list(list(id = "x"))), path)

    loaded <- home_recent_store_load(max_records = 200L)
    testthat::expect_equal(loaded$schema_version, home_recent_store_schema())
    testthat::expect_equal(loaded$next_seq, 0L)
    testthat::expect_equal(length(loaded$import_records), 0L)
  })
})

testthat::test_that("home recent store trims to 200 records", {
  with_temp_recent_store({
    origin <- as.POSIXct("2026-02-14 10:00:00", tz = "UTC")
    records <- lapply(seq_len(205L), function(i) {
      ts <- format(origin + i, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      list(id = sprintf("r%03d", i), imported_at = ts, source_path = sprintf("/tmp/%03d.xlsx", i))
    })
    state <- list(
      schema_version = home_recent_store_schema(),
      next_seq = 205L,
      import_records = records,
      updated_at = "2026-02-14T10:10:00Z"
    )
    ok <- home_recent_store_save(state, max_records = 200L)
    testthat::expect_true(isTRUE(ok))

    loaded <- home_recent_store_load(max_records = 200L)
    testthat::expect_equal(length(loaded$import_records), 200L)
    testthat::expect_equal(loaded$import_records[[1]]$id, "r205")
    testthat::expect_equal(loaded$import_records[[200]]$id, "r006")
  })
})

testthat::test_that("home recent store repairs next_seq from record ids when needed", {
  with_temp_recent_store({
    state <- list(
      schema_version = home_recent_store_schema(),
      next_seq = 1L,
      import_records = list(
        list(id = "home_rec_000123", imported_at = "2026-02-14T10:00:03Z", source_path = "/tmp/a.xlsx")
      ),
      updated_at = "2026-02-14T10:00:03Z"
    )
    ok <- home_recent_store_save(state, max_records = 200L)
    testthat::expect_true(isTRUE(ok))
    loaded <- home_recent_store_load(max_records = 200L)
    testthat::expect_equal(loaded$next_seq, 123L)
  })
})
