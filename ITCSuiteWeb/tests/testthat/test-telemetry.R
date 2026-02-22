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
source(file.path(web_dir, "R", "telemetry.R"), local = FALSE)

parse_last_jsonl <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  testthat::expect_true(length(lines) >= 1L)
  jsonlite::fromJSON(lines[[length(lines)]], simplifyVector = FALSE)
}

testthat::test_that("telemetry_create_session writes structured JSONL entry", {
  td <- file.path(tempdir(), paste0("telemetry_test_", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  old_env <- Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ITCSUITE_USER_DATA_DIR")
    } else {
      Sys.setenv(ITCSUITE_USER_DATA_DIR = old_env)
    }
  }, add = TRUE)
  Sys.setenv(ITCSUITE_USER_DATA_DIR = td)

  session <- telemetry_create_session(app_version = "1.2.3", runtime = "desktop", retention_days = 30L)
  session$log_event(
    event = "home.user_action",
    level = "INFO",
    module = "host",
    payload = list(action = "lang_toggle", path = file.path(path.expand("~"), "secret", "a.txt"))
  )

  log_path <- file.path(td, "logs", "app-events.log")
  testthat::expect_true(file.exists(log_path))
  entry <- parse_last_jsonl(log_path)
  testthat::expect_equal(entry$event, "home.user_action")
  testthat::expect_equal(entry$runtime, "desktop")
  testthat::expect_equal(entry$app_version, "1.2.3")
  testthat::expect_true(is.list(entry$payload))
  payload_path <- ""
  if (is.list(entry$payload$path) && length(entry$payload$path) >= 1L) {
    payload_path <- as.character(entry$payload$path[[1]])
  } else if (is.character(entry$payload$path) && length(entry$payload$path) >= 1L) {
    payload_path <- entry$payload$path[[1]]
  }
  testthat::expect_true(startsWith(payload_path, "~") || grepl("<itcsuite_user_data>", payload_path, fixed = TRUE))
})

testthat::test_that("telemetry rotation creates rolled file when size threshold reached", {
  td <- file.path(tempdir(), paste0("telemetry_rotate_", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  old_env <- Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ITCSUITE_USER_DATA_DIR")
    } else {
      Sys.setenv(ITCSUITE_USER_DATA_DIR = old_env)
    }
  }, add = TRUE)
  Sys.setenv(ITCSUITE_USER_DATA_DIR = td)

  session <- telemetry_create_session(app_version = "1.2.3", runtime = "desktop", retention_days = 30L, rotate_bytes = 512)
  for (i in seq_len(24)) {
    session$log_event(
      event = "step2.fit",
      level = "INFO",
      module = "step2",
      payload = list(index = i, msg = paste(rep("x", 120), collapse = ""))
    )
  }

  files <- list.files(file.path(td, "logs"), full.names = FALSE)
  has_rotated <- any(grepl("^app-events\\.log\\.", files))
  testthat::expect_true(has_rotated)
})

testthat::test_that("telemetry cleanup removes log files older than retention window", {
  td <- file.path(tempdir(), paste0("telemetry_cleanup_", as.integer(Sys.time())))
  logs_dir <- file.path(td, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  stale_file <- file.path(logs_dir, "stale.log")
  writeLines("old", stale_file, useBytes = TRUE)
  Sys.setFileTime(stale_file, Sys.time() - as.difftime(40, units = "days"))

  old_env <- Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ITCSUITE_USER_DATA_DIR")
    } else {
      Sys.setenv(ITCSUITE_USER_DATA_DIR = old_env)
    }
  }, add = TRUE)
  Sys.setenv(ITCSUITE_USER_DATA_DIR = td)

  telemetry_create_session(app_version = "1.2.3", runtime = "desktop", retention_days = 30L)
  testthat::expect_false(file.exists(stale_file))
})
