# ==============================================================================
# runner_utils.R - Shared helpers for ITCSuite test runners
# ==============================================================================

detect_repo_root <- function() {
  candidates <- unique(c(
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  ))
  for (d in candidates) {
    if (!dir.exists(d)) next
    if (dir.exists(file.path(d, "ITCprocessor")) &&
        dir.exists(file.path(d, "ITCsimfit")) &&
        dir.exists(file.path(d, "ITCgraph")) &&
        dir.exists(file.path(d, "ITCSuiteWeb"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot detect ITCSuite repo root from current working directory.")
}

parse_strict_flag <- function(args) {
  any(args %in% c("--strict", "-s"))
}

run_testthat_file <- function(path, label = NULL) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    return(list(ok = FALSE, detail = "Package `testthat` is required."))
  }
  if (!file.exists(path)) {
    return(list(ok = FALSE, detail = paste0("Missing test file: ", path)))
  }
  lbl <- if (is.null(label)) basename(path) else label
  cat(sprintf("\n[TESTTHAT FILE] %s\n", lbl))
  ok <- tryCatch({
    testthat::test_file(path, reporter = testthat::StopReporter$new())
    TRUE
  }, error = function(e) {
    cat(sprintf("[FAIL] %s: %s\n", lbl, conditionMessage(e)))
    FALSE
  })
  list(ok = isTRUE(ok), detail = if (ok) "passed" else "failed")
}

run_testthat_dir <- function(path, label = NULL) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    return(list(ok = FALSE, detail = "Package `testthat` is required."))
  }
  if (!dir.exists(path)) {
    return(list(ok = FALSE, detail = paste0("Missing test dir: ", path)))
  }
  lbl <- if (is.null(label)) basename(path) else label
  cat(sprintf("\n[TESTTHAT DIR] %s\n", lbl))
  ok <- tryCatch({
    testthat::test_dir(path, reporter = testthat::StopReporter$new())
    TRUE
  }, error = function(e) {
    cat(sprintf("[FAIL] %s: %s\n", lbl, conditionMessage(e)))
    FALSE
  })
  list(ok = isTRUE(ok), detail = if (ok) "passed" else "failed")
}

extract_legacy_failed_count <- function(env, value_obj) {
  if (exists("tests_failed", envir = env, inherits = FALSE)) {
    v <- suppressWarnings(as.integer(get("tests_failed", envir = env, inherits = FALSE))[1])
    if (is.finite(v)) return(max(v, 0L))
  }
  if (exists("pass_count", envir = env, inherits = FALSE) &&
      exists("test_count", envir = env, inherits = FALSE)) {
    pass <- suppressWarnings(as.integer(get("pass_count", envir = env, inherits = FALSE))[1])
    total <- suppressWarnings(as.integer(get("test_count", envir = env, inherits = FALSE))[1])
    if (is.finite(pass) && is.finite(total) && total >= pass) return(max(total - pass, 0L))
  }
  if (is.list(value_obj) && !is.null(value_obj$failed)) {
    v <- suppressWarnings(as.integer(value_obj$failed)[1])
    if (is.finite(v)) return(max(v, 0L))
  }
  if (exists("test_results", envir = env, inherits = FALSE)) {
    tr <- get("test_results", envir = env, inherits = FALSE)
    if (is.list(tr)) {
      vals <- unlist(tr, use.names = FALSE)
      if (is.logical(vals)) return(sum(!vals, na.rm = TRUE))
    }
  }
  NA_integer_
}

run_legacy_script <- function(script_path, working_dir = NULL, label = NULL) {
  resolved_path <- script_path
  if (!file.exists(resolved_path) && !is.null(working_dir) && !grepl("^/", script_path)) {
    candidate <- file.path(working_dir, script_path)
    if (file.exists(candidate)) resolved_path <- candidate
  }
  if (!file.exists(resolved_path)) {
    return(list(ok = FALSE, detail = paste0("Missing legacy script: ", script_path), failed = NA_integer_))
  }
  lbl <- if (is.null(label)) basename(script_path) else label
  cat(sprintf("\n[LEGACY SCRIPT] %s\n", lbl))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  if (!is.null(working_dir)) setwd(working_dir)

  env <- new.env(parent = globalenv())
  value_obj <- NULL
  err <- tryCatch({
    src <- source(resolved_path, local = env)
    value_obj <<- src$value
    NULL
  }, error = function(e) e)

  if (inherits(err, "error")) {
    return(list(ok = FALSE, detail = conditionMessage(err), failed = NA_integer_))
  }

  failed_count <- extract_legacy_failed_count(env, value_obj)
  if (is.na(failed_count)) {
    return(list(ok = FALSE, detail = "Cannot determine pass/fail from legacy script output.", failed = NA_integer_))
  }

  list(
    ok = failed_count == 0L,
    detail = if (failed_count == 0L) "passed" else sprintf("%d failed", failed_count),
    failed = failed_count
  )
}

print_runner_summary <- function(results) {
  cat("\n============================================================\n")
  cat("Runner Summary\n")
  cat("============================================================\n")
  for (nm in names(results)) {
    status <- if (isTRUE(results[[nm]]$ok)) "PASS" else "FAIL"
    cat(sprintf("- [%s] %s: %s\n", status, nm, results[[nm]]$detail))
  }
  cat("============================================================\n")
}
