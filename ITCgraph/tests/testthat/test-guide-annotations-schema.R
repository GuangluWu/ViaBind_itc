resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    p <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(p, "ITCgraph")) && dir.exists(file.path(p, "tests"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  cur <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:8) {
    if (dir.exists(file.path(cur, "ITCgraph")) && dir.exists(file.path(cur, "tests"))) {
      return(normalizePath(cur, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Cannot resolve repository root.")
}

repo_root <- resolve_repo_root()
graph_dir <- file.path(repo_root, "ITCgraph")

source(file.path(graph_dir, "R", "guide_annotations.R"), local = FALSE)

testthat::test_that("ITCgraph guide config passes baseline validation", {
  df <- load_guide_annotations(file.path(graph_dir, "config", "guide_annotations.v1.csv"))
  check <- validate_guide_annotations(df)
  testthat::expect_true(isTRUE(check$ok))
  testthat::expect_length(check$errors, 0)
})

testthat::test_that("validator rejects missing columns and invalid enums", {
  df <- load_guide_annotations(file.path(graph_dir, "config", "guide_annotations.v1.csv"))
  df$control_id <- NULL
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("Missing required columns", check$errors)))

  df2 <- load_guide_annotations(file.path(graph_dir, "config", "guide_annotations.v1.csv"))
  df2$severity[1] <- "fatal"
  check2 <- validate_guide_annotations(df2)
  testthat::expect_false(isTRUE(check2$ok))
  testthat::expect_true(any(grepl("severity invalid", check2$errors)))
})

testthat::test_that("validator rejects duplicate ids and invalid versions", {
  df <- load_guide_annotations(file.path(graph_dir, "config", "guide_annotations.v1.csv"))
  df$guide_id[2] <- df$guide_id[1]
  df$since_version[1] <- "v1"
  df$since_version[2] <- "2.0.0"
  df$until_version[2] <- "1.0.0"
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("guide_id must be unique", check$errors)))
  testthat::expect_true(any(grepl("since_version format invalid", check$errors)))
  testthat::expect_true(any(grepl("since_version must be <= until_version", check$errors)))
})
