resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  candidates <- unique(c(
    env_root,
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  ))
  for (cand in candidates) {
    if (!nzchar(cand)) next
    normalized <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (!dir.exists(normalized)) next
    if (dir.exists(file.path(normalized, "ITCsimfit")) && dir.exists(file.path(normalized, "tests"))) {
      return(normalizePath(normalized, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot resolve repository root for guide annotation tests.")
}

repo_root <- resolve_repo_root()
simfit_dir <- file.path(repo_root, "ITCsimfit")

source(file.path(simfit_dir, "R", "constants.R"), local = FALSE)
source(file.path(simfit_dir, "R", "guide_annotations.R"), local = FALSE)

testthat::test_that("guide annotation config passes baseline validation", {
  df <- load_guide_annotations(file.path(simfit_dir, "config", "guide_annotations.v1.csv"))
  check <- validate_guide_annotations(df)
  testthat::expect_true(isTRUE(check$ok))
  testthat::expect_length(check$errors, 0)
})

testthat::test_that("validator rejects missing required columns", {
  df <- load_guide_annotations(file.path(simfit_dir, "config", "guide_annotations.v1.csv"))
  df$control_id <- NULL
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("Missing required columns", check$errors)))
})

testthat::test_that("validator rejects invalid enums and invalid schema version", {
  df <- load_guide_annotations(file.path(simfit_dir, "config", "guide_annotations.v1.csv"))
  df$severity[1] <- "fatal"
  df$status[2] <- "enabled"
  df$schema_version[3] <- "itcsuite.guide_annotation.v0"
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("severity invalid", check$errors)))
  testthat::expect_true(any(grepl("status invalid", check$errors)))
  testthat::expect_true(any(grepl("schema_version", check$errors)))
})

testthat::test_that("validator rejects duplicate guide_id and blank control_id", {
  df <- load_guide_annotations(file.path(simfit_dir, "config", "guide_annotations.v1.csv"))
  df$guide_id[2] <- df$guide_id[1]
  df$control_id[1] <- ""
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("guide_id must be unique", check$errors)))
  testthat::expect_true(any(grepl("control_id must be non-empty", check$errors)))
})

testthat::test_that("validator enforces active language fields and version ranges", {
  df <- load_guide_annotations(file.path(simfit_dir, "config", "guide_annotations.v1.csv"))
  df$lang_en[1] <- ""
  df$since_version[2] <- "v1"
  df$since_version[3] <- "2.0.0"
  df$until_version[3] <- "1.0.0"
  check <- validate_guide_annotations(df)
  testthat::expect_false(isTRUE(check$ok))
  testthat::expect_true(any(grepl("active row must provide", check$errors)))
  testthat::expect_true(any(grepl("since_version format invalid", check$errors)))
  testthat::expect_true(any(grepl("since_version must be <= until_version", check$errors)))
})
