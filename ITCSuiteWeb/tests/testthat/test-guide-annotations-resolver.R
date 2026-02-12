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

source(file.path(web_dir, "R", "guide_annotations.R"), local = FALSE)

testthat::test_that("resolver returns zh/en rows", {
  en <- resolve_guide_annotation("main_tabs", lang = "en", app_version = "1.0.0")
  zh <- resolve_guide_annotation("main_tabs", lang = "zh", app_version = "1.0.0")
  testthat::expect_true(nrow(en) >= 1)
  testthat::expect_true(nrow(zh) >= 1)
  testthat::expect_true(any(grepl("tab", en$text, ignore.case = TRUE)))
  testthat::expect_true(any(grepl("标签页", zh$text)))
})

testthat::test_that("resolver filters by status and version", {
  inactive <- resolve_guide_annotation("legacy_simfit_ui", lang = "en", app_version = "1.0.0")
  testthat::expect_equal(nrow(inactive), 0)

  active <- resolve_guide_annotation("main_tabs", lang = "en", app_version = "1.0.0")
  testthat::expect_true(nrow(active) >= 1)
  testthat::expect_true(all(active$status == "active"))
})

testthat::test_that("resolver returns empty for unknown control id", {
  miss <- resolve_guide_annotation("missing_control_id", lang = "en", app_version = "1.0.0")
  testthat::expect_equal(nrow(miss), 0)
})

testthat::test_that("resolver unsupported language falls back to english", {
  hit <- resolve_guide_annotation("main_tabs", lang = "fr", app_version = "1.0.0")
  testthat::expect_true(nrow(hit) >= 1)
  testthat::expect_true(all(hit$lang_used == "en"))
})
