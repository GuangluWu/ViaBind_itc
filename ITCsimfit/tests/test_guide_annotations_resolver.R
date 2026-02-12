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

testthat::test_that("resolver returns english and chinese texts", {
  en_hit <- resolve_guide_annotation("sim_to_exp", lang = "en", app_version = "1.0.0")
  zh_hit <- resolve_guide_annotation("sim_to_exp", lang = "zh", app_version = "1.0.0")

  testthat::expect_true(nrow(en_hit) >= 1)
  testthat::expect_true(nrow(zh_hit) >= 1)
  testthat::expect_true(any(grepl("simulation", en_hit$text, ignore.case = TRUE)))
  testthat::expect_true(any(grepl("模拟", zh_hit$text)))
})

testthat::test_that("resolver applies app version ranges", {
  miss <- resolve_guide_annotation("fit_global", lang = "en", app_version = "1.1.0")
  hit <- resolve_guide_annotation("fit_global", lang = "en", app_version = "1.2.0")

  testthat::expect_equal(nrow(miss), 0)
  testthat::expect_true(nrow(hit) >= 1)
})

testthat::test_that("resolver filters inactive rows", {
  hit <- resolve_guide_annotation("heat_offset", lang = "en", app_version = "1.0.0")
  testthat::expect_true(nrow(hit) >= 1)
  testthat::expect_true(all(hit$status == "active"))
})

testthat::test_that("resolver returns empty result for unknown control id", {
  miss <- resolve_guide_annotation("not_existing_control_id", lang = "en", app_version = "1.0.0")
  testthat::expect_equal(nrow(miss), 0)
})

testthat::test_that("resolver falls back to english for unsupported language code", {
  hit <- resolve_guide_annotation("sim_to_exp", lang = "fr", app_version = "1.0.0")
  testthat::expect_true(nrow(hit) >= 1)
  testthat::expect_true(any(grepl("simulation", hit$text, ignore.case = TRUE)))
  testthat::expect_true(all(hit$lang_used == "en"))
})
