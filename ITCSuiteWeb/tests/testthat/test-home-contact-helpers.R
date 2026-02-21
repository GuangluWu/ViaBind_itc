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

source(file.path(web_dir, "R", "home_contact_helpers.R"), local = FALSE)

testthat::test_that("home_contact_qr_filename maps language to configured filenames", {
  testthat::expect_equal(home_contact_qr_filename("en"), "bmc_qr_en.png")
  testthat::expect_equal(home_contact_qr_filename("zh"), "bmc_qr_zh.png")
  testthat::expect_equal(home_contact_qr_filename("fr"), "bmc_qr_en.png")
})

testthat::test_that("home_contact_resolve_qr_src falls back to english QR when zh is missing", {
  assets_dir <- file.path(tempdir(), paste0("qr_test_", as.integer(Sys.time())))
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(assets_dir, "bmc_qr_en.png"))

  out <- home_contact_resolve_qr_src(
    lang = "zh",
    assets_dir = assets_dir,
    resource_prefix = "/assets"
  )
  testthat::expect_true(isTRUE(out$exists))
  testthat::expect_true(isTRUE(out$fallback_to_en))
  testthat::expect_equal(out$src, "/assets/bmc_qr_en.png")
})

testthat::test_that("home_contact_validate_https_url accepts only https URLs", {
  testthat::expect_equal(
    home_contact_validate_https_url("https://buymeacoffee.com/example"),
    "https://buymeacoffee.com/example"
  )
  testthat::expect_equal(home_contact_validate_https_url("http://buymeacoffee.com/example"), "")
  testthat::expect_equal(home_contact_validate_https_url("javascript:alert(1)"), "")
  testthat::expect_equal(home_contact_validate_https_url(""), "")
})

testthat::test_that("home_contact_read_viabind_version reads desktop package.json and falls back", {
  sandbox <- file.path(tempdir(), paste0("home_contact_ver_", as.integer(Sys.time())))
  dir.create(file.path(sandbox, "desktop"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("{", '  "name": "viabind-desktop",', '  "version": "1.2.3"', "}"),
    con = file.path(sandbox, "desktop", "package.json"),
    useBytes = TRUE
  )

  testthat::expect_equal(
    home_contact_read_viabind_version(repo_root = sandbox, default_version = "x.x.x"),
    "1.2.3"
  )
  testthat::expect_equal(
    home_contact_read_viabind_version(repo_root = file.path(sandbox, "missing"), default_version = "x.x.x"),
    "x.x.x"
  )
})

testthat::test_that("home_contact_build_viabind_signature builds expected string", {
  sandbox <- file.path(tempdir(), paste0("home_contact_sig_", as.integer(Sys.time())))
  dir.create(file.path(sandbox, "desktop"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("{", '  "version": "9.8.7"', "}"),
    con = file.path(sandbox, "desktop", "package.json"),
    useBytes = TRUE
  )
  testthat::expect_equal(
    home_contact_build_viabind_signature(repo_root = sandbox, default_version = "x.x.x"),
    "ViaBind v9.8.7"
  )
})

testthat::test_that("home_contact_read_viabind_version prefers ITCSUITE_APP_VERSION", {
  old_env <- Sys.getenv("ITCSUITE_APP_VERSION", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ITCSUITE_APP_VERSION")
    } else {
      Sys.setenv(ITCSUITE_APP_VERSION = old_env)
    }
  }, add = TRUE)

  Sys.setenv(ITCSUITE_APP_VERSION = "7.7.7")
  testthat::expect_equal(
    home_contact_read_viabind_version(repo_root = file.path(tempdir(), "missing"), default_version = "x.x.x"),
    "7.7.7"
  )
  testthat::expect_equal(
    home_contact_build_viabind_signature(repo_root = file.path(tempdir(), "missing"), default_version = "x.x.x"),
    "ViaBind v7.7.7"
  )
})
