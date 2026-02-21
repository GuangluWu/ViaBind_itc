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

source(file.path(graph_dir, "R", "export_metadata.R"), local = FALSE)

testthat::test_that("app version resolver uses env then fallback", {
  old_env <- Sys.getenv("ITCSUITE_APP_VERSION", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("ITCSUITE_APP_VERSION") else Sys.setenv(ITCSUITE_APP_VERSION = old_env)
  }, add = TRUE)

  Sys.unsetenv("ITCSUITE_APP_VERSION")
  testthat::expect_equal(graph_meta_resolve_app_version(session = NULL, fallback = "0.0.0-dev"), "0.0.0-dev")

  Sys.setenv(ITCSUITE_APP_VERSION = "0.3.7")
  testthat::expect_equal(graph_meta_resolve_app_version(session = NULL, fallback = "0.0.0-dev"), "0.3.7")
})

testthat::test_that("app version resolver prefers host metadata api", {
  old_env <- Sys.getenv("ITCSUITE_APP_VERSION", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("ITCSUITE_APP_VERSION") else Sys.setenv(ITCSUITE_APP_VERSION = old_env)
  }, add = TRUE)
  Sys.setenv(ITCSUITE_APP_VERSION = "9.9.9")

  fake_session <- list(
    userData = list(
      itcsuite_app_meta = list(
        get_app_version = function() "1.2.3"
      )
    )
  )

  testthat::expect_equal(
    graph_meta_resolve_app_version(session = fake_session, fallback = "0.0.0-dev"),
    "1.2.3"
  )
})

testthat::test_that("pdf metadata composes expected title and author", {
  fake_session <- list(
    userData = list(
      itcsuite_app_meta = list(
        get_app_version = function() "0.3.0",
        get_developer_profile = function() list(
          name = "Guanglu Wu (Wu Guanglu)",
          email = "guanglu.wu@gmail.com",
          website = "https://guanglu.xyz"
        )
      )
    )
  )

  meta <- graph_meta_resolve_pdf_metadata(
    session = fake_session,
    module_name = "ITCgraph",
    fallback_version = "0.0.0-dev"
  )

  testthat::expect_equal(meta$title, "ViaBind v0.3.0: ITCgraph")
  testthat::expect_equal(meta$author, "Guanglu Wu (Wu Guanglu) <guanglu.wu@gmail.com> | https://guanglu.xyz")
})
