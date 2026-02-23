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
source(file.path(web_dir, "R", "bridge_contract.R"), local = FALSE)

testthat::test_that("normalize_bridge_token keeps numeric token", {
  testthat::expect_equal(normalize_bridge_token("1.25"), 1.25)
})

testthat::test_that("normalize_bridge_token rejects invalid token", {
  testthat::expect_true(is.na(normalize_bridge_token("bad-token")))
})

testthat::test_that("sanitize_step1_payload rejects payload without integration", {
  testthat::expect_null(sanitize_step1_payload(NULL))
  testthat::expect_null(sanitize_step1_payload("bad"))
  testthat::expect_null(sanitize_step1_payload(list(source = "bad")))
})

testthat::test_that("sanitize_step1_payload accepts valid strict payload", {
  payload <- list(
    schema_version = "itcsuite.step1.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = "2.5",
    source = "",
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10),
      meta = data.frame(parameter = "Temp_K", value = "298.15", stringsAsFactors = FALSE)
    )
  )

  res <- sanitize_step1_payload(payload)
  testthat::expect_true(is.list(res))
  testthat::expect_equal(res$schema_version, "itcsuite.step1.v1")
  testthat::expect_equal(res$source, "Step1")
  testthat::expect_equal(res$token, 2.5)
  testthat::expect_equal(res$created_at, "2026-02-12T12:00:00.000Z")
  testthat::expect_true(is.data.frame(res$bundle$integration))
  testthat::expect_true(nrow(res$bundle$integration) == 1)
})

testthat::test_that("sanitize_step1_payload enforces strict schema and required columns", {
  bad_schema <- list(
    schema_version = "itcsuite.step1.v0",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 1,
    source = "s",
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10)
    )
  )
  testthat::expect_null(sanitize_step1_payload(bad_schema))

  payload <- list(
    schema_version = "itcsuite.step1.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 1,
    source = "s",
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      integration = data.frame(Injection = 2, Heat_ucal = 20)
    )
  )
  testthat::expect_null(sanitize_step1_payload(payload))
})

testthat::test_that("sanitize_step1_payload rejects missing created_at and bad bundle schema", {
  missing_created_at <- list(
    schema_version = "itcsuite.step1.v1",
    token = 1,
    source = "s",
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10)
    )
  )
  testthat::expect_null(sanitize_step1_payload(missing_created_at))

  bad_bundle_schema <- list(
    schema_version = "itcsuite.step1.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 1,
    source = "s",
    bundle = list(
      schema_version = "itcsuite.bundle.v0",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10)
    )
  )
  testthat::expect_null(sanitize_step1_payload(bad_bundle_schema))
})

testthat::test_that("sanitize_step2_plot_payload rejects empty body", {
  empty_payload <- list(source = "none")
  testthat::expect_null(sanitize_step2_plot_payload(empty_payload))
})

testthat::test_that("sanitize_step2_plot_payload normalizes valid payload", {
  payload <- list(
    schema_version = "itcsuite.step2_plot.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = "3.25",
    source = "bridge",
    source_label = "Step1 bridge",
    source_path = " /tmp/step2.xlsx ",
    sheets = list(integration = data.frame(Injection = 1))
  )
  res <- sanitize_step2_plot_payload(payload)
  testthat::expect_true(is.list(res))
  testthat::expect_equal(res$schema_version, "itcsuite.step2_plot.v1")
  testthat::expect_equal(res$source, "bridge")
  testthat::expect_equal(res$source_label, "Step1 bridge")
  testthat::expect_equal(res$source_path, "/tmp/step2.xlsx")
  testthat::expect_equal(res$token, 3.25)
  testthat::expect_true(is.list(res$sheets))
})

testthat::test_that("sanitize_step2_plot_payload rejects invalid source", {
  payload <- list(
    schema_version = "itcsuite.step2_plot.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 1,
    source = "Step2 bridge",
    sheets = list(integration = data.frame(Injection = 1))
  )
  testthat::expect_null(sanitize_step2_plot_payload(payload))
})

testthat::test_that("sanitize_step2_plot_payload rejects missing created_at", {
  payload <- list(
    schema_version = "itcsuite.step2_plot.v1",
    token = 1,
    source = "bridge",
    sheets = list(integration = data.frame(Injection = 1))
  )
  testthat::expect_null(sanitize_step2_plot_payload(payload))
})

testthat::test_that("make_bridge_channel stores only validated payloads", {
  validator <- function(x) {
    if (is.list(x) && !is.null(x$id)) return(x)
    NULL
  }
  ch <- make_bridge_channel(validator, "test_channel")

  testthat::expect_null(shiny::isolate(ch()))
  ch(list(id = 1L, value = "ok"))
  testthat::expect_equal(shiny::isolate(ch())$id, 1L)

  testthat::expect_warning(ch(list(value = "bad")), "Bridge payload rejected")
  testthat::expect_equal(shiny::isolate(ch())$id, 1L)

  ch(NULL)
  testthat::expect_null(shiny::isolate(ch()))
})
