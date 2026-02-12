repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCSuiteWeb", "R", "bridge_contract.R"))
source(file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"))

make_signature_consumer <- function() {
  state <- new.env(parent = emptyenv())
  state$last_sig <- NA_character_
  function(payload) {
    sig <- build_step1_bridge_signature(payload)
    if (is.na(sig) || identical(sig, state$last_sig)) return(FALSE)
    state$last_sig <- sig
    TRUE
  }
}

testthat::test_that("Step1 payload enforces required schema fields", {
  payload_missing_created_at <- list(
    schema_version = "itcsuite.step1.v1",
    token = 1,
    source = "Step1",
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10)
    )
  )
  testthat::expect_null(sanitize_step1_payload(payload_missing_created_at))

  payload_bad_bundle_schema <- list(
    schema_version = "itcsuite.step1.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 1,
    source = "Step1",
    bundle = list(
      schema_version = "itcsuite.bundle.v0",
      integration = data.frame(Injection = 1, Ratio_App = 0.1, Heat_ucal = 10)
    )
  )
  testthat::expect_null(sanitize_step1_payload(payload_bad_bundle_schema))
})

testthat::test_that("Step1 signature deduplicates identical token+source", {
  consume_once <- make_signature_consumer()
  payload <- list(token = 10, source = "bridge")
  testthat::expect_true(consume_once(payload))
  testthat::expect_false(consume_once(payload))
})

testthat::test_that("Step1 signature differentiates source for same token", {
  sig_a <- build_step1_bridge_signature(list(token = 10, source = "bridge"))
  sig_b <- build_step1_bridge_signature(list(token = 10, source = "file"))
  testthat::expect_false(identical(sig_a, sig_b))
})

testthat::test_that("Step2 plot payload strict validation keeps required body", {
  valid_payload <- list(
    schema_version = "itcsuite.step2_plot.v1",
    created_at = "2026-02-12T12:00:00.000Z",
    token = 2,
    source = "bridge",
    sheets = list(simulation = data.frame(Ratio_App = 0.1, dQ_App = -10))
  )
  testthat::expect_true(is.list(sanitize_step2_plot_payload(valid_payload)))

  invalid_payload <- valid_payload
  invalid_payload$source <- "bad"
  testthat::expect_null(sanitize_step2_plot_payload(invalid_payload))
})

