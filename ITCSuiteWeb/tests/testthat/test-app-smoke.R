strict_smoke <- identical(tolower(Sys.getenv("ITCSUITE_STRICT_SMOKE", unset = "false")), "true")
repo_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
if (!nzchar(repo_root)) {
  repo_root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
}
web_app_dir <- normalizePath(file.path(repo_root, "ITCSuiteWeb"), winslash = "/", mustWork = FALSE)
simfit_bridge_file <- normalizePath(
  file.path(repo_root, "ITCsimfit", "R", "bridge_step1_import.R"),
  winslash = "/",
  mustWork = FALSE
)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Smoke tests require package `testthat`.")
} else {
  if (requireNamespace("shinytest2", quietly = TRUE)) {
    testthat::test_that("app launches", {
      app <- shinytest2::AppDriver$new(
        app_dir = web_app_dir,
        name = "app-launch-smoke",
        load_timeout = 60000
      )
      testthat::expect_true(app$get_value(input = "main_tabs") %in% c("step1", "step2", "step3"))
      app$stop()
    })
  } else {
    if (isTRUE(strict_smoke)) {
      stop("Strict smoke mode requires package `shinytest2`.")
    }
    message("Skipping shinytest2 app launch smoke: shinytest2 not installed.")
  }

  source(simfit_bridge_file)

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

  testthat::test_that("bridge smoke: Step1->Step2 single consume", {
    consume_once <- make_signature_consumer()
    payload <- list(token = 10.0, source = "bridge")

    testthat::expect_true(consume_once(payload))
    testthat::expect_false(consume_once(payload))
  })

  testthat::test_that("bridge smoke: Step2->Step3 single consume", {
    consume_once <- make_signature_consumer()
    payload <- list(token = 20.0, source = "file")

    testthat::expect_true(consume_once(payload))
    testthat::expect_false(consume_once(payload))
  })

  testthat::test_that("bridge smoke: duplicate token dedup", {
    consume_once <- make_signature_consumer()
    payload_a <- list(token = 30.0, source = "bridge")
    payload_b <- list(token = 30.0, source = "bridge")
    payload_c <- list(token = 30.1, source = "bridge")

    testthat::expect_true(consume_once(payload_a))
    testthat::expect_false(consume_once(payload_b))
    testthat::expect_true(consume_once(payload_c))
  })
}
