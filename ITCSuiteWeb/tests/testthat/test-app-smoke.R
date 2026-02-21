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
      testthat::expect_true(app$get_value(input = "main_tabs") %in% c("home", "step1", "step2", "step3"))
      testthat::expect_identical(app$get_value(input = "main_tabs"), "home")
      app$stop()
    })

    testthat::test_that("bridge action buttons stay operable across language switch", {
      app <- shinytest2::AppDriver$new(
        app_dir = web_app_dir,
        name = "app-lang-bridge-clicks-smoke",
        load_timeout = 60000
      )
      on.exit(app$stop(), add = TRUE)

      wait_idle <- function() {
        tryCatch(app$wait_for_idle(timeout = 8000), error = function(e) NULL)
        Sys.sleep(0.2)
      }

      read_click <- function(id) {
        val <- suppressWarnings(as.integer(app$get_value(input = id)))
        if (length(val) < 1 || !is.finite(val[1])) return(NA_integer_)
        as.integer(val[1])
      }

      click_and_expect_usable <- function(id) {
        testthat::expect_true(is.finite(read_click(id)))
        testthat::expect_no_error(app$set_inputs(.list = stats::setNames(list("click"), id)))
        wait_idle()
        testthat::expect_true(is.finite(read_click(id)))
      }

      app$set_inputs(main_tabs = "step1")
      click_and_expect_usable("btn_data_to_fit")
      app$set_inputs(host_lang_toggle = "click")
      wait_idle()
      click_and_expect_usable("btn_data_to_fit")

      app$set_inputs(main_tabs = "step2")
      click_and_expect_usable("data_to_plot")
      app$set_inputs(host_lang_toggle = "click")
      wait_idle()
      click_and_expect_usable("data_to_plot")

      testthat::expect_true(app$get_value(input = "main_tabs") %in% c("home", "step1", "step2", "step3"))
    })

    testthat::test_that("step3 ranges persist after tab revisit for same Step2 payload", {
      app <- shinytest2::AppDriver$new(
        app_dir = web_app_dir,
        name = "app-step3-tab-revisit-ranges-smoke",
        load_timeout = 60000
      )
      on.exit(app$stop(), add = TRUE)

      wait_idle <- function() {
        tryCatch(app$wait_for_idle(timeout = 8000), error = function(e) NULL)
        Sys.sleep(0.2)
      }

      wait_for_tab <- function(target, timeout_ms = 60000) {
        start <- Sys.time()
        repeat {
          current <- as.character(app$get_value(input = "main_tabs"))
          if (identical(current, target)) return(invisible(TRUE))
          elapsed_ms <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
          if (elapsed_ms > timeout_ms) {
            stop(sprintf("Timed out waiting for tab '%s' (current: '%s').", target, current))
          }
          Sys.sleep(0.25)
        }
      }

      wait_for_finite_input <- function(id, timeout_ms = 60000) {
        start <- Sys.time()
        repeat {
          val <- suppressWarnings(as.numeric(app$get_value(input = id)))
          if (length(val) >= 1 && is.finite(val[1])) return(invisible(TRUE))
          elapsed_ms <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
          if (elapsed_ms > timeout_ms) stop(sprintf("Timed out waiting for finite input '%s'.", id))
          Sys.sleep(0.2)
        }
      }

      click_until_tab <- function(button_id, target_tab, attempts = 8L) {
        attempts <- max(1L, suppressWarnings(as.integer(attempts)[1]))
        for (i in seq_len(attempts)) {
          app$click(input = button_id)
          wait_idle()
          current <- as.character(app$get_value(input = "main_tabs"))
          if (identical(current, target_tab)) return(invisible(TRUE))
          Sys.sleep(0.4)
        }
        stop(sprintf("Button '%s' did not navigate to tab '%s'.", button_id, target_tab))
      }

      read_step3_ranges <- function() {
        list(
          top_xmin = suppressWarnings(as.numeric(app$get_value(input = "top_xmin"))[1]),
          top_xmax = suppressWarnings(as.numeric(app$get_value(input = "top_xmax"))[1]),
          top_ymin = suppressWarnings(as.numeric(app$get_value(input = "top_ymin"))[1]),
          top_ymax = suppressWarnings(as.numeric(app$get_value(input = "top_ymax"))[1]),
          bot_xmin = suppressWarnings(as.numeric(app$get_value(input = "bot_xmin"))[1]),
          bot_xmax = suppressWarnings(as.numeric(app$get_value(input = "bot_xmax"))[1]),
          bot_ymin = suppressWarnings(as.numeric(app$get_value(input = "bot_ymin"))[1]),
          bot_ymax = suppressWarnings(as.numeric(app$get_value(input = "bot_ymax"))[1]),
          bot_no_dim_range = suppressWarnings(as.integer(app$get_value(input = "bot_no_dim_range")))
        )
      }

      c3_file <- normalizePath(
        file.path(repo_root, "Examples", "c3-itc.itc"),
        winslash = "/",
        mustWork = TRUE
      )
      app$set_inputs(main_tabs = "step1")
      wait_for_tab("step1")
      app$upload_file(file1 = c3_file)
      wait_idle()
      click_until_tab("btn_data_to_fit", "step2")
      wait_for_tab("step2")
      click_until_tab("data_to_plot", "step3")
      wait_for_tab("step3")
      wait_idle()

      ids <- c("top_xmin", "top_xmax", "top_ymin", "top_ymax", "bot_xmin", "bot_xmax", "bot_ymin", "bot_ymax")
      for (id in ids) wait_for_finite_input(id)

      current_no_dim <- suppressWarnings(as.integer(app$get_value(input = "bot_no_dim_range")))
      n_inj <- if (length(current_no_dim) >= 2 && is.finite(current_no_dim[2])) current_no_dim[2] else 3L
      target_no_dim_end <- max(2L, min(as.integer(n_inj), 6L))
      target_no_dim_start <- if (target_no_dim_end >= 3L) 2L else 1L

      target_values <- list(
        top_xmin = -1.25,
        top_xmax = 8.75,
        top_ymin = -2.5,
        top_ymax = 1.5,
        bot_xmin = 0.2,
        bot_xmax = 1.4,
        bot_ymin = -1.2,
        bot_ymax = 0.3,
        bot_no_dim_range = c(target_no_dim_start, target_no_dim_end)
      )

      app$set_inputs(
        top_xmin = target_values$top_xmin,
        top_xmax = target_values$top_xmax,
        top_ymin = target_values$top_ymin,
        top_ymax = target_values$top_ymax,
        bot_xmin = target_values$bot_xmin,
        bot_xmax = target_values$bot_xmax,
        bot_ymin = target_values$bot_ymin,
        bot_ymax = target_values$bot_ymax,
        bot_no_dim_range = target_values$bot_no_dim_range
      )
      wait_idle()

      expected <- read_step3_ranges()

      app$set_inputs(main_tabs = "step1")
      wait_for_tab("step1")
      app$set_inputs(main_tabs = "step3")
      wait_for_tab("step3")
      wait_idle()
      app$set_inputs(main_tabs = "step2")
      wait_for_tab("step2")
      app$set_inputs(main_tabs = "step3")
      wait_for_tab("step3")
      wait_idle()

      actual <- read_step3_ranges()
      testthat::expect_equal(actual$top_xmin, expected$top_xmin, tolerance = 1e-8)
      testthat::expect_equal(actual$top_xmax, expected$top_xmax, tolerance = 1e-8)
      testthat::expect_equal(actual$top_ymin, expected$top_ymin, tolerance = 1e-8)
      testthat::expect_equal(actual$top_ymax, expected$top_ymax, tolerance = 1e-8)
      testthat::expect_equal(actual$bot_xmin, expected$bot_xmin, tolerance = 1e-8)
      testthat::expect_equal(actual$bot_xmax, expected$bot_xmax, tolerance = 1e-8)
      testthat::expect_equal(actual$bot_ymin, expected$bot_ymin, tolerance = 1e-8)
      testthat::expect_equal(actual$bot_ymax, expected$bot_ymax, tolerance = 1e-8)
      testthat::expect_equal(actual$bot_no_dim_range, expected$bot_no_dim_range)
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
