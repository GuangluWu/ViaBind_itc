repo_root <- itcsuite_repo_root()

read_src <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

testthat::test_that("desktop main wires suspend/resume power events into shiny input", {
  src <- read_src(file.path(repo_root, "desktop", "src", "main", "index.js"))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"suspend\"", src, perl = TRUE))
  testthat::expect_true(grepl("emitPowerEventToRenderer\\(\"suspend\"", src, perl = TRUE))
  testthat::expect_true(grepl("window\\.Shiny\\.setInputValue\\(\"itcsuite_power_event\"", src, perl = TRUE))
  testthat::expect_true(grepl("POWER_EVENT_REPLAY_WINDOW_MS", src, perl = TRUE))
  testthat::expect_true(grepl("replayLatestPowerEventToRenderer\\(\"did-finish-load\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("waitForShinyMs\\s*:\\s*10000", src, perl = TRUE))
  testthat::expect_true(grepl("isRendererSessionAlive", src, perl = TRUE))
  testthat::expect_true(grepl("session_still_alive", src, perl = TRUE))
  testthat::expect_true(grepl("renderer_session_probe", src, perl = TRUE))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"resume\"", src, perl = TRUE))
  testthat::expect_true(grepl(
    "emitPowerEventToRenderer\\(\"resume\"[^\\n]*\\n\\s*setTimeout\\(\\(\\)\\s*=\u003e\\s*recoverAfterResume\\(\"power-resume\"\\)",
    src,
    perl = TRUE
  ))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"unlock-screen\"", src, perl = TRUE))
  testthat::expect_true(grepl(
    "emitPowerEventToRenderer\\(\"unlock-screen\"[^\\n]*\\n\\s*setTimeout\\(\\(\\)\\s*=\u003e\\s*recoverAfterResume\\(\"unlock-screen\"\\)",
    src,
    perl = TRUE
  ))
})
