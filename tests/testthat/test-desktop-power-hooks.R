repo_root <- itcsuite_repo_root()

read_src <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

testthat::test_that("desktop main wires suspend/resume power events into shiny input", {
  # Power event listeners and most logic moved from index.js to power-events.js
  events_src <- read_src(file.path(repo_root, "desktop", "src", "main", "power-events.js"))
  # Main entry point still calls replay from did-finish-load
  index_src <- read_src(file.path(repo_root, "desktop", "src", "main", "index.js"))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"suspend\"", events_src, perl = TRUE))
  testthat::expect_true(grepl("emitPowerEventToRenderer\\(\"suspend\"", events_src, perl = TRUE))
  testthat::expect_true(grepl("window\\.Shiny\\.setInputValue\\(\"itcsuite_power_event\"", events_src, perl = TRUE))
  testthat::expect_true(grepl("POWER_EVENT_REPLAY_WINDOW_MS", events_src, perl = TRUE))

  # replayLatestPowerEventToRenderer call is in index.js, but helper is in power-events.js
  testthat::expect_true(grepl("replayLatestPowerEventToRenderer\\(\"did-finish-load\"\\)", index_src, perl = TRUE))
  testthat::expect_true(grepl("waitForShinyMs\\s*:\\s*10000", events_src, perl = TRUE))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"resume\"", events_src, perl = TRUE))
  testthat::expect_true(grepl(
    "emitPowerEventToRenderer\\(\"resume\"[\\s\\S]*recoverAfterResume\\(\"power-resume\"\\)",
    events_src,
    perl = TRUE
  ))

  testthat::expect_true(grepl("powerMonitor\\.on\\(\"unlock-screen\"", events_src, perl = TRUE))
  testthat::expect_true(grepl(
    "emitPowerEventToRenderer\\(\"unlock-screen\"[\\s\\S]*recoverAfterResume\\(\"unlock-screen\"\\)",
    events_src,
    perl = TRUE
  ))
})
