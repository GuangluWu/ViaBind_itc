repo_root <- itcsuite_repo_root()

read_src <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

testthat::test_that("step3 replay on tab revisit is gated by pending flag", {
  src <- read_src(file.path(repo_root, "ITCgraph", "server.R"))

  testthat::expect_true(grepl(
    "step3_replay_pending\\s*<-\\s*reactiveVal\\(FALSE\\)",
    src,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "consumed\\s*<-\\s*isTRUE\\(consume_step2_plot_payload\\(payload\\)\\)",
    src,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "if\\s*\\(is_step3_tab_selected\\(input\\$main_tabs\\)\\)\\s*\\{\\s*step3_replay_pending\\(FALSE\\)\\s*\\}\\s*else\\s*\\{\\s*step3_replay_pending\\(TRUE\\)\\s*\\}",
    src,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "if\\s*\\(!isTRUE\\(step3_replay_pending\\(\\)\\)\\)\\s*return\\(invisible\\(FALSE\\)\\)",
    src,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "on\\.exit\\(step3_replay_pending\\(FALSE\\),\\s*add\\s*=\\s*TRUE\\)",
    src,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "consume_step2_plot_payload\\(payload,\\s*replay_only\\s*=\\s*TRUE\\)",
    src,
    perl = TRUE
  ))
})
