repo_root <- itcsuite_repo_root()
ui_src <- paste(readLines(file.path(repo_root, "ITCsimfit", "ui.R"), warn = FALSE), collapse = "\n")

testthat::test_that("step2 column scroll css is defined for independent vertical scrolling", {
  testthat::expect_true(
    grepl("\\.simfit-root\\s+\\.step2-col-scroll\\s*\\{", ui_src, perl = TRUE)
  )

  required_rules <- c(
    "max-height:\\s*calc\\(var\\(--itcsuite-vh,\\s*1vh\\)\\s*\\*\\s*100\\s*-\\s*var\\(--itcsuite-host-chrome,\\s*140px\\)\\)",
    "overflow-y:\\s*auto",
    "overflow-x:\\s*hidden",
    "min-height:\\s*0",
    "padding-right:\\s*5px",
    "overscroll-behavior:\\s*contain"
  )

  for (rule in required_rules) {
    testthat::expect_true(grepl(rule, ui_src, perl = TRUE), info = rule)
  }
})

testthat::test_that("step2 middle and right columns use dedicated independent scroll wrappers", {
  middle_pattern <- "column\\(3,\\s*class\\s*=\\s*\"middle-column\",\\s*div\\(class\\s*=\\s*\"step2-col-scroll\\s+step2-col2-scroll\""
  right_pattern <- "column\\(3,\\s*class\\s*=\\s*\"right-column\",\\s*div\\(class\\s*=\\s*\"step2-col-scroll\\s+step2-col3-scroll\""

  testthat::expect_true(grepl(middle_pattern, ui_src, perl = TRUE), info = middle_pattern)
  testthat::expect_true(grepl(right_pattern, ui_src, perl = TRUE), info = right_pattern)
})

testthat::test_that("step2 mobile layout resets column scroll wrappers to avoid nested scrolling", {
  mobile_fallback_pattern <- paste0(
    "@media\\s*\\(max-width:\\s*768px\\)\\s*\\{[\\s\\S]*?",
    "\\.simfit-root\\s+\\.step2-col-scroll\\s*\\{[\\s\\S]*?",
    "max-height:\\s*none;[\\s\\S]*?",
    "overflow:\\s*visible;[\\s\\S]*?",
    "padding-right:\\s*0;[\\s\\S]*?",
    "\\}"
  )

  testthat::expect_true(grepl(mobile_fallback_pattern, ui_src, perl = TRUE))
})
