if (!requireNamespace("testthat", quietly = TRUE) || !requireNamespace("shinytest2", quietly = TRUE)) {
  message("Skipping shinytest2 smoke test: testthat/shinytest2 not installed.")
} else {
  testthat::test_that("app launches", {
    app <- shinytest2::AppDriver$new(
      app_dir = "/Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb",
      name = "app-launch-smoke",
      load_timeout = 60000
    )
    testthat::expect_true(app$get_value(input = "main_tabs") %in% c("Step 1 处理", "Step 2 拟合", "Step 3 出图与导出", "Metrics"))
    app$stop()
  })
}
