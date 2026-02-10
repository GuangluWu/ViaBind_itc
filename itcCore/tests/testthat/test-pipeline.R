test_that("minimal parse/process pipeline works", {
  root <- "/Users/guanglu/Documents/myScript/ITCSuite"
  core_dir <- file.path(root, "itcCore", "R")
  for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = FALSE)
  }

  raw <- parse_itc(file.path(root, "ITCprocessor", "ada2cb7c.itc"))
  processed <- process_itc(raw)

  expect_true(nrow(raw$data) > 0)
  expect_true(length(processed$corrected_power) == nrow(raw$data))
  expect_true("heat_cal_mol" %in% names(processed$integration))
})
