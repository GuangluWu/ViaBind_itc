test_that("bundle xlsx roundtrip works", {
  root <- "/Users/guanglu/Documents/myScript/ITCSuite"
  core_dir <- file.path(root, "itcCore", "R")
  for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = FALSE)
  }

  raw <- parse_itc(file.path(root, "ITCprocessor", "ada2cb7c.itc"))
  processed <- process_itc(raw)
  bundle <- create_bundle_from_processed(raw, processed, source_file = "ada2cb7c.itc")

  out <- tempfile(fileext = ".xlsx")
  export_bundle(bundle, out, compat_legacy = TRUE)
  back <- import_bundle(out)

  expect_true(validate_itc_bundle(back))
  expect_equal(nrow(back$integration), nrow(bundle$integration))
})
