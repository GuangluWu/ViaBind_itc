source(file.path("..", "..", "R", "i18n.R"))

testthat::test_that("graph_normalize_lang normalizes zh/en", {
  testthat::expect_equal(graph_normalize_lang("zh"), "zh")
  testthat::expect_equal(graph_normalize_lang("ZH"), "zh")
  testthat::expect_equal(graph_normalize_lang("en"), "en")
  testthat::expect_equal(graph_normalize_lang("fr"), "en")
  testthat::expect_equal(graph_normalize_lang(NULL), "en")
})

testthat::test_that("graph_tr resolves bilingual labels and fallback", {
  testthat::expect_equal(graph_tr("section_import", "en"), "Data Import")
  testthat::expect_equal(graph_tr("section_import", "zh"), "数据导入")
  testthat::expect_equal(graph_tr("export_pdf", "zh"), "PDF")
  testthat::expect_equal(graph_tr("unknown_key", "zh"), "unknown_key")
})
