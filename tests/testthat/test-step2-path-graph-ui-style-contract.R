repo_root <- itcsuite_repo_root()
ui_src <- paste(readLines(file.path(repo_root, "ITCsimfit", "ui.R"), warn = FALSE), collapse = "\n")
graph_src <- paste(
  readLines(
    file.path(repo_root, "ITCsimfit", "R", "server", "body", "ui_i18n", "02_ui_outputs_report.R"),
    warn = FALSE
  ),
  collapse = "\n"
)

testthat::test_that("path graph css defines dedicated palette tokens", {
  required_tokens <- c(
    "--path-c-base:",
    "--path-c-rxn-d:",
    "--path-c-rxn-t:",
    "--path-c-rxn-b:",
    "--path-c-rxn-f:",
    "--path-c-rxn-u:",
    "--path-c-inactive-stroke:",
    "--path-c-inactive-fill:",
    "--path-c-text:",
    "--path-c-text-muted:",
    "--path-c-rxn-b-ink:"
  )

  for (token in required_tokens) {
    testthat::expect_true(
      grepl(token, ui_src, fixed = TRUE),
      info = paste("Missing path graph token:", token)
    )
  }
})

testthat::test_that("path graph node labels avoid glyph squeezing and keep fit guard", {
  testthat::expect_true(
    grepl("fit_text\\s*<-\\s*nchar\\(label,\\s*type\\s*=\\s*\"width\"\\)\\s*>=\\s*8", graph_src, perl = TRUE)
  )
  testthat::expect_true(
    grepl("textLength\\s*<-\\s*max\\(width\\s*-\\s*10,\\s*1\\)", graph_src, perl = TRUE)
  )
  testthat::expect_true(
    grepl("lengthAdjust\\s*<-\\s*\"spacing\"", graph_src, perl = TRUE)
  )
})

testthat::test_that("path graph output includes semantic path and state classes", {
  class_patterns <- c(
    "class\\s*=\\s*paste\\(\"path-edge-line\",\\s*path_cls,\\s*state_cls\\)",
    "class\\s*=\\s*paste\\(\"path-edge-toggle\",\\s*path_cls,\\s*state_cls\\)",
    "class\\s*=\\s*paste\\(\"path-edge-label\",\\s*path_cls,\\s*state_cls\\)",
    "class\\s*=\\s*paste\\(\\s*\"path-node\",\\s*path_cls,\\s*state_cls,"
  )

  for (pattern in class_patterns) {
    testthat::expect_true(grepl(pattern, graph_src, perl = TRUE), info = pattern)
  }

  required_literals <- c(
    "path-base",
    "path-rxn-d",
    "path-rxn-t",
    "path-rxn-b",
    "path-rxn-f",
    "path-rxn-u",
    "is-active",
    "is-inactive"
  )

  for (literal in required_literals) {
    testthat::expect_true(
      grepl(literal, graph_src, fixed = TRUE),
      info = paste("Missing class literal:", literal)
    )
  }
})
