repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCsimfit", "R", "path_selection_helpers.R"))

testthat::test_that("normalize_active_paths_with_dependencies enforces F->D and valid ordering", {
  testthat::expect_equal(
    normalize_active_paths_with_dependencies(c("rxn_F")),
    c("rxn_D", "rxn_F")
  )

  testthat::expect_equal(
    normalize_active_paths_with_dependencies(c("rxn_T", "rxn_D", "rxn_T", "invalid")),
    c("rxn_D", "rxn_T")
  )

  testthat::expect_equal(
    normalize_active_paths_with_dependencies(NULL),
    character(0)
  )
})

testthat::test_that("apply_path_graph_toggle_with_dependencies handles add/remove with dependency", {
  testthat::expect_equal(
    apply_path_graph_toggle_with_dependencies(c("rxn_D"), "rxn_F"),
    c("rxn_D", "rxn_F")
  )

  testthat::expect_equal(
    apply_path_graph_toggle_with_dependencies(c("rxn_D", "rxn_F", "rxn_T"), "rxn_D"),
    c("rxn_T")
  )

  testthat::expect_equal(
    apply_path_graph_toggle_with_dependencies(c("rxn_D", "rxn_F"), "rxn_F"),
    c("rxn_D")
  )

  testthat::expect_equal(
    apply_path_graph_toggle_with_dependencies(c("rxn_D"), "unknown"),
    c("rxn_D")
  )
})

testthat::test_that("same_active_paths_selection compares normalized sets", {
  testthat::expect_true(
    same_active_paths_selection(c("rxn_T", "rxn_D"), c("rxn_D", "rxn_T"))
  )

  testthat::expect_true(
    same_active_paths_selection(c("rxn_F"), c("rxn_D", "rxn_F"))
  )

  testthat::expect_false(
    same_active_paths_selection(c("rxn_T"), c("rxn_D", "rxn_T"))
  )
})

testthat::test_that("should_skip_active_paths_update honors current and last requested state", {
  # Import reset -> restore race: current still old, last requested already reset.
  # This must NOT be skipped so restore can be applied.
  testthat::expect_false(
    should_skip_active_paths_update(
      current_raw = c("rxn_D", "rxn_T"),
      last_requested = character(0),
      target = c("rxn_D", "rxn_T")
    )
  )

  # Fully in-sync state: safe to skip.
  testthat::expect_true(
    should_skip_active_paths_update(
      current_raw = c("rxn_D", "rxn_T"),
      last_requested = c("rxn_D", "rxn_T"),
      target = c("rxn_D", "rxn_T")
    )
  )

  # Missing ActivePaths should allow a real reset to empty.
  testthat::expect_false(
    should_skip_active_paths_update(
      current_raw = c("rxn_D"),
      last_requested = c("rxn_D"),
      target = character(0)
    )
  )
})

testthat::test_that("path_graph_toggle payload can drive path state updates", {
  srv <- function(input, output, session) {
    selected_paths <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$active_paths, {
      selected_paths(normalize_active_paths_with_dependencies(input$active_paths))
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$path_graph_toggle, {
      payload <- input$path_graph_toggle
      path_id <- as.character(if (is.null(payload$path_id)) "" else payload$path_id)[1]
      if (!nzchar(path_id)) return()
      selected_paths(apply_path_graph_toggle_with_dependencies(selected_paths(), path_id))
    }, ignoreInit = TRUE)
  }

  shiny::testServer(srv, {
    session$setInputs(active_paths = c("rxn_T"))
    testthat::expect_equal(selected_paths(), c("rxn_T"))

    session$setInputs(path_graph_toggle = list(path_id = "rxn_F", nonce = 1))
    testthat::expect_equal(selected_paths(), c("rxn_D", "rxn_T", "rxn_F"))

    session$setInputs(path_graph_toggle = list(path_id = "rxn_D", nonce = 2))
    testthat::expect_equal(selected_paths(), c("rxn_T"))
  })
})
