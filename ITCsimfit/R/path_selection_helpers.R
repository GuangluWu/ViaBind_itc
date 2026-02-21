# ==============================================================================
# R/path_selection_helpers.R - Step2 path selection helper functions
# ==============================================================================

ITCSIMFIT_VALID_ACTIVE_PATHS <- c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U")

normalize_active_paths_with_dependencies <- function(paths,
                                                     valid_paths = ITCSIMFIT_VALID_ACTIVE_PATHS) {
  tokens <- if (is.null(paths)) character(0) else as.character(paths)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  normalized <- valid_paths[valid_paths %in% unique(tokens)]

  # rxn_F depends on rxn_D. Keep output ordered by valid_paths.
  if ("rxn_F" %in% normalized && !"rxn_D" %in% normalized) {
    normalized <- valid_paths[valid_paths %in% c(normalized, "rxn_D")]
  }

  normalized
}

apply_path_graph_toggle_with_dependencies <- function(current_paths,
                                                      path_id,
                                                      valid_paths = ITCSIMFIT_VALID_ACTIVE_PATHS) {
  normalized_current <- normalize_active_paths_with_dependencies(
    current_paths,
    valid_paths = valid_paths
  )

  path_chr <- as.character(if (is.null(path_id)) "" else path_id)[1]
  if (!nzchar(path_chr) || !path_chr %in% valid_paths) {
    return(normalized_current)
  }

  if (path_chr %in% normalized_current) {
    next_paths <- setdiff(normalized_current, path_chr)
    if (identical(path_chr, "rxn_D")) {
      next_paths <- setdiff(next_paths, "rxn_F")
    }
  } else {
    next_paths <- c(normalized_current, path_chr)
  }

  normalize_active_paths_with_dependencies(next_paths, valid_paths = valid_paths)
}
