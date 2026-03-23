# ==============================================================================
# R/path_selection_helpers.R - Step2 path selection helper functions
# ==============================================================================

ITCSIMFIT_VALID_ACTIVE_PATHS <- c("rxn_D", "rxn_T", "rxn_E", "rxn_B", "rxn_F", "rxn_U")
ITCSIMFIT_ACTIVE_PATH_DEPENDENCIES <- c(
  rxn_E = "rxn_T",
  rxn_F = "rxn_D"
)

normalize_active_paths_with_dependencies <- function(paths,
                                                     valid_paths = ITCSIMFIT_VALID_ACTIVE_PATHS) {
  tokens <- if (is.null(paths)) character(0) else as.character(paths)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  normalized <- valid_paths[valid_paths %in% unique(tokens)]

  dependency_map <- ITCSIMFIT_ACTIVE_PATH_DEPENDENCIES[
    names(ITCSIMFIT_ACTIVE_PATH_DEPENDENCIES) %in% valid_paths
  ]

  # Keep output ordered by valid_paths while recursively filling required parents.
  changed <- TRUE
  while (isTRUE(changed)) {
    changed <- FALSE
    for (child in names(dependency_map)) {
      parent <- as.character(dependency_map[[child]])[1]
      if (child %in% normalized && !parent %in% normalized) {
        normalized <- valid_paths[valid_paths %in% c(normalized, parent)]
        changed <- TRUE
      }
    }
  }

  normalized
}

same_active_paths_selection <- function(a,
                                        b,
                                        valid_paths = ITCSIMFIT_VALID_ACTIVE_PATHS) {
  a_norm <- normalize_active_paths_with_dependencies(a, valid_paths = valid_paths)
  b_norm <- normalize_active_paths_with_dependencies(b, valid_paths = valid_paths)
  setequal(a_norm, b_norm) && length(a_norm) == length(b_norm)
}

should_skip_active_paths_update <- function(current_raw,
                                            last_requested,
                                            target,
                                            valid_paths = ITCSIMFIT_VALID_ACTIVE_PATHS) {
  same_active_paths_selection(current_raw, target, valid_paths = valid_paths) &&
    same_active_paths_selection(last_requested, target, valid_paths = valid_paths)
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

  dependency_map <- ITCSIMFIT_ACTIVE_PATH_DEPENDENCIES[
    names(ITCSIMFIT_ACTIVE_PATH_DEPENDENCIES) %in% valid_paths
  ]
  drop_path_and_dependents <- function(paths, removed_id) {
    out <- unique(as.character(paths))
    queue <- as.character(removed_id)[1]

    while (length(queue) > 0) {
      current_id <- queue[1]
      queue <- queue[-1]
      out <- setdiff(out, current_id)
      child_ids <- names(dependency_map)[as.character(dependency_map) == current_id]
      child_ids <- child_ids[child_ids %in% out]
      if (length(child_ids) > 0) {
        queue <- unique(c(queue, child_ids))
      }
    }

    out
  }

  if (path_chr %in% normalized_current) {
    next_paths <- drop_path_and_dependents(normalized_current, path_chr)
  } else {
    next_paths <- c(normalized_current, path_chr)
  }

  normalize_active_paths_with_dependencies(next_paths, valid_paths = valid_paths)
}
