# ==============================================================================
# species_dist_helpers.R - Step2 species distribution data/plot helpers
# ==============================================================================

normalize_species_dist_active_paths <- function(active_paths = character(0)) {
  if (exists("normalize_active_paths_with_dependencies", mode = "function", inherits = TRUE)) {
    return(tryCatch(
      normalize_active_paths_with_dependencies(active_paths),
      error = function(e) active_paths
    ))
  }

  paths_raw <- if (is.null(active_paths)) character(0) else active_paths
  paths_raw <- unique(trimws(as.character(paths_raw)))
  paths_raw <- setdiff(paths_raw[nzchar(paths_raw)], "rxn_M")
  valid_paths <- c("rxn_D", "rxn_T", "rxn_E", "rxn_B", "rxn_F", "rxn_U")
  paths_norm <- valid_paths[valid_paths %in% paths_raw]
  if ("rxn_E" %in% paths_norm && !"rxn_T" %in% paths_norm) {
    paths_norm <- valid_paths[valid_paths %in% c(paths_norm, "rxn_T")]
  }
  if ("rxn_F" %in% paths_norm && !"rxn_D" %in% paths_norm) {
    paths_norm <- valid_paths[valid_paths %in% c(paths_norm, "rxn_D")]
  }
  paths_norm
}

resolve_species_dist_columns <- function(active_paths = character(0), available_cols = NULL) {
  active_paths_norm <- normalize_species_dist_active_paths(active_paths)
  species_map <- c(
    H_pct = "H",
    M_pct = "H1G1",
    D_pct = "H1G2",
    T_pct = "H2G2",
    E_pct = "H3G2",
    B_pct = "H2G1",
    F_pct = "H2G3",
    U_pct = "H1G1(U)"
  )

  selected <- c("H_pct", "M_pct")
  if ("rxn_D" %in% active_paths_norm) selected <- c(selected, "D_pct")
  if ("rxn_T" %in% active_paths_norm) selected <- c(selected, "T_pct")
  if ("rxn_E" %in% active_paths_norm) selected <- c(selected, "E_pct")
  if ("rxn_B" %in% active_paths_norm) selected <- c(selected, "B_pct")
  if ("rxn_F" %in% active_paths_norm) selected <- c(selected, "F_pct")
  if ("rxn_U" %in% active_paths_norm) selected <- c(selected, "U_pct")

  if (!is.null(available_cols)) {
    selected <- selected[selected %in% available_cols]
  }

  stats::setNames(unname(species_map[selected]), selected)
}

build_species_dist_export_df <- function(sim, active_paths = character(0)) {
  if (is.null(sim) || !is.data.frame(sim) || nrow(sim) < 1L) return(NULL)

  sim_df <- as.data.frame(sim, stringsAsFactors = FALSE, check.names = FALSE)
  species_cols <- resolve_species_dist_columns(
    active_paths = active_paths,
    available_cols = colnames(sim_df)
  )
  if (length(species_cols) < 1L) return(NULL)

  ratio_vals <- if ("Ratio_App" %in% colnames(sim_df)) {
    sim_df$Ratio_App
  } else if ("Ratio" %in% colnames(sim_df)) {
    sim_df$Ratio
  } else {
    rep(NA_real_, nrow(sim_df))
  }

  out <- data.frame(
    Injection = if ("Inj" %in% colnames(sim_df)) sim_df$Inj else seq_len(nrow(sim_df)),
    Simulated_GH_Ratio = ratio_vals,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (src_col in names(species_cols)) {
    out[[species_cols[[src_col]]]] <- sim_df[[src_col]]
  }

  out
}

build_species_dist_plot_data <- function(sim, active_paths = character(0)) {
  export_df <- build_species_dist_export_df(sim = sim, active_paths = active_paths)
  if (is.null(export_df) || nrow(export_df) < 1L) return(NULL)

  species_levels <- setdiff(colnames(export_df), c("Injection", "Simulated_GH_Ratio"))
  if (length(species_levels) < 1L) return(NULL)

  n_rows <- nrow(export_df)
  plot_df <- data.frame(
    Simulated_GH_Ratio = rep(export_df$Simulated_GH_Ratio, times = length(species_levels)),
    Species = factor(rep(species_levels, each = n_rows), levels = species_levels),
    Fraction = unlist(export_df[species_levels], use.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  plot_df <- plot_df[order(plot_df$Species, plot_df$Simulated_GH_Ratio), , drop = FALSE]
  rownames(plot_df) <- NULL
  plot_df
}

species_dist_tr <- function(key, lang = "zh", zh_default = "", en_default = "") {
  translated <- tryCatch({
    if (!exists("tr", mode = "function", inherits = TRUE)) return(NA_character_)
    tr(key, lang)
  }, error = function(e) NA_character_)
  translated <- as.character(translated)[1]
  if (!is.na(translated) && nzchar(trimws(translated)) && !identical(translated, key)) {
    return(translated)
  }
  if (identical(lang, "zh")) zh_default else en_default
}

build_species_dist_plot <- function(sim, active_paths = character(0), lang = "zh", base_size = 14) {
  plot_df <- build_species_dist_plot_data(sim = sim, active_paths = active_paths)
  if (is.null(plot_df) || nrow(plot_df) < 1L) return(NULL)

  legend_species <- species_dist_tr("legend_species", lang, zh_default = "物种", en_default = "Species")
  x_label <- species_dist_tr("axis_simulated_ratio", lang, zh_default = "模拟的 G/H 比例", en_default = "Simulated G/H Ratio")
  y_label <- species_dist_tr("axis_fraction_based_on_h", lang, zh_default = "物种分布（基于 H）", en_default = "Fraction (based on H)")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Simulated_GH_Ratio, y = Fraction, fill = Species)) +
    ggplot2::geom_area(alpha = 0.8, color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_brewer(palette = "Set3", name = legend_species) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")
}
