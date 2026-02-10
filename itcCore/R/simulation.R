solve_equi_modular <- function(G_tot, H_tot, p, active_paths, last_guess) {
  stopifnot(
    is.numeric(G_tot), G_tot >= 0,
    is.numeric(H_tot), H_tot >= 0,
    is.list(p),
    all(c("K1", "H1") %in% names(p)),
    is.numeric(last_guess), length(last_guess) >= 2
  )

  if (is_near_zero(G_tot, EPSILON)) return(c(0, H_tot, 0))
  if (is_near_zero(H_tot, EPSILON)) return(c(G_tot, 0, 0))

  solve_quadratic_M <- function(Gt, Ht, K) {
    K <- max(K, EPSILON)
    a <- K
    b <- -(K * Ht + K * Gt + 1)
    c <- K * Ht * Gt
    delta <- b^2 - 4 * a * c
    if (delta < 0) delta <- 0
    M_exact <- (-b - sqrt(delta)) / (2 * a)
    G_free <- max(0, Gt - M_exact)
    H_free <- max(0, Ht - M_exact)
    c(G_free, H_free)
  }

  anchor_guess <- solve_quadratic_M(G_tot, H_tot, p$K1)
  is_simple_model <- length(setdiff(active_paths, "rxn_M")) == 0
  if (is_simple_model) {
    return(c(anchor_guess, 0))
  }

  Ks <- c(p$K1, p$K2, p$K3, p$K4, p$K5, p$K6)
  Ks <- pmax(Ks, EPSILON)
  lnKs <- safe_log(Ks, EPSILON_LOG)
  start_log <- safe_log(anchor_guess, EPSILON_LOG)

  try_log_solve <- function() {
    eqs_log <- function(y) {
      if (any(y > 700)) return(c(1e10, 1e10))
      lnG <- y[1]
      lnH <- y[2]
      G <- exp(lnG)
      H <- exp(lnH)

      lnM <- lnKs[1] + lnH + lnG
      M <- exp(lnM)

      D <- 0
      T_val <- 0
      B <- 0
      F_val <- 0
      U <- 0

      if ("rxn_D" %in% active_paths) {
        D <- exp(lnKs[2] + lnM + lnG)
      }
      if ("rxn_T" %in% active_paths) {
        T_val <- exp(lnKs[3] + 2 * lnM)
      }
      if ("rxn_B" %in% active_paths) {
        B <- exp(lnKs[4] + lnM + lnH)
      }
      if ("rxn_F" %in% active_paths && "rxn_D" %in% active_paths) {
        F_val <- exp(lnKs[5] + lnM + safe_log(D, EPSILON_LOG))
      }
      if ("rxn_U" %in% active_paths) {
        U <- exp(lnKs[6] + lnM)
      }

      diff_G <- (G + M + U + 2 * D + 2 * T_val + B + 3 * F_val) - G_tot
      diff_H <- (H + M + U + D + 2 * T_val + 2 * B + 2 * F_val) - H_tot
      c(f1 = diff_G, f2 = diff_H)
    }

    res <- tryCatch({
      rootSolve::multiroot(f = eqs_log, start = start_log, rtol = 1e-10, atol = 1e-10, maxiter = 200)$root
    }, error = function(e) NULL)

    if (is.null(res)) return(NULL)
    c(exp(res), 0)
  }

  ans <- try_log_solve()
  if (!is.null(ans)) {
    if (ans[1] > G_tot * 1.5 || ans[2] > H_tot * 1.5) {
      return(c(anchor_guess, 1))
    }
    ans[1] <- min(ans[1], G_tot)
    ans[2] <- min(ans[2], H_tot)
    return(c(ans, 0))
  }

  c(anchor_guess, 1)
}

run_sim_modular <- function(p, active_paths) {
  if (is.null(p$V_inj_vec) && !is.null(p$V_inj) && !is.null(p$n_inj)) {
    p$V_inj_vec <- rep(p$V_inj, p$n_inj)
  }

  stopifnot(
    is.list(p),
    all(c("V_cell", "V_inj_vec", "n_inj", "H_cell_0", "G_syringe") %in% names(p))
  )

  V_cell <- p$V_cell
  n_inj <- p$n_inj
  V_inj_vec <- p$V_inj_vec
  if (length(V_inj_vec) != n_inj) {
    stop("V_inj_vec length must equal n_inj")
  }

  safe_K <- function(val) if (is.null(val) || is.na(val)) 10^DEFAULT_PARAMS$logK else 10^val
  safe_H <- function(val) if (is.null(val) || is.na(val)) DEFAULT_PARAMS$H else val

  lin_p <- list(
    K1 = safe_K(p$logK1), H1 = safe_H(p$H1),
    K2 = safe_K(p$logK2), H2 = safe_H(p$H2),
    K3 = safe_K(p$logK3), H3 = safe_H(p$H3),
    K4 = safe_K(p$logK4), H4 = safe_H(p$H4),
    K5 = safe_K(p$logK5), H5 = safe_H(p$H5),
    K6 = safe_K(p$logK6), H6 = safe_H(p$H6)
  )

  dilution_factor_vec <- (V_cell - V_inj_vec) / V_cell
  injection_contribution_vec <- p$G_syringe * V_inj_vec / V_cell

  G_t_real_vec <- numeric(n_inj)
  H_t_real_vec <- numeric(n_inj)
  G_t_real_vec[1] <- injection_contribution_vec[1]
  H_t_real_vec[1] <- p$H_cell_0 * dilution_factor_vec[1]
  if (n_inj >= 2) {
    for (i in 2:n_inj) {
      G_t_real_vec[i] <- G_t_real_vec[i - 1] * dilution_factor_vec[i] + injection_contribution_vec[i]
      H_t_real_vec[i] <- H_t_real_vec[i - 1] * dilution_factor_vec[i]
    }
  }

  G_free_vec <- numeric(n_inj)
  H_free_vec <- numeric(n_inj)
  is_fb_vec <- integer(n_inj)
  M_vec <- numeric(n_inj)
  D_vec <- numeric(n_inj)
  T_vec <- numeric(n_inj)
  B_vec <- numeric(n_inj)
  F_vec <- numeric(n_inj)
  U_vec <- numeric(n_inj)
  Q_total_vec <- numeric(n_inj)

  current_guess <- c(EPSILON_LOG, p$H_cell_0)
  is_simple_model <- length(setdiff(active_paths, "rxn_M")) == 0

  if (is_simple_model) {
    K1 <- lin_p$K1
    for (i in seq_len(n_inj)) {
      Gt <- G_t_real_vec[i]
      Ht <- H_t_real_vec[i]
      K <- max(K1, EPSILON)
      a <- K
      b <- -(K * Ht + K * Gt + 1)
      c0 <- K * Ht * Gt
      delta <- b^2 - 4 * a * c0
      if (delta < 0) delta <- 0
      M_exact <- (-b - sqrt(delta)) / (2 * a)

      G_free_vec[i] <- max(0, Gt - M_exact)
      H_free_vec[i] <- max(0, Ht - M_exact)
      M_vec[i] <- M_exact
      Q_total_vec[i] <- V_cell * M_vec[i] * lin_p$H1
    }
  } else {
    for (i in seq_len(n_inj)) {
      sol <- solve_equi_modular(G_t_real_vec[i], H_t_real_vec[i], lin_p, active_paths, current_guess)
      if (any(is.na(sol))) return(NULL)

      current_guess <- sol[1:2]
      G_free_vec[i] <- sol[1]
      H_free_vec[i] <- sol[2]
      is_fb_vec[i] <- if (length(sol) > 2) sol[3] else 0

      M_vec[i] <- lin_p$K1 * H_free_vec[i] * G_free_vec[i]
      if ("rxn_D" %in% active_paths) D_vec[i] <- lin_p$K2 * M_vec[i] * G_free_vec[i]
      if ("rxn_T" %in% active_paths) T_vec[i] <- lin_p$K3 * M_vec[i]^2
      if ("rxn_B" %in% active_paths) B_vec[i] <- lin_p$K4 * M_vec[i] * H_free_vec[i]
      if ("rxn_F" %in% active_paths && "rxn_D" %in% active_paths) F_vec[i] <- lin_p$K5 * M_vec[i] * D_vec[i]
      if ("rxn_U" %in% active_paths) U_vec[i] <- lin_p$K6 * M_vec[i]

      Q_sum <- M_vec[i] * lin_p$H1
      if ("rxn_D" %in% active_paths && D_vec[i] > 0) Q_sum <- Q_sum + D_vec[i] * (lin_p$H1 + lin_p$H2)
      if ("rxn_T" %in% active_paths && T_vec[i] > 0) Q_sum <- Q_sum + T_vec[i] * (2 * lin_p$H1 + lin_p$H3)
      if ("rxn_B" %in% active_paths && B_vec[i] > 0) Q_sum <- Q_sum + B_vec[i] * (lin_p$H1 + lin_p$H4)
      if ("rxn_F" %in% active_paths && "rxn_D" %in% active_paths && F_vec[i] > 0) Q_sum <- Q_sum + F_vec[i] * (2 * lin_p$H1 + lin_p$H2 + lin_p$H5)
      if ("rxn_U" %in% active_paths && U_vec[i] > 0) Q_sum <- Q_sum + U_vec[i] * (lin_p$H1 + lin_p$H6)

      Q_total_vec[i] <- V_cell * Q_sum
    }
  }

  Q_prev_vec <- c(0, Q_total_vec[-n_inj] * dilution_factor_vec[-1])
  dQ_heat_vec <- Q_total_vec - Q_prev_vec
  dQ_norm_vec <- dQ_heat_vec / (p$G_syringe * V_inj_vec)

  data.frame(
    Inj = seq_len(n_inj),
    Ratio = G_t_real_vec / H_t_real_vec,
    dQ = dQ_norm_vec,
    H_pct = H_free_vec / H_t_real_vec,
    M_pct = M_vec / H_t_real_vec,
    D_pct = D_vec / H_t_real_vec,
    T_pct = (2 * T_vec) / H_t_real_vec,
    B_pct = (2 * B_vec) / H_t_real_vec,
    F_pct = (2 * F_vec) / H_t_real_vec,
    U_pct = U_vec / H_t_real_vec,
    Fallback = is_fb_vec
  )
}

calculate_simulation <- function(p, active_paths) {
  if (is.null(p) || !is.list(p)) {
    stop("Parameter p must be a non-null list")
  }
  if (is.null(active_paths) || length(active_paths) == 0) {
    active_paths <- character(0)
  }
  if (is.null(p$V_inj_vec) && !is.null(p$V_inj) && !is.null(p$n_inj)) {
    p$V_inj_vec <- rep(p$V_inj, p$n_inj)
  }

  p_true <- p
  p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-3
  p_true$G_syringe <- p$G_syringe * p$fG * 1e-3
  p_true$V_inj_vec <- p$V_inj_vec / 1000

  paths <- if (length(active_paths) == 0) "rxn_M" else unique(c("rxn_M", active_paths))
  res_raw <- run_sim_modular(p_true, paths)
  if (is.null(res_raw)) return(NULL)

  res_raw$Ratio_App <- res_raw$Ratio * (p$fH / p$fG)
  res_raw$dQ_App <- (res_raw$dQ * p$fG) + p$Offset
  res_raw
}

calculate_ratio_app <- function(p) {
  if (is.null(p) || !is.list(p)) {
    stop("Parameter p must be a non-null list")
  }
  if (is.null(p$V_inj_vec) && !is.null(p$V_inj) && !is.null(p$n_inj)) {
    p$V_inj_vec <- rep(p$V_inj, p$n_inj)
  }
  if (is.null(p$V_inj_vec) || length(p$V_inj_vec) == 0) {
    return(numeric(0))
  }

  n_inj <- if (!is.null(p$n_inj) && is.finite(p$n_inj)) p$n_inj else length(p$V_inj_vec)
  if (length(p$V_inj_vec) != n_inj) {
    n_inj <- length(p$V_inj_vec)
  }

  p_true <- p
  p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-3
  p_true$G_syringe <- p$G_syringe * p$fG * 1e-3
  V_inj_vec <- p$V_inj_vec / 1000
  V_cell <- p$V_cell

  dilution_factor_vec <- (V_cell - V_inj_vec) / V_cell
  injection_contribution_vec <- p_true$G_syringe * V_inj_vec / V_cell

  G_t_real_vec <- numeric(n_inj)
  H_t_real_vec <- numeric(n_inj)

  H_t_real_vec[1] <- p_true$H_cell_0 * dilution_factor_vec[1]
  G_t_real_vec[1] <- injection_contribution_vec[1]

  if (n_inj >= 2) {
    for (i in 2:n_inj) {
      G_t_real_vec[i] <- G_t_real_vec[i - 1] * dilution_factor_vec[i] + injection_contribution_vec[i]
      H_t_real_vec[i] <- H_t_real_vec[i - 1] * dilution_factor_vec[i]
    }
  }

  ratio_true_vec <- G_t_real_vec / H_t_real_vec
  ratio_true_vec * (p$fH / p$fG)
}
