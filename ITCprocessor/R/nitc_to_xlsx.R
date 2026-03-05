#!/usr/bin/env Rscript

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Missing package '%s'. Install with: install.packages('%s')", pkg, pkg),
      call. = FALSE
    )
  }
}

require_pkg("writexl")

suppressPackageStartupMessages({
  library(writexl)
})

parse_cli <- function(argv) {
  if (length(argv) == 0) {
    stop("Usage: Rscript nitc_to_xlsx.R <input.nitc> [-o <out.xlsx>]", call. = FALSE)
  }
  input <- NULL
  out_xlsx <- NULL
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[[i]]
    if (a %in% c("-o", "--out")) {
      if (i == length(argv)) {
        stop("Missing value after -o/--out", call. = FALSE)
      }
      out_xlsx <- argv[[i + 1L]]
      i <- i + 2L
      next
    }
    if (startsWith(a, "-")) {
      stop(sprintf("Unknown option: %s", a), call. = FALSE)
    }
    if (!is.null(input)) {
      stop("Only one input NITC file is supported", call. = FALSE)
    }
    input <- a
    i <- i + 1L
  }
  if (is.null(input)) {
    stop("Input NITC path is required", call. = FALSE)
  }
  list(input = input, out_xlsx = out_xlsx)
}

read_blob <- function(path) {
  size <- file.info(path)$size
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = size)
}

load_nitc_payload <- function(nitc_path) {
  raw <- read_blob(nitc_path)
  dec <- tryCatch(
    memDecompress(raw, type = "gzip"),
    error = function(e) e
  )
  if (inherits(dec, "error")) {
    stop(
      sprintf("Input is not a valid gzip-compressed NITC file: %s", nitc_path),
      call. = FALSE
    )
  }
  dec
}

read_doubles_by_phase <- function(payload, phase) {
  start <- phase + 1L
  available <- length(payload) - start + 1L
  count <- available %/% 8L
  if (count <= 0L) {
    return(numeric())
  }
  bytes <- payload[start:(start + count * 8L - 1L)]
  con <- rawConnection(bytes, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "double", n = count, size = 8L, endian = "little")
}

is_intish <- function(v, tol = 1e-9) {
  is.finite(v) && abs(v - round(v)) < tol
}

scan_numeric_series <- function(payload, min_len = 500L, abs_min = 1e-9) {
  series <- list()
  for (phase in 0:7) {
    values <- read_doubles_by_phase(payload, phase)
    if (length(values) == 0) {
      next
    }

    run_start <- NA_integer_
    flush_run <- function(end_idx) {
      if (is.na(run_start) || end_idx < run_start) {
        return()
      }
      run_vals <- values[run_start:end_idx]
      if (length(run_vals) < min_len) {
        return()
      }
      head_vals <- run_vals[seq_len(min(2000L, length(run_vals)))]
      uniq <- length(unique(round(head_vals, 7)))
      if (uniq <= 10) {
        return()
      }
      series[[length(series) + 1L]] <<- list(
        phase = phase,
        start = phase + (run_start - 1L) * 8L,
        len = length(run_vals),
        values = run_vals
      )
    }

    for (i in seq_along(values)) {
      v <- values[[i]]
      plausible <- is.finite(v) && v >= -1e5 && v <= 1e5 && (v == 0 || abs(v) >= abs_min)
      if (plausible) {
        if (is.na(run_start)) {
          run_start <- i
        }
      } else {
        flush_run(i - 1L)
        run_start <- NA_integer_
      }
    }
    if (!is.na(run_start)) {
      flush_run(length(values))
    }
  }
  series
}

is_index_axis <- function(values) {
  if (length(values) < 100L) {
    return(FALSE)
  }
  check <- values[seq_len(min(length(values), 4500L))]
  diffs <- diff(check)
  finite <- is.finite(check[-1]) & is.finite(check[-length(check)])
  if (!any(finite)) {
    return(FALSE)
  }
  unit_like <- abs(diffs[finite] - 1.0) < 1e-6
  good_ratio <- mean(unit_like)
  mono_ratio <- mean(diffs[finite] > 0)
  starts_on_integer_grid <- is.finite(check[[1]]) && abs(check[[1]] - round(check[[1]])) < 1e-3
  starts_on_integer_grid && good_ratio > 0.98 && mono_ratio > 0.995
}

pick_best_axis_idx <- function(series, axis_idx) {
  if (length(axis_idx) == 0L) {
    return(NA_integer_)
  }
  if (length(axis_idx) == 1L) {
    return(axis_idx[[1]])
  }
  lens <- vapply(series, function(s) s$len, numeric(1))
  starts <- vapply(series, function(s) s$start, numeric(1))
  peer_cnt <- vapply(axis_idx, function(i) as.integer(sum(lens == lens[[i]]) - 1L), integer(1))
  ord <- order(-peer_cnt, -lens[axis_idx], starts[axis_idx])
  axis_idx[[ord[[1]]]]
}

dedup_seq_by_key <- function(seq_list, key_fn) {
  if (length(seq_list) == 0) {
    return(seq_list)
  }
  ord <- order(vapply(seq_list, function(x) x$start, numeric(1)))
  seq_list <- seq_list[ord]
  seen <- character()
  out <- list()
  for (item in seq_list) {
    key <- key_fn(item)
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- item
  }
  out
}

find_step200_sequences <- function(data, min_len = 6L) {
  out <- list()
  tol <- 1e-9
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < min_len) {
      next
    }
    i <- 1L
    while (i < length(vals)) {
      a <- vals[[i]]
      b <- vals[[i + 1L]]
      if (!(is_intish(a, tol) && is_intish(b, tol))) {
        i <- i + 1L
        next
      }
      d <- b - a
      if (!is.finite(d) || abs(d - 200.0) > tol) {
        i <- i + 1L
        next
      }

      j <- i + 1L
      while (j < length(vals)) {
        x1 <- vals[[j]]
        x2 <- vals[[j + 1L]]
        if (!(is_intish(x1, tol) && is_intish(x2, tol))) {
          break
        }
        dd <- x2 - x1
        if (!is.finite(dd) || abs(dd - 200.0) > tol) {
          break
        }
        j <- j + 1L
      }

      run_vals <- as.numeric(round(vals[i:j]))
      run_len <- length(run_vals)
      if (!(run_len >= min_len && run_vals[[1]] >= 0 && run_vals[[run_len]] >= run_vals[[1]])) {
        i <- j + 1L
        next
      }

      out[[length(out) + 1L]] <- list(
        phase = phase,
        start = phase + (i - 1L) * 8L,
        len = run_len,
        step = 200.0,
        values = run_vals
      )

      i <- j + 1L
    }
  }
  dedup_seq_by_key(out, function(x) paste(format(round(x$values, 9), scientific = FALSE, trim = TRUE), collapse = ","))
}

find_arithmetic_int_sequences <- function(data, min_len = 8L, step_min = 1L, step_max = 10000L) {
  out <- list()
  tol <- 1e-9

  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < min_len) {
      next
    }

    i <- 1L
    while (i < length(vals)) {
      v1 <- vals[[i]]
      v2 <- vals[[i + 1L]]
      if (!(is_intish(v1, tol) && is_intish(v2, tol))) {
        i <- i + 1L
        next
      }

      d_raw <- v2 - v1
      d <- round(d_raw)
      if (!is.finite(d) || abs(d) > 1e6) {
        i <- i + 1L
        next
      }
      if (d < step_min || d > step_max || abs(d_raw - d) > tol) {
        i <- i + 1L
        next
      }

      j <- i + 1L
      while (j < length(vals)) {
        a <- vals[[j]]
        b <- vals[[j + 1L]]
        if (!(is_intish(a, tol) && is_intish(b, tol))) {
          break
        }
        dd <- b - a
        if (!is.finite(dd)) {
          break
        }
        if (abs(dd - d) > tol) {
          break
        }
        j <- j + 1L
      }

      run_vals <- as.numeric(round(vals[i:j]))
      run_len <- length(run_vals)
      if (run_len >= min_len && run_vals[[1]] >= 0 && run_vals[[run_len]] >= run_vals[[1]]) {
        out[[length(out) + 1L]] <- list(
          phase = phase,
          start = phase + (i - 1L) * 8L,
          step = d,
          len = run_len,
          values = run_vals
        )
      }

      i <- j + 1L
    }
  }

  dedup_seq_by_key(out, function(x) paste(c(x$step, x$values), collapse = ","))
}

find_monotonic_int_sequences <- function(data, n = 8L) {
  out <- list()
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < n) {
      next
    }
    for (start_idx in seq_len(length(vals) - n + 1L)) {
      s <- vals[start_idx:(start_idx + n - 1L)]
      if (!all(is.finite(s))) {
        next
      }
      if (!all(vapply(s, is_intish, logical(1)))) {
        next
      }
      iv <- as.numeric(round(s))
      if (iv[[1]] < 0 || iv[[n]] > 10000) {
        next
      }
      d <- diff(iv)
      if (min(d) < 0) {
        next
      }
      if (sum(d > 0) < (n - 2L)) {
        next
      }
      out[[length(out) + 1L]] <- list(
        phase = phase,
        start = phase + (start_idx - 1L) * 8L,
        values = iv
      )
    }
  }
  dedup_seq_by_key(out, function(x) paste(x$values, collapse = ","))
}

find_constant_sequences <- function(data, target, n = 6L) {
  out <- list()
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < n) {
      next
    }
    for (start_idx in seq_len(length(vals) - n + 1L)) {
      s <- vals[start_idx:(start_idx + n - 1L)]
      if (!all(is.finite(s))) {
        next
      }
      if (all(abs(s - target) < 1e-12)) {
        out[[length(out) + 1L]] <- list(
          phase = phase,
          start = phase + (start_idx - 1L) * 8L,
          values = s
        )
      }
    }
  }
  dedup_seq_by_key(out, function(x) paste(format(round(x$values, 12), scientific = FALSE, trim = TRUE), collapse = ","))
}

score_widths <- function(widths, min_coverage = 0.6) {
  valid <- is.finite(widths) & widths > 0
  n_valid <- sum(valid)
  needed <- max(1L, ceiling(length(widths) * min_coverage))
  if (n_valid < needed) {
    return(list(score = -Inf, n_valid = n_valid, cv = Inf))
  }
  w <- widths[valid]
  med <- stats::median(w)
  cv <- if (length(w) >= 2L && is.finite(med) && med > 0) stats::sd(w) / med else 0
  score <- n_valid - min(cv, 5)
  list(score = score, n_valid = n_valid, cv = cv)
}

derive_step_sequence_pair <- function(step_sequences) {
  if (is.null(step_sequences) || length(step_sequences) == 0) {
    return(list(start = NULL, original_stop = NULL, source = "none"))
  }

  seq_vals <- lapply(step_sequences, function(x) as.numeric(x$values))
  seq_vals <- seq_vals[vapply(seq_vals, function(v) length(v) > 0L && all(is.finite(v)), logical(1))]
  if (length(seq_vals) == 0) {
    return(list(start = NULL, original_stop = NULL, source = "none"))
  }

  best <- NULL
  best_score <- -Inf
  for (i in seq_along(seq_vals)) {
    for (j in seq_along(seq_vals)) {
      if (j <= i) next
      a <- seq_vals[[i]]
      b <- seq_vals[[j]]
      if (length(a) != length(b)) next
      if (all(a > b)) {
        tmp <- a
        a <- b
        b <- tmp
      }
      if (!all(b > a)) next

      widths <- b - a
      ws <- score_widths(widths, min_coverage = 0.6)
      if (!is.finite(ws$score)) next
      score <- ws$score
      if (is.finite(ws$cv) && ws$cv < 1e-9) score <- score + 4
      score <- score + length(a) * 0.5

      better <- FALSE
      if (score > best_score) {
        better <- TRUE
      } else if (!is.null(best) && abs(score - best_score) < 1e-9) {
        if (a[[1]] < best$start[[1]]) {
          better <- TRUE
        } else if (a[[1]] == best$start[[1]] && length(a) > length(best$start)) {
          better <- TRUE
        }
      }
      if (better) {
        best_score <- score
        best <- list(start = a, original_stop = b, source = "step_pair")
      }
    }
  }

  if (!is.null(best)) return(best)

  firsts <- vapply(seq_vals, function(v) v[[1]], numeric(1))
  lens <- vapply(seq_vals, length, integer(1))
  ord <- order(firsts, -lens)
  start <- seq_vals[[ord[[1]]]]
  step_guess <- suppressWarnings(stats::median(diff(start)))
  if (!is.finite(step_guess) || step_guess <= 0) step_guess <- 200
  list(
    start = start,
    original_stop = start + step_guess,
    source = "step_derived"
  )
}

is_sequence_extension <- function(candidate, base, tol = 1e-9) {
  if (is.null(base)) {
    return(TRUE)
  }
  if (length(candidate) < length(base)) {
    return(FALSE)
  }
  all(abs(candidate[seq_len(length(base))] - base) <= tol)
}

select_start_stop <- function(step200, mono_int = NULL, arith = NULL) {
  step_pick <- derive_step_sequence_pair(step200)
  preferred_start <- step_pick$start
  preferred_original_stop <- step_pick$original_stop

  if (!is.null(arith) && length(arith) > 0L) {
    cand_idx <- seq_along(arith)
    if (!is.null(preferred_start)) {
      compat <- which(vapply(
        arith,
        function(x) is_sequence_extension(as.numeric(x$values), preferred_start),
        logical(1)
      ))
      cand_idx <- compat
    }
    if (length(cand_idx) > 0L) {
      lens <- vapply(arith[cand_idx], function(x) x$len, numeric(1))
      starts <- vapply(arith[cand_idx], function(x) x$values[[1]], numeric(1))
      steps <- vapply(arith[cand_idx], function(x) x$step, numeric(1))
      pref_step <- if (!is.null(preferred_start) && length(preferred_start) >= 2L) {
        suppressWarnings(stats::median(diff(preferred_start)))
      } else {
        NA_real_
      }
      step_delta <- if (is.finite(pref_step)) abs(steps - pref_step) else abs(steps - 200.0)
      ord <- order(-lens, step_delta, starts)
      best_arith <- arith[[cand_idx[[ord[[1]]]]]]

      start <- as.numeric(best_arith$values)
      stop <- start + as.numeric(best_arith$step)
      source <- "arithmetic_derived"

      same_shape_idx <- which(vapply(
        arith,
        function(x) x$len == best_arith$len && x$step == best_arith$step,
        logical(1)
      ))
      if (length(same_shape_idx) > 0L) {
        best_pair <- NULL
        best_pair_score <- -Inf
        for (i in same_shape_idx) {
          cand <- as.numeric(arith[[i]]$values)
          if (all(cand == start) || !all(cand > start)) {
            next
          }
          widths <- cand - start
          ws <- score_widths(widths, min_coverage = 0.6)
          if (!is.finite(ws$score)) {
            next
          }
          score <- ws$score + length(start) * 0.25
          if (is.finite(ws$cv) && ws$cv < 1e-9) {
            score <- score + 1
          }
          if (score > best_pair_score) {
            best_pair_score <- score
            best_pair <- cand
          }
        }
        if (!is.null(best_pair) && best_pair_score > 0) {
          stop <- best_pair
          source <- "arithmetic_pair"
        }
      }

      return(list(
        start = start,
        stop = stop,
        original_stop = stop,
        source = source
      ))
    }
  }

  if (is.null(preferred_start)) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }
  start <- preferred_start

  best_stop <- NULL
  best_score <- -Inf
  best_support <- 0L
  if (!is.null(mono_int)) {
    for (cand_item in mono_int) {
      cand <- cand_item$values
      if (length(cand) != length(start)) {
        next
      }
      if (all(cand == start)) {
        next
      }
      widths <- cand - start
      ws <- score_widths(widths, min_coverage = 0.5)
      if (!is.finite(ws$score)) {
        next
      }
      score <- ws$score
      if (length(unique(round(widths[is.finite(widths)], 6))) > 5) {
        score <- score + 2L
      }
      support <- as.integer(ws$n_valid)
      if (support > best_support || (support == best_support && score > best_score)) {
        best_support <- support
        best_score <- score
        best_stop <- cand
      }
    }
  }

  if (!is.null(best_stop) && best_support >= max(3L, ceiling(length(start) * 0.35))) {
    orig <- if (!is.null(preferred_original_stop)) preferred_original_stop else (start + 200.0)
    return(list(start = start, stop = best_stop, original_stop = orig, source = "monotonic_int_match"))
  }

  if (!is.null(preferred_original_stop)) {
    return(list(start = start, stop = preferred_original_stop, original_stop = preferred_original_stop, source = "step200_fallback"))
  }
  derived <- start + 200.0
  list(start = start, stop = derived, original_stop = derived, source = "derived_plus_200")
}

select_from_monotonic_sequences <- function(mono_int) {
  if (is.null(mono_int) || length(mono_int) == 0L) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }
  lens <- vapply(mono_int, function(x) length(x$values), integer(1))
  starts <- vapply(mono_int, function(x) x$values[[1]], numeric(1))
  ord <- order(-lens, starts)
  best <- mono_int[[ord[[1]]]]
  start <- as.numeric(best$values)
  ds <- diff(start)
  ds <- ds[is.finite(ds) & ds > 0]
  step <- if (length(ds) > 0L) stats::median(ds) else 200.0
  stop <- start + step
  list(start = start, stop = stop, original_stop = stop, source = "monotonic_derived")
}

find_constant_run <- function(data, target, tol = 1e-12, min_len = 6L) {
  best <- NULL
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) == 0) {
      next
    }
    i <- 1L
    while (i <= length(vals)) {
      if (!(is.finite(vals[[i]]) && abs(vals[[i]] - target) < tol)) {
        i <- i + 1L
        next
      }
      j <- i
      while (j < length(vals) && is.finite(vals[[j + 1L]]) && abs(vals[[j + 1L]] - target) < tol) {
        j <- j + 1L
      }
      run_len <- j - i + 1L
      if (run_len >= min_len) {
        run_vals <- vals[i:j]
        if (is.null(best) || run_len > best$len) {
          best <- list(
            phase = phase,
            start = phase + (i - 1L) * 8L,
            len = run_len,
            values = run_vals
          )
        }
      }
      i <- j + 1L
    }
  }
  best
}

find_best_volume_run <- function(
  data,
  expected_len = NA_integer_,
  preferred_targets = numeric(),
  tol = 1e-12,
  min_len = 3L,
  value_min = 0.01,
  value_max = 100.0
) {
  min_len <- max(1L, as.integer(min_len))
  expected_len <- suppressWarnings(as.integer(expected_len))
  has_expected <- is.finite(expected_len) && expected_len > 0L

  candidates <- list()
  add_candidate <- function(item, preferred = FALSE) {
    if (is.null(item) || is.null(item$values) || length(item$values) < min_len) {
      return()
    }
    v <- suppressWarnings(as.numeric(item$values[[1]]))
    if (!is.finite(v) || v <= 0 || v < value_min || v > value_max) {
      return()
    }
    candidates[[length(candidates) + 1L]] <<- list(
      phase = item$phase,
      start = item$start,
      len = as.integer(length(item$values)),
      value = v,
      values = as.numeric(item$values),
      preferred = isTRUE(preferred)
    )
  }

  pref <- suppressWarnings(as.numeric(preferred_targets))
  pref <- unique(pref[is.finite(pref) & pref > 0 & pref >= value_min & pref <= value_max])
  if (length(pref) > 0L) {
    for (target in pref) {
      run <- find_constant_run(data, target = target, tol = tol, min_len = min_len)
      if (!is.null(run)) {
        add_candidate(run, preferred = TRUE)
      }
    }
  }

  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) == 0L) {
      next
    }
    i <- 1L
    while (i <= length(vals)) {
      v <- vals[[i]]
      if (!(is.finite(v) && v > 0 && v >= value_min && v <= value_max)) {
        i <- i + 1L
        next
      }
      j <- i
      while (j < length(vals) && is.finite(vals[[j + 1L]]) && abs(vals[[j + 1L]] - v) < tol) {
        j <- j + 1L
      }
      run_len <- j - i + 1L
      if (run_len >= min_len) {
        add_candidate(
          list(
            phase = phase,
            start = phase + (i - 1L) * 8L,
            values = vals[i:j]
          ),
          preferred = FALSE
        )
      }
      i <- j + 1L
    }
  }

  if (length(candidates) == 0L) {
    return(NULL)
  }

  lens <- vapply(candidates, function(x) as.numeric(x$len), numeric(1))
  starts <- vapply(candidates, function(x) as.numeric(x$start), numeric(1))
  preferred_flag <- vapply(candidates, function(x) isTRUE(x$preferred), logical(1))
  len_delta <- if (has_expected) abs(lens - expected_len) else rep(0, length(lens))
  score <- lens - 2 * len_delta + ifelse(preferred_flag, 3, 0)
  ord <- order(-score, -as.integer(preferred_flag), len_delta, -lens, starts)
  candidates[[ord[[1]]]]
}

build_injection_df <- function(starts, stops, original_stops, volumes = NULL) {
  n <- min(length(starts), length(stops), length(original_stops))
  if (n <= 0) {
    return(data.frame(
      inj_num = integer(),
      start_s = numeric(),
      stop_s = numeric(),
      width_s = numeric(),
      original_start_s = numeric(),
      original_stop_s = numeric(),
      original_width_s = numeric(),
      inj_volume_uL = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  inj_vol <- rep(NA_real_, n)
  if (!is.null(volumes) && length(volumes) > 0L) {
    m <- min(n, length(volumes))
    inj_vol[seq_len(m)] <- as.numeric(volumes[seq_len(m)])
  }
  data.frame(
    inj_num = seq_len(n),
    start_s = starts[seq_len(n)],
    stop_s = stops[seq_len(n)],
    width_s = stops[seq_len(n)] - starts[seq_len(n)],
    original_start_s = starts[seq_len(n)],
    original_stop_s = original_stops[seq_len(n)],
    original_width_s = original_stops[seq_len(n)] - starts[seq_len(n)],
    inj_volume_uL = inj_vol,
    stringsAsFactors = FALSE
  )
}

extract_ascii_tokens <- function(
  data,
  min_len = 1L,
  max_len = 2000L,
  start_idx = 1L,
  end_idx = length(data)
) {
  ints <- as.integer(data)
  if (length(ints) == 0L) {
    return(character())
  }

  start_idx <- max(1L, as.integer(start_idx))
  end_idx <- min(length(ints), as.integer(end_idx))
  if (start_idx > end_idx) {
    return(character())
  }

  # Use a growable list to avoid repeated vector copies on large payloads.
  token_buf <- vector("list", 1024L)
  token_count <- 0L
  append_token <- function(txt) {
    token_count <<- token_count + 1L
    if (token_count > length(token_buf)) {
      length(token_buf) <<- length(token_buf) * 2L
    }
    token_buf[[token_count]] <<- txt
  }

  run_start <- NA_integer_
  flush_run <- function(end_run_idx) {
    if (is.na(run_start)) {
      return()
    }
    run_len <- end_run_idx - run_start + 1L
    if (run_len < min_len || run_len > max_len) {
      return()
    }
    txt <- rawToChar(as.raw(ints[run_start:end_run_idx]))
    txt <- trimws(txt)
    if (!identical(txt, "")) {
      append_token(txt)
    }
  }

  for (i in seq.int(start_idx, end_idx)) {
    ok <- ints[[i]] >= 32L && ints[[i]] <= 126L
    if (ok) {
      if (is.na(run_start)) {
        run_start <- i
      }
    } else {
      flush_run(i - 1L)
      run_start <- NA_integer_
    }
  }
  if (!is.na(run_start)) {
    flush_run(end_idx)
  }

  if (token_count == 0L) {
    return(character())
  }
  unlist(token_buf[seq_len(token_count)], use.names = FALSE)
}

locate_ascii_anchor_window <- function(payload, anchor = "stirrate", pre = 512L, post = 220000L) {
  if (length(payload) == 0L) {
    return(c(1L, 0L))
  }
  hits <- tryCatch(
    grepRaw(charToRaw(anchor), payload, fixed = TRUE, all = TRUE),
    error = function(e) integer()
  )
  if (length(hits) == 0L) {
    return(c(1L, length(payload)))
  }
  start <- max(1L, as.integer(hits[[1]]) - as.integer(pre))
  end <- min(length(payload), as.integer(hits[[1]]) + as.integer(post))
  c(start, end)
}

find_anchor_index <- function(tokens, anchor) {
  if (length(tokens) < length(anchor)) {
    return(NA_integer_)
  }
  limit <- length(tokens) - length(anchor) + 1L
  for (i in seq_len(limit)) {
    if (all(tokens[i:(i + length(anchor) - 1L)] == anchor)) {
      return(i)
    }
  }
  NA_integer_
}

extract_raw_kv_block <- function(payload) {
  anchor <- c("stirrate", "syringesize", "injrate", "tempsetpoint")
  is_key_token <- function(tok) {
    grepl("^[A-Za-z][A-Za-z0-9_.:-]{1,120}$", tok, perl = TRUE) &&
      !grepl("^(true|false)$", tok, ignore.case = TRUE, perl = TRUE) &&
      !grepl("^-?\\d+(?:\\.\\d+)?$", tok, perl = TRUE)
  }

  # Parse around an anchor window first; full-payload min_len=1 scans can be slow.
  win <- locate_ascii_anchor_window(payload, anchor = "stirrate", pre = 512L, post = 220000L)
  tokens <- extract_ascii_tokens(
    payload,
    min_len = 1L,
    max_len = 2000L,
    start_idx = win[[1]],
    end_idx = win[[2]]
  )
  i_start <- find_anchor_index(tokens, anchor)
  if (is.na(i_start)) {
    # Fallback 1: look for stirrate only.
    i_start <- match(TRUE, tolower(tokens) == "stirrate")
  }
  if (is.na(i_start)) {
    # Fallback 2: broader scan with a safer minimum token length.
    tokens <- extract_ascii_tokens(payload, min_len = 3L, max_len = 2000L)
    i_start <- find_anchor_index(tokens, anchor)
    if (is.na(i_start)) {
      i_start <- match(TRUE, tolower(tokens) == "stirrate")
      if (is.na(i_start)) {
        return(data.frame(raw_key = character(), raw_value = character(), stringsAsFactors = FALSE))
      }
    }
  }

  i_sha_rel <- match("SHA1", tokens[i_start:length(tokens)])
  if (!is.na(i_sha_rel)) {
    i_end <- i_start + i_sha_rel - 1L
  } else {
    # Fallback end-boundary: consume contiguous key-like tokens.
    i_end <- i_start
    i <- i_start + 1L
    while (i <= length(tokens)) {
      tok <- tokens[[i]]
      if (!is_key_token(tok) && (i - i_start) >= 6L) {
        break
      }
      if (!is_key_token(tok) && (i - i_start) < 6L) {
        i <- i + 1L
        next
      }
      i_end <- i
      i <- i + 1L
      if ((i_end - i_start) > 256L) {
        break
      }
    }
  }

  keys <- tokens[i_start:i_end]
  keys <- keys[vapply(keys, is_key_token, logical(1))]
  n_keys <- length(keys)
  if (n_keys == 0L) {
    return(data.frame(raw_key = character(), raw_value = character(), stringsAsFactors = FALSE))
  }
  values_start <- i_end + 1L
  values_end <- min(length(tokens), values_start + n_keys - 1L)
  values <- if (values_start <= values_end) tokens[values_start:values_end] else character()

  if (length(values) < n_keys) {
    values <- c(values, rep("", n_keys - length(values)))
  }
  if (length(values) > n_keys) {
    values <- values[seq_len(n_keys)]
  }

  data.frame(
    raw_key = keys,
    raw_value = values,
    stringsAsFactors = FALSE
  )
}

build_experiment_parameters <- function(payload, input_filename = "") {
  raw_kv <- extract_raw_kv_block(payload)
  rows <- list()

  add_row <- function(key, value, confidence = "high", note = "") {
    rows[[length(rows) + 1L]] <<- list(
      key = key,
      value = value,
      confidence = confidence,
      note = note
    )
  }

  if (!is.null(input_filename) && nzchar(input_filename)) {
    add_row("InputFileName", input_filename, "high", "Source .nitc filename")
  }

  if (nrow(raw_kv) > 0) {
    for (i in seq_len(nrow(raw_kv))) {
      add_row(
        key = paste0("raw::", raw_kv$raw_key[[i]]),
        value = raw_kv$raw_value[[i]],
        confidence = "high"
      )
    }
  }

  lookup_value <- function(src_key) {
    if (nrow(raw_kv) == 0) {
      return(list(value = "", found = FALSE, exact = FALSE))
    }

    exact <- which(raw_kv$raw_key == src_key)
    if (length(exact) > 0) {
      return(list(value = raw_kv$raw_value[[exact[[1]]]], found = TRUE, exact = TRUE))
    }

    idx <- which(tolower(raw_kv$raw_key) == tolower(src_key))
    if (length(idx) > 0) {
      return(list(value = raw_kv$raw_value[[idx[[1]]]], found = TRUE, exact = FALSE))
    }

    list(value = "", found = FALSE, exact = FALSE)
  }

  std_map <- list(
    StirRate = "stirrate",
    Temperature = "tempsetpoint",
    Titrant = "syringeconcentration",
    Titrate = "cellconcentration",
    HeatRateOffset = "heatRateOffset",
    starttime = "starttime",
    equilseconds = "equilseconds",
    UserName = "user",
    Comments = "comments",
    ExothermUp = "ExothermUp",
    Version = "FileVersion"
  )

  for (std_key in names(std_map)) {
    src_key <- std_map[[std_key]]
    got <- lookup_value(src_key)
    if (got$found) {
      note <- if (got$exact) "" else sprintf("Mapped from key '%s' (case-insensitive)", src_key)
      conf <- if (got$exact) "high" else "medium"
      add_row(std_key, got$value, conf, note)
    } else {
      if (nrow(raw_kv) == 0) {
        note <- sprintf("KV block not found; missing source key: %s", src_key)
        add_row(std_key, "", "low", note)
      } else {
        note <- sprintf("Missing source key: %s", src_key)
        add_row(std_key, "", "medium", note)
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      key = character(),
      value = character(),
      confidence = character(),
      note = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    key = vapply(rows, function(x) x$key, character(1)),
    value = vapply(rows, function(x) x$value, character(1)),
    confidence = vapply(rows, function(x) x$confidence, character(1)),
    note = vapply(rows, function(x) x$note, character(1)),
    stringsAsFactors = FALSE
  )
}

get_param_numeric <- function(exp_params, key) {
  if (nrow(exp_params) == 0) {
    return(NA_real_)
  }
  idx <- which(exp_params$key == key)
  if (length(idx) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(exp_params$value[[idx[[1]]]]))
}

collect_numeric_param_values <- function(params_df, keys) {
  if (is.null(params_df) || nrow(params_df) == 0L || length(keys) == 0L) {
    return(numeric())
  }
  out <- numeric()
  key_lc <- tolower(as.character(params_df$key))
  for (k in keys) {
    idx <- which(key_lc == tolower(k))
    if (length(idx) == 0L) {
      next
    }
    vals <- suppressWarnings(as.numeric(params_df$value[idx]))
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0L) {
      out <- c(out, vals)
    }
  }
  unique(out)
}

empty_heatflow_df <- function() {
  data.frame(
    "Time (seconds)" = numeric(),
    "Raw Heat Rate (µJ / s)" = numeric(),
    "Raw Heat Rate (µcal / s)" = numeric(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pick_heat_series <- function(peers, heat_rate_offset = NA_real_) {
  means <- vapply(peers, function(s) mean(s$values, na.rm = TRUE), numeric(1))
  sds <- vapply(peers, function(s) sd(s$values, na.rm = TRUE), numeric(1))
  if (is.finite(heat_rate_offset)) {
    which.min(abs(means - heat_rate_offset))
  } else {
    which.max(sds)
  }
}

build_heatflow_df_from_series <- function(series, heat_rate_offset = NA_real_, allow_synthetic_axis = FALSE) {
  if (length(series) == 0) {
    return(NULL)
  }

  axis_idx <- which(vapply(series, function(s) is_index_axis(s$values), logical(1)))
  axis_values <- NULL
  peers_idx <- integer()

  if (length(axis_idx) > 0) {
    axis_pick <- pick_best_axis_idx(series, axis_idx)
    axis <- series[[axis_pick]]
    axis_values <- axis$values
    peers_idx <- which(vapply(series, function(s) s$len == axis$len, logical(1)))
    peers_idx <- setdiff(peers_idx, axis_pick)
  } else if (allow_synthetic_axis) {
    lens <- vapply(series, function(s) s$len, numeric(1))
    uniq <- sort(unique(lens), decreasing = TRUE)
    if (length(uniq) == 0) {
      return(NULL)
    }
    best_len <- uniq[[1]]
    best_cnt <- sum(lens == best_len)
    for (k in uniq) {
      cnt <- sum(lens == k)
      if (cnt > best_cnt || (cnt == best_cnt && k > best_len)) {
        best_len <- k
        best_cnt <- cnt
      }
    }
    peers_idx <- which(lens == best_len)
    axis_values <- seq(0, best_len - 1)
  } else {
    return(NULL)
  }

  out <- data.frame(
    "Time (seconds)" = axis_values,
    "Raw Heat Rate (µJ / s)" = NA_real_,
    "Raw Heat Rate (µcal / s)" = NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (length(peers_idx) == 0) {
    return(out)
  }

  peers <- lapply(peers_idx, function(i) series[[i]])
  uj_idx <- pick_heat_series(peers, heat_rate_offset = heat_rate_offset)

  uj <- peers[[uj_idx]]$values
  out[["Raw Heat Rate (µJ / s)"]] <- uj

  uc_idx <- NA_integer_
  best_delta <- Inf
  for (i in seq_along(peers)) {
    if (i == uj_idx) {
      next
    }
    cand <- peers[[i]]$values
    ok <- is.finite(uj) & is.finite(cand) & abs(cand) > 1e-12
    if (!any(ok)) {
      next
    }
    ratio <- median(uj[ok] / cand[ok], na.rm = TRUE)
    if (!is.finite(ratio)) {
      next
    }
    if (ratio >= 3.5 && ratio <= 4.8) {
      delta <- abs(ratio - 4.184)
      if (delta < best_delta) {
        best_delta <- delta
        uc_idx <- i
      }
    }
  }

  if (!is.na(uc_idx)) {
    out[["Raw Heat Rate (µcal / s)"]] <- peers[[uc_idx]]$values
  } else {
    out[["Raw Heat Rate (µcal / s)"]] <- uj / 4.184
  }

  out
}

extract_heatflow_df <- function(payload, heat_rate_offset = NA_real_) {
  primary <- scan_numeric_series(payload, min_len = 500L, abs_min = 1e-9)
  out <- build_heatflow_df_from_series(primary, heat_rate_offset = heat_rate_offset, allow_synthetic_axis = FALSE)
  if (!is.null(out)) {
    return(out)
  }

  # Fallback for short/partial records: use lower threshold and synthetic axis if needed.
  fallback <- scan_numeric_series(payload, min_len = 100L, abs_min = 1e-12)
  out2 <- build_heatflow_df_from_series(fallback, heat_rate_offset = heat_rate_offset, allow_synthetic_axis = TRUE)
  if (!is.null(out2)) {
    return(out2)
  }

  empty_heatflow_df()
}

main <- function() {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  nitc <- path.expand(cli$input)
  if (!file.exists(nitc)) {
    stop(sprintf("Input not found: %s", nitc), call. = FALSE)
  }

  if (is.null(cli$out_xlsx)) {
    out_xlsx <- file.path(dirname(nitc), paste0(tools::file_path_sans_ext(basename(nitc)), "_nitc_extract.xlsx"))
  } else {
    out_xlsx <- path.expand(cli$out_xlsx)
  }
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  payload <- load_nitc_payload(nitc)
  exp_params <- build_experiment_parameters(payload, input_filename = basename(nitc))

  step200 <- find_step200_sequences(payload, min_len = 6L)
  arith <- find_arithmetic_int_sequences(payload, min_len = 6L)
  selected <- select_start_stop(step200 = step200, arith = arith)
  mono_int <- NULL
  if (identical(selected$source, "none")) {
    mono_int <- find_monotonic_int_sequences(payload, n = 8L)
    selected <- select_start_stop(step200 = step200, mono_int = mono_int)
  }
  if (identical(selected$source, "none")) {
    selected <- select_from_monotonic_sequences(mono_int)
  }

  n_inj <- if (!is.null(selected$start)) length(selected$start) else 0L
  volume_min_len <- if (n_inj > 0L) max(3L, min(12L, as.integer(n_inj))) else 6L
  volume_targets <- collect_numeric_param_values(
    exp_params,
    keys = c("DefaultInjVolume", "raw::injvol", "raw::injvolume", "raw::injectionvolume", "raw::defaultinjvolume")
  )
  volume_run <- find_best_volume_run(
    payload,
    expected_len = n_inj,
    preferred_targets = volume_targets,
    min_len = volume_min_len
  )
  volumes <- if (!is.null(volume_run)) volume_run$values else NULL

  injections_df <- data.frame(
    inj_num = integer(),
    start_s = numeric(),
    stop_s = numeric(),
    width_s = numeric(),
    original_start_s = numeric(),
    original_stop_s = numeric(),
    original_width_s = numeric(),
    inj_volume_uL = numeric(),
    stringsAsFactors = FALSE
  )
  if (!is.null(selected$start) && !is.null(selected$stop)) {
    injections_df <- build_injection_df(selected$start, selected$stop, selected$original_stop, volumes)
  }

  heat_offset <- get_param_numeric(exp_params, "HeatRateOffset")
  heatflow_df <- extract_heatflow_df(payload, heat_rate_offset = heat_offset)

  sheets <- list(
    experiment_parameters = exp_params,
    titration_points = injections_df,
    heatflow = heatflow_df
  )
  write_xlsx(sheets, path = out_xlsx)

  cat(normalizePath(out_xlsx, winslash = "/", mustWork = FALSE), "\n", sep = "")
}

main()
