parse_itc_metadata <- function(lines) {
  params <- list(
    n_injections = NA_integer_,
    temperature_C = NA_real_,
    syringe_conc_mM = NA_real_,
    cell_conc_mM = NA_real_,
    cell_volume_mL = NA_real_,
    V_pre_ul = NA_real_,
    V_inj_ul = NA_real_,
    titration_interval_s = NA_real_,
    injection_volumes_ul = NULL
  )

  dollar_lines <- which(grepl("^\\$", lines))
  for (i in seq_along(dollar_lines)) {
    idx <- dollar_lines[i]
    line <- trimws(lines[idx])
    if (grepl("^\\$\\s*[0-9]+\\s*$", line)) {
      n <- as.integer(gsub("^\\$\\s*([0-9]+).*", "\\1", line))
      if (i == 2 || is.na(params$n_injections)) {
        params$n_injections <- n
      }
    }
    if (line == "$NOT" && idx < length(lines)) {
      next_line <- trimws(lines[idx + 1])
      if (grepl("^\\$\\s*[0-9.-]+", next_line)) {
        params$temperature_C <- as.numeric(gsub("^\\$\\s*([0-9.-]+).*", "\\1", next_line))
      }
    }
    if (grepl("^\\$\\s*[0-9.]+\\s*,", line)) {
      content <- sub("^\\$\\s*", "", line)
      parts <- trimws(strsplit(content, ",")[[1]])
      nums <- suppressWarnings(as.numeric(parts))

      if (length(nums) >= 1 && !is.na(nums[1])) {
        if (is.null(params$injection_volumes_ul)) {
          params$injection_volumes_ul <- nums[1]
        } else {
          params$injection_volumes_ul <- c(params$injection_volumes_ul, nums[1])
        }
      }

      if (is.na(params$titration_interval_s) && length(nums) >= 3 && !is.na(nums[3])) {
        params$titration_interval_s <- nums[3]
      }
    }
  }

  if (!is.null(params$injection_volumes_ul) && length(params$injection_volumes_ul) >= 1) {
    params$V_pre_ul <- params$injection_volumes_ul[1]
    params$V_inj_ul <- if (length(params$injection_volumes_ul) >= 2) {
      params$injection_volumes_ul[2]
    } else {
      params$injection_volumes_ul[1]
    }
  }

  hash_numeric <- grep("^#\\s*[0-9.-]+", lines, value = FALSE)
  hash_vals <- numeric(0)
  for (j in hash_numeric) {
    line <- trimws(lines[j])
    val <- as.numeric(gsub("^#\\s*([0-9.-]+).*", "\\1", line))
    if (!is.na(val)) {
      hash_vals <- c(hash_vals, val)
    }
  }

  if (length(hash_vals) >= 2) params$syringe_conc_mM <- hash_vals[2]
  if (length(hash_vals) >= 3) params$cell_conc_mM <- hash_vals[3]
  if (length(hash_vals) >= 4) params$cell_volume_mL <- hash_vals[4]
  if (length(hash_vals) >= 5) params$temperature_C <- hash_vals[5]

  params
}

read_itc <- function(file_path) {
  lines <- readLines(file_path)
  params <- parse_itc_metadata(lines)

  time <- c()
  power <- c()
  injection_indices <- c()
  parsed_injection_times <- c()
  parsed_injection_volumes_ul <- c()
  parsed_injection_index <- c()
  current_idx <- 0

  for (i in seq_along(lines)) {
    line <- lines[i]

    if (grepl("^@", line)) {
      injection_indices <- c(injection_indices, current_idx + 1)
      content <- sub("^@", "", line)
      parts <- trimws(strsplit(content, ",")[[1]])

      inj_idx <- NA_integer_
      if (length(parts) >= 1L) {
        inj_idx <- suppressWarnings(as.integer(parts[1L]))
      }
      parsed_injection_index <- c(parsed_injection_index, inj_idx)

      inj_time <- NA_real_
      if (length(parts) >= 4) {
        inj_time <- as.numeric(parts[4])
      }
      parsed_injection_times <- c(parsed_injection_times, inj_time)

      inj_vol_ul <- NA_real_
      if (length(parts) >= 2) {
        v <- as.numeric(parts[2])
        if (!is.na(v)) inj_vol_ul <- v
      }
      parsed_injection_volumes_ul <- c(parsed_injection_volumes_ul, inj_vol_ul)
      next
    }

    if (grepl("^[$#%?]", line) || nchar(trimws(line)) == 0) next

    if (grepl("^[0-9.-]", line)) {
      parts <- strsplit(line, ",")[[1]]
      if (length(parts) >= 2) {
        t <- as.numeric(parts[1])
        p <- as.numeric(parts[2])
        if (!is.na(t) && !is.na(p)) {
          time <- c(time, t)
          power <- c(power, p)
          current_idx <- current_idx + 1
        }
      }
    }
  }

  if (length(injection_indices) == 0 && current_idx > 0) {
    injection_indices <- c(1)
    parsed_injection_times <- c(NA)
    parsed_injection_volumes_ul <- c(NA_real_)
    parsed_injection_index <- c(NA_integer_)
  }

  final_injection_times <- numeric(length(injection_indices))
  for (k in seq_along(injection_indices)) {
    idx <- injection_indices[k]
    raw_time <- if (k <= length(parsed_injection_times)) parsed_injection_times[k] else NA_real_
    if (!is.na(raw_time)) {
      final_injection_times[k] <- raw_time
    } else if (idx <= length(time)) {
      final_injection_times[k] <- time[idx]
    } else {
      final_injection_times[k] <- NA_real_
    }
  }

  n_inj <- length(injection_indices)
  injection_volumes_ul <- if (length(parsed_injection_volumes_ul) >= n_inj) {
    parsed_injection_volumes_ul[seq_len(n_inj)]
  } else {
    rep(NA_real_, n_inj)
  }

  for (k in seq_len(n_inj)) {
    if (is.na(injection_volumes_ul[k])) {
      injection_volumes_ul[k] <- if (k == 1) params$V_pre_ul else params$V_inj_ul
    }
  }

  last_index <- if (length(parsed_injection_index) > 0L) parsed_injection_index[length(parsed_injection_index)] else NA_integer_
  params$n_injections <- if (!is.na(last_index)) as.integer(last_index) else length(injection_indices)

  first_vol_idx <- 1L
  if (length(parsed_injection_index) >= 1L && parsed_injection_index[1L] == 0L && length(injection_volumes_ul) >= 2L) {
    first_vol_idx <- 2L
  }
  if (length(injection_volumes_ul) >= first_vol_idx && !is.na(injection_volumes_ul[first_vol_idx])) {
    params$V_pre_ul <- injection_volumes_ul[first_vol_idx]
  }
  if (length(injection_volumes_ul) >= 2L && !is.na(injection_volumes_ul[2L])) {
    params$V_inj_ul <- injection_volumes_ul[2L]
  }

  list(
    data = data.frame(Time = time, Power = power),
    injections = injection_indices,
    injection_times = final_injection_times,
    injection_volumes_ul = injection_volumes_ul,
    params = params
  )
}

parse_itc <- function(path) {
  read_itc(path)
}
