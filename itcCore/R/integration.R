integrate_peaks <- function(time, corrected_power, injection_indices, integration_window = NULL, start_offset = 0) {
  heats <- c()
  start_times <- c()
  end_times <- c()

  if (length(injection_indices) < 2) {
    return(data.frame(Injection = integer(), Heat_ucal = numeric(), StartTime_s = numeric(), EndTime_s = numeric()))
  }

  injection_indices <- sort(unique(injection_indices))
  n_points <- length(time)

  for (i in 1:(length(injection_indices) - 1)) {
    orig_start_idx <- injection_indices[i]
    start_idx <- max(1, min(orig_start_idx + start_offset, n_points))

    next_orig_idx <- injection_indices[i + 1]
    next_start_idx <- max(1, min(next_orig_idx + start_offset, n_points))
    end_idx_limit <- next_start_idx - 1

    if (end_idx_limit < start_idx) {
      t_record <- if (start_idx <= n_points) time[start_idx] else NA_real_
      heats <- c(heats, 0)
      start_times <- c(start_times, t_record)
      end_times <- c(end_times, t_record)
      next
    }

    if (!is.null(integration_window) && integration_window > 0) {
      end_idx <- orig_start_idx + integration_window
    } else {
      end_idx <- end_idx_limit
    }

    end_idx <- min(end_idx, n_points)

    if (start_idx >= end_idx) {
      t_record <- if (start_idx <= n_points) time[start_idx] else NA_real_
      heats <- c(heats, 0)
      start_times <- c(start_times, t_record)
      end_times <- c(end_times, t_record)
      next
    }

    t_seg <- time[start_idx:end_idx]
    p_seg <- corrected_power[start_idx:end_idx]
    dt <- diff(t_seg)
    p_avg <- (head(p_seg, -1) + tail(p_seg, -1)) / 2
    area <- sum(dt * p_avg)

    heats <- c(heats, area)
    start_times <- c(start_times, time[start_idx])
    end_times <- c(end_times, time[end_idx])
  }

  last_inj <- tail(injection_indices, 1)
  last_start_idx <- max(1, last_inj + start_offset)
  if (last_start_idx < n_points) {
    end_idx <- n_points
    if (!is.null(integration_window) && integration_window > 0) {
      end_idx <- min(last_inj + integration_window, n_points)
    }

    if (last_start_idx < end_idx) {
      t_seg <- time[last_start_idx:end_idx]
      p_seg <- corrected_power[last_start_idx:end_idx]
      dt <- diff(t_seg)
      p_avg <- (head(p_seg, -1) + tail(p_seg, -1)) / 2
      area <- sum(dt * p_avg)
      heats <- c(heats, area)
      start_times <- c(start_times, time[last_start_idx])
      end_times <- c(end_times, time[end_idx])
    } else {
      t_record <- if (last_start_idx <= n_points) time[last_start_idx] else NA_real_
      heats <- c(heats, 0)
      start_times <- c(start_times, t_record)
      end_times <- c(end_times, t_record)
    }
  }

  data.frame(
    Injection = 0:(length(heats) - 1),
    Heat_ucal = heats,
    StartTime_s = start_times,
    EndTime_s = end_times
  )
}
