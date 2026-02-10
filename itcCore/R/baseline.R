.get_anchor <- function(time, power, end_idx, baseline_duration) {
  if (end_idx < 1) return(NULL)

  t_end <- time[end_idx]
  t_start <- t_end - baseline_duration

  curr <- end_idx
  while (curr > 0 && time[curr] > t_start) {
    curr <- curr - 1
  }
  start_idx <- curr + 1

  if (start_idx >= end_idx) return(NULL)

  val_x <- mean(time[start_idx:end_idx])
  val_y <- mean(power[start_idx:end_idx])
  c(val_x, val_y)
}

.collect_baseline_anchors <- function(time, power, injection_indices,
                                      injection_times = NULL,
                                      baseline_duration = 20,
                                      baseline_offset = 0) {
  anchors_x <- c()
  anchors_y <- c()

  if (length(injection_indices) < 1) {
    return(list(x = anchors_x, y = anchors_y))
  }

  inj_start_times <- numeric(length(injection_indices))
  if (!is.null(injection_times) && length(injection_times) == length(injection_indices)) {
    inj_start_times <- injection_times
  } else {
    inj_start_times <- time[injection_indices]
  }

  for (i in seq_along(injection_indices)) {
    idx <- injection_indices[i]
    t_inj_start <- inj_start_times[i]
    t_target_end <- t_inj_start - baseline_offset

    real_end_idx <- idx - 1
    while (real_end_idx > 0 && time[real_end_idx] > t_target_end) {
      real_end_idx <- real_end_idx - 1
    }

    if (real_end_idx > 0) {
      anchor <- .get_anchor(time, power, real_end_idx, baseline_duration)
      if (!is.null(anchor)) {
        anchors_x <- c(anchors_x, anchor[1])
        anchors_y <- c(anchors_y, anchor[2])
      }
    }
  }

  last_idx <- length(time)
  anchor <- .get_anchor(time, power, last_idx, baseline_duration)
  if (!is.null(anchor)) {
    anchors_x <- c(anchors_x, anchor[1])
    anchors_y <- c(anchors_y, anchor[2])
  }

  if (length(anchors_x) < 1) {
    return(list(x = anchors_x, y = anchors_y))
  }

  ord <- order(anchors_x)
  anchors_x <- anchors_x[ord]
  anchors_y <- anchors_y[ord]

  if (any(duplicated(anchors_x))) {
    uniq_x <- unique(anchors_x)
    uniq_y <- sapply(uniq_x, function(ux) mean(anchors_y[anchors_x == ux]))
    anchors_x <- uniq_x
    anchors_y <- uniq_y
  }

  list(x = anchors_x, y = anchors_y)
}

SegmentedBaseline <- function(time, power, injection_indices,
                              injection_times = NULL,
                              baseline_duration = 20,
                              baseline_offset = 0,
                              spar = 0.1) {
  if (length(injection_indices) < 1) {
    fit_lm <- lm(power ~ time)
    return(predict(fit_lm, data.frame(time = time)))
  }

  anchors <- .collect_baseline_anchors(time, power, injection_indices,
                                       injection_times, baseline_duration, baseline_offset)

  if (length(anchors$x) > 1) {
    fit_spline <- if (is.null(spar) || is.na(spar)) {
      smooth.spline(anchors$x, anchors$y)
    } else {
      smooth.spline(anchors$x, anchors$y, spar = spar)
    }
    return(predict(fit_spline, time)$y)
  }

  fit_lm <- lm(power ~ time)
  predict(fit_lm, data.frame(time = time))
}

SegmentedBaselineAnchors <- function(time, power, injection_indices,
                                     injection_times = NULL,
                                     baseline_duration = 20,
                                     baseline_offset = 0) {
  anchors <- .collect_baseline_anchors(time, power, injection_indices,
                                       injection_times, baseline_duration, baseline_offset)
  if (length(anchors$x) < 1) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  data.frame(x = anchors$x, y = anchors$y)
}
