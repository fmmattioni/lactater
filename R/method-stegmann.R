#' Individual anaerobic threshold of Stegmann et al. (1981)
#'
#' It applies the `Stegmann` method: blood lactate is followed into recovery until it falls back to its
#' end-of-exercise value, and from that point a tangent is drawn to the lactate curve of the test; the point of
#' contact is the threshold, and the slope of the tangent the maximal rate of lactate elimination (Stegmann et al.,
#' 1981). Lactate is followed over time, so recovery minutes are put on the intensity axis at the rate the test rose
#' (the median increment between stages divided by `settings$stage_duration`). The time lactate is back at its
#' end-of-exercise value is interpolated linearly between the recovery samples after the post-exercise peak.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`.
#' @keywords internal
method_stegmann <- function(data, curve, settings) {
  tangent <- stegmann_tangent(data, curve, settings)
  method_result(
    method = "Stegmann",
    fitting = user_fitting(settings),
    intensity = tangent$intensity,
    lactate = if(is.na(tangent$intensity)) NA_real_ else retrieve_lactate(curve, tangent$intensity),
    plot_data = list(curve = curve_points(curve), tangent = tangent, fitting = curve$fit)
  )
}

#' The Stegmann tangent
#'
#' @inheritParams method_obla
#'
#' @return a list with the contact `intensity` (or `NA` when recovery lactate never fell back to its
#' end-of-exercise value after a rise), the `back` time (minutes after exercise), the point `from` which the tangent
#' is drawn, the `recovery` samples placed on the intensity axis, and the maximal `elimination` rate
#' (mmol/L/min).
#' @keywords internal
stegmann_tangent <- function(data, curve, settings) {
  stages <- data[data$intensity > 0, ]
  end <- stages[nrow(stages), ]
  rate <- stats::median(diff(stages$intensity)) / settings$stage_duration
  recovery <- settings$recovery
  placed <- data.frame(
    time = recovery$time, intensity = end$intensity + recovery$time * rate, lactate = recovery$lactate
  )
  out <- list(intensity = NA_real_, back = NA_real_, from = NULL, recovery = placed, elimination = NA_real_)

  ## after the post-exercise peak, the first time lactate is back at its end-of-exercise value
  times <- c(0, recovery$time)
  values <- c(end$lactate, recovery$lactate)
  peak <- which.max(values)
  back <- which(seq_along(values) > peak & values <= end$lactate)
  if(peak == 1 || length(back) == 0) return(out)
  j <- back[1]
  out$back <- times[j - 1] + (end$lactate - values[j - 1]) / (values[j] - values[j - 1]) * (times[j] - times[j - 1])
  out$from <- c(intensity = end$intensity + out$back * rate, lactate = end$lactate)

  ## the tangent that touches the curve from below is the steepest line from that point to the curve
  slope <- function(x) (end$lactate - curve$predict(x)) / (out$from[["intensity"]] - x)
  grid <- seq(min(stages$intensity), end$intensity, length.out = 2001)
  best <- which.max(slope(grid))
  out$intensity <- stats::optimize(
    slope, grid[c(max(best - 1, 1), min(best + 1, length(grid)))], maximum = TRUE, tol = 1e-8
  )$maximum
  out$elimination <- slope(out$intensity) * rate
  out
}

#' Check and tidy the recovery samples
#'
#' @param recovery The `recovery` argument of [lactate_threshold()].
#' @param stage_duration The `stage_duration` argument of [lactate_threshold()].
#' @return a data frame with `time` and `lactate`, ordered by time, without missing samples.
#' @keywords internal
prepare_recovery <- function(recovery, stage_duration) {
  if(!is.data.frame(recovery) || !all(c("time", "lactate") %in% names(recovery)))
    stop("`recovery` must be a data frame with the columns `time` (minutes after exercise) and `lactate`.",
         call. = FALSE)
  recovery <- data.frame(time = as.numeric(recovery[["time"]]), lactate = as.numeric(recovery[["lactate"]]))
  recovery <- recovery[stats::complete.cases(recovery), ]
  recovery <- recovery[order(recovery[["time"]]), ]
  if(nrow(recovery) < 2 || any(recovery[["time"]] <= 0) || any(recovery[["lactate"]] <= 0))
    stop("`recovery` needs at least two samples after the end of exercise, with positive times and lactate.",
         call. = FALSE)
  if(!is.numeric(stage_duration) || length(stage_duration) != 1 || is.na(stage_duration) || stage_duration <= 0)
    stop("`stage_duration` (minutes) is needed with `recovery`, to put the recovery times on the intensity axis.",
         call. = FALSE)
  recovery
}
