#' Log-log
#'
#' It applies the `Log-log` method. The two-segment regression of log(lactate) on log(intensity)
#' is fitted to the measured points, and the breakpoint with the lowest residual sum of squares
#' is found by exhaustive search (see `fit_breakpoints()`).
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_loglog <- function(data, curve, settings) {
  breakpoint <- loglog_breakpoint(modeling_data(data, settings$include_baseline), settings$loglog_restrainer)
  method_result(
    method = "Log-log",
    fitting = user_fitting(settings),
    intensity = breakpoint$intensity,
    lactate = retrieve_lactate(curve, breakpoint$intensity),
    plot_data = list(breakpoints = breakpoint$fit, log = TRUE, fitting = "two straight segments, log-log")
  )
}

#' Log-log breakpoint
#'
#' Fits the two-segment regression of log(lactate) on log(intensity) to the measured points and
#' returns the unrounded breakpoint intensity. For internal use only.
#'
#' @param points The measured points (`intensity`, `lactate`) used for the fit.
#' @param loglog_restrainer The share of the points to use (see [lactate_threshold()]).
#'
#' @return a list with the breakpoint `intensity` and the breakpoint `fit` (in log-log space).
#' @keywords internal
loglog_breakpoint <- function(points, loglog_restrainer = 1) {
  points <- points[points$intensity > 0, ]
  points <- points[seq_len(floor(loglog_restrainer * nrow(points))), ]

  if(nrow(points) < 4) {
    stop("Log-log method requires at least 4 measured stages after applying `loglog_restrainer`.", call. = FALSE)
  }

  breakpoint_fit <- fit_breakpoints(log(points$intensity), log(points$lactate), n_breakpoints = 1)
  list(intensity = exp(breakpoint_fit$psi), fit = breakpoint_fit)
}
