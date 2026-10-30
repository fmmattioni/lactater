#' Log-log
#'
#' It applies the `Log-log` method. Two straight lines are fitted to log(lactate) vs log(intensity),
#' split at the measured stage with the lowest total residual sum of squares, and the threshold is
#' where they cross (see `fit_two_lines()`). The lactate at the threshold is read off the lines.
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
    lactate = round(exp(predict_breakpoints(breakpoint$fit, log(breakpoint$intensity))), 1),
    plot_data = list(breakpoints = breakpoint$fit, log = TRUE, fitting = "two straight lines, log-log")
  )
}

#' Log-log breakpoint
#'
#' Fits two straight lines to log(lactate) vs log(intensity) of the measured points and returns the
#' unrounded intensity where they cross. For internal use only.
#'
#' @param points The measured points (`intensity`, `lactate`) used for the fit.
#' @param loglog_restrainer The share of the points to use (see [lactate_threshold()]).
#'
#' @return a list with the breakpoint `intensity` and the `fit` from `fit_two_lines()` (in log-log space).
#' @keywords internal
loglog_breakpoint <- function(points, loglog_restrainer = 1) {
  points <- points[points$intensity > 0, ]
  points <- points[seq_len(floor(loglog_restrainer * nrow(points))), ]

  if(nrow(points) < 5) {
    stop("Log-log method requires at least 5 measured stages after applying `loglog_restrainer`.", call. = FALSE)
  }

  fit <- fit_two_lines(log(points$intensity), log(points$lactate))
  list(intensity = exp(fit$psi), fit = fit)
}
