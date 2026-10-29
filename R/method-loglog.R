#' Log-log
#'
#' It applies the `Log-log` method.
#'
#' The two-segment regression of log(lactate) on log(intensity) is fitted to the measured points, and the
#' breakpoint with the lowest residual sum of squares is found by exhaustive search (see `fit_breakpoints()`).
#'
#' @param data_prepared The data retrieved from `prepare_fit()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @return a tibble with the following columns:
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @keywords internal
method_loglog <- function(data_prepared, fit, sport, loglog_restrainer = 1, plot) {
  breakpoint <- loglog_breakpoint(modeling_data(data_prepared$data[[1]], data_prepared$bsln), loglog_restrainer)
  method_intensity <- breakpoint$intensity

  out <- data_prepared %>%
    dplyr::mutate(
      method = "Log-log",
      fitting = paste({{ fit }}, "(user-defined)", sep = " "),
      intensity = method_intensity,
      lactate = retrieve_lactate(model = model[[1]], intensity_value = method_intensity)
    ) %>%
    dplyr::mutate(breakpoint_fit = list(breakpoint$fit))

  if(plot) {
    out <- out %>%
      dplyr::mutate(plot = list(plot_lactate(data_processed = ., method = method)))
  }

  if(sport == "swimming") {
    out <- out %>%
      dplyr::mutate(pace = convert_to_pace(speed = intensity))
  }

  out <- out %>%
    dplyr::select(dplyr::any_of(c("method", "fitting", "intensity", "pace", "lactate", "heart_rate", "plot")))

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
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
