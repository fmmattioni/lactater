#' Lactate Turning Point (LTP)
#'
#' It applies the `LTP` methods: `LTP1`, and `LTP2`.
#'
#' A three-segment continuous linear regression is fitted to the measured points; the two breakpoints with the
#' lowest residual sum of squares are found by exhaustive search (see `fit_breakpoints()`).
#'
#' @param data_prepared The data retrieved from `prepare_fit()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param fix_first_slope Whether the first segment (before LTP1) is flat (slope fixed at 0).
#'
#' @return a tibble with the following columns:
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @keywords internal
method_ltp <- function(data_prepared, fit, sport, plot, fix_first_slope = FALSE) {
  points <- modeling_data(data_prepared$data[[1]], data_prepared$bsln)
  breakpoint_fit <- fit_breakpoints(points$intensity, points$lactate, n_breakpoints = 2, flat_first = fix_first_slope)
  method_intensity <- breakpoint_fit$psi

  out <- data_prepared %>%
    tidyr::crossing(
      method = c("LTP1", "LTP2"),
      fitting = paste({{ fit }}, "(user-defined)", sep = " ")
    ) %>%
    dplyr::mutate(
      intensity = method_intensity,
      lactate = unlist(Map(retrieve_lactate, model = model, intensity_value = intensity))
    ) %>%
    dplyr::mutate(breakpoint_fit = list(breakpoint_fit))

  if(plot) {
    out <- out %>%
      dplyr::mutate(
        plot = Map(
          function(row_id, method_name) plot_lactate(data_processed = out[row_id, ], method = method_name),
          seq_len(nrow(out)),
          method
        )
      )
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
