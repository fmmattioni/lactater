#' Lactate Turning Point (LTP)
#'
#' It applies the `LTP` methods: `LTP1`, and `LTP2`. A three-segment continuous linear regression
#' is fitted to the measured points; the two breakpoints with the lowest residual sum of squares
#' are found by exhaustive search (see `fit_breakpoints()`). With `ltp_fix_first_slope` the first
#' segment is flat.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_ltp <- function(data, curve, settings) {
  points <- modeling_data(data, settings$include_baseline)
  fit <- fit_breakpoints(points$intensity, points$lactate, n_breakpoints = 2, flat_first = settings$ltp_fix_first_slope)
  method_result(
    method = c("LTP1", "LTP2"),
    fitting = user_fitting(settings),
    intensity = fit$psi,
    lactate = retrieve_lactate(curve, fit$psi),
    plot_data = list(breakpoints = fit, log = FALSE, fitting = "three straight segments")
  )
}
