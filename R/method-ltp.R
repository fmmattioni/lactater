#' Lactate Turning Point (LTP)
#'
#' It applies the `LTP` methods: `LTP1`, and `LTP2`. A three-segment continuous linear regression
#' is fitted to the measured points; the two breakpoints with the lowest residual sum of squares
#' are found by exhaustive search (see `fit_breakpoints()`). With `ltp_fix_first_slope` the first
#' segment is flat. Hofmann et al. (1997) and Pokan et al. (1997) fitted each turn point with its own
#' two-line regression inside a window, on tests with 1-min or 90-s stages; with the 6-8 stages of a
#' typical step test those windows hold too few stages, so the three segments are fitted together.
#' With `settings$ltp_breakpoints = 1` (a submaximal test) two segments are fitted, the breakpoint is
#' LTP1 and LTP2 is `NA`. The lactate at each turn point is read off the fitted segments. The resting
#' sample is never used: it has no exercise intensity.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_ltp <- function(data, curve, settings) {
  points <- data[data$intensity > 0, ]
  fit <- fit_breakpoints(
    points$intensity, points$lactate,
    n_breakpoints = settings$ltp_breakpoints, flat_first = settings$ltp_fix_first_slope
  )
  intensity <- c(fit$psi, NA_real_)[1:2]
  method_result(
    method = c("LTP1", "LTP2"),
    fitting = user_fitting(settings),
    intensity = intensity,
    lactate = round(predict_breakpoints(fit, intensity), 1),
    plot_data = list(
      breakpoints = fit, log = FALSE,
      fitting = if(settings$ltp_breakpoints == 2) "three straight segments" else "two straight segments"
    )
  )
}
