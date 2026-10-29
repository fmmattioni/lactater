#' Minimum Lactate-Intensity Ratio (LTratio)
#'
#' It applies the `LTratio` method: the intensity where lactate / intensity, on a B-spline fit, is
#' lowest. The search runs over the exercise stages only.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_ltratio <- function(data, curve, settings) {
  minimum <- ltratio_minimum(data, curve, settings)
  method_result(
    method = "LTratio",
    fitting = "B-Spline (default)",
    intensity = minimum$intensity,
    lactate = retrieve_lactate(minimum$spline, minimum$intensity),
    plot_data = list(ratio = minimum$ratio, ratio_min = minimum$ratio_min, fitting = "B-spline")
  )
}

#' Individual Anaerobic Threshold (IAT)
#'
#' It applies the `IAT` method: the intensity at which the B-spline curve is 1.5 mmol/L above the
#' lactate at LTratio (Dickhuth et al., 1999).
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_iat <- function(data, curve, settings) {
  minimum <- ltratio_minimum(data, curve, settings)
  target <- minimum$spline$predict(minimum$intensity) + 1.5
  method_result(
    method = "IAT",
    fitting = "B-Spline (default)",
    intensity = retrieve_intensity(minimum$spline, target),
    lactate = round(target, 1),
    plot_data = list(curve = curve_points(minimum$spline), fitting = "B-spline")
  )
}

#' LTratio minimum
#'
#' @inheritParams method_obla
#'
#' @return a list with the B-spline `spline`, the `intensity` of the minimum ratio, its value
#' `ratio_min`, and the `ratio` curve over the exercise stages.
#' @keywords internal
ltratio_minimum <- function(data, curve, settings) {
  spline <- if(curve$fit == "B-spline") curve else fit_curve(modeling_data(data, settings$include_baseline), "B-spline")
  ratio <- function(x) spline$predict(x) / x

  ## the ratio is only meaningful during exercise, not at the positioned resting sample
  grid <- seq(min(data$intensity[data$intensity > 0]), spline$domain[2], length.out = 2001)
  best <- which.min(ratio(grid))
  intensity <- stats::optimize(ratio, grid[c(max(best - 1, 1), min(best + 1, length(grid)))], tol = 1e-8)$minimum

  list(
    spline = spline,
    intensity = intensity,
    ratio_min = ratio(intensity),
    ratio = data.frame(intensity = grid, ratio = ratio(grid))
  )
}
