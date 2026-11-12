#' Keul and Simon
#'
#' The individual anaerobic thresholds of Keul et al. (1979) and Simon et al. (1981): the speed at which a 3rd degree
#' polynomial through the exercise stages rises by 1.26 mmol/L per km/h (an angle of 51°34') or by 1 mmol/L per km/h
#' (45°). Both fitted lactate against running speed in km/h, on treadmill tests at a 5% grade with 3-min stages of
#' 2 km/h; a slope in other units has no meaning, so these methods run on running tests only. When the slope reaches
#' the value more than once, the last crossing where it rises through it is used. For internal use only.
#'
#' @inheritParams method_obla
#' @param method The method name.
#' @param slope The slope (mmol/L per km/h).
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`.
#' @keywords internal
method_slope <- function(data, method, slope) {
  poly <- fit_curve(data[data$intensity > 0, ], "3rd degree polynomial")
  k <- poly$coefficients
  gradient <- list(domain = poly$domain, predict = function(x) k[2] + 2 * k[3] * x + 3 * k[4] * x^2)
  intensity <- retrieve_intensity(gradient, slope)
  lactate <- poly$predict(intensity)
  ## the tangent drawn over a fifth of the test either side of the point, within the stages and above zero lactate
  half <- 0.2 * diff(poly$domain)
  ends <- c(
    max(intensity - half, poly$domain[1], intensity - 0.95 * lactate / slope),
    min(intensity + half, poly$domain[2])
  )
  method_result(
    method = method,
    fitting = "3rd degree polynomial (default)",
    intensity = intensity,
    lactate = round(lactate, 1),
    plot_data = list(
      curve = curve_points(poly),
      touch = data.frame(intensity = ends, lactate = lactate + (ends - intensity) * slope),
      fitting = "3rd degree polynomial"
    )
  )
}

method_keul <- function(data, curve, settings) method_slope(data, "Keul", 1.26)

method_simon <- function(data, curve, settings) method_slope(data, "Simon", 1)
