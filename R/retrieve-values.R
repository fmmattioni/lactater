#' Retrieve intensity
#'
#' Retrieves the intensity at which the fitted lactate curve reaches a given lactate value,
#' by solving the fitted curve (not by interpolating between fitted points, which
#' under-estimates the intensity on a convex curve, see issue #8).
#'
#' @param curve A curve from `fit_curve()`.
#' @param lactate_value The lactate value.
#'
#' @return the intensity associated with the estimated lactate threshold, or `NA` when the
#' curve never reaches `lactate_value` within the tested range.
#' @keywords internal
#' @importFrom stats uniroot
retrieve_intensity <- function(curve, lactate_value) {
  x <- seq(curve$domain[1], curve$domain[2], length.out = 2001)
  gap <- curve$predict(x) - lactate_value
  crossings <- which(gap[-1] * gap[-length(gap)] <= 0)
  if (length(crossings) == 0) return(NA_real_)

  ## the curve can cross the target more than once (early lactate dip): keep the highest intensity
  i <- max(crossings)
  uniroot(function(v) curve$predict(v) - lactate_value, lower = x[i], upper = x[i + 1], tol = 1e-8)$root
}

#' Retrieve lactate
#'
#' Retrieves the fitted lactate at given intensities, rounded to 0.1 mmol/L.
#'
#' @param curve A curve from `fit_curve()`.
#' @param intensity_value The intensity values.
#'
#' @return the lactate associated with the estimated lactate threshold
#' @keywords internal
retrieve_lactate <- function(curve, intensity_value) {
  round(curve$predict(intensity_value), digits = 1)
}

#' Retrieve heart rate
#'
#' Heart rate at given intensities, interpolated linearly between the measured stages. A
#' straight line through the whole test misses the bend of the heart-rate curve at high
#' intensities (2-6 bpm on `demo_data`). Outside the stages (e.g. below the first one) it is `NA`.
#'
#' @param raw_data The raw data, with a `heart_rate` column.
#' @param intensity_value The intensity values.
#'
#' @return the heart rate (bpm, rounded) at each intensity.
#' @keywords internal
#' @importFrom stats approx
retrieve_heart_rate <- function(raw_data, intensity_value) {
  stages <- raw_data[raw_data$intensity > 0 & !is.na(raw_data$heart_rate), ]
  if (nrow(stages) < 2) return(rep(NA_real_, length(intensity_value)))
  round(approx(stages$intensity, stages$heart_rate, xout = intensity_value)$y)
}
