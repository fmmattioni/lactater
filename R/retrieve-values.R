#' Retrieve intensity
#'
#' Retrieves the intensity at which the fitted lactate curve reaches a given lactate value,
#' by solving the fitted model (not by interpolating between fitted points, which
#' under-estimates the intensity on a convex curve, see issue #8).
#'
#' @param data_augmented The augmented data from the model (a fine intensity grid with `.fitted`).
#' @param model The model chosen in `fit`.
#' @param lactate_value The lactate value.
#'
#' @return the intensity associated with the estimated lactate threshold, or `NA` when the
#' curve never reaches `lactate_value` within the tested range.
#' @keywords internal
#' @importFrom stats predict uniroot
retrieve_intensity <- function(data_augmented, model, lactate_value) {
  x <- data_augmented$intensity
  gap <- data_augmented$.fitted - lactate_value
  crossings <- which(gap[-1] * gap[-length(gap)] <= 0)
  if (length(crossings) == 0) return(NA_real_)

  ## the curve can cross the target more than once (early lactate dip): keep the highest intensity
  i <- max(crossings)
  uniroot(
    function(v) predict(model, newdata = data.frame(intensity = v)) - lactate_value,
    lower = x[i], upper = x[i + 1], tol = 1e-8
  )$root
}

#' Retrieve lactate
#'
#' Retrieves the lactate associated to a given intensity value.
#' It is mainly for internal use but it is exported for possible extensions.
#'
#' @param model The model chosen in `fit`.
#' @param intensity_value The intensity value.
#'
#' @return the lactate associated with the estimated lactate threshold
#' @keywords internal
#' @importFrom stats predict
retrieve_lactate <- function(model, intensity_value) {

  new_data <- data.frame(intensity = intensity_value)

  out <- predict(object = model, newdata = new_data)

  out <- round(x = out, digits = 1)

  out

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

