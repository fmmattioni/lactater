#' Retrieve intensity
#'
#' Retrieves the intensity associated to a given lactate value by computing the
#' inverse of the fitted model using root-finding.
#' It is mainly for internal use but it is exported for possible extensions.
#'
#' @param data_augmented The augmented data from the model.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param model The model chosen in `fit`.
#' @param lactate_value The lactate value.
#'
#' @return the intensity associated with the estimated lactate threshold
#' @keywords internal
retrieve_intensity <- function(data_augmented, fit, model, lactate_value) {

  ## build a prediction function that returns zero at the target lactate value
  pred_fun <- function(x) {
    unname(predict(model, newdata = data.frame(intensity = x))) - lactate_value
  }

  intensity_min <- min(data_augmented$intensity)
  intensity_max <- max(data_augmented$intensity)

  ## find all roots (intensities) where the model equals the target lactate value
  method_intensity <- pracma::findzeros(pred_fun, intensity_min, intensity_max)

  if (length(method_intensity) == 0) return(NA_real_)

  ## if multiple roots found, return the highest intensity
  out <- max(method_intensity)

  out
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
#' Builds a linear heart rate model from the raw incremental test data.
#'
#' @param raw_data The raw data.
#'
#' @return a linear model (`lm`) for heart-rate prediction
#' @keywords internal
build_heart_rate_model <- function(raw_data) {

  ## remove baseline value
  raw_data <- raw_data[-1, ]

  ## linear model heart rate vs intensity
  lm(heart_rate ~ intensity, data = raw_data)
}

#' Retrieve heart rate
#'
#' Retrieves the heart rate associated to a given intensity value.
#' It is mainly for internal use but it is exported for possible extensions.
#'
#' @param raw_data The raw data.
#' @param intensity_value The intensity value.
#' @param model_heart_rate A pre-fit heart-rate model. If `NULL`, it will be fit from `raw_data`.
#'
#' @return the heart rate associated with the estimated lactate threshold
#' @keywords internal
#' @importFrom stats predict.lm
retrieve_heart_rate <- function(raw_data, intensity_value, model_heart_rate = NULL) {

  if(is.null(model_heart_rate)) {
    model_heart_rate <- build_heart_rate_model(raw_data = raw_data)
  }

  ## predict heart rate based on intensity value
  out <- predict.lm(model_heart_rate, newdata = data.frame(intensity = intensity_value))

  out <- round(x = out, digits = 0)

  out
}
