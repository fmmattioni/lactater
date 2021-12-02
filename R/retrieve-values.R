#' Retrieve intensity
#'
#' Retrieves the intensity associated to a given lactate value.
#' It is mainly for internal use but it is exported for possible extensions.
#'
#' @param data_augmented The augmented data from the model.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param model The model chosen in `fit`.
#' @param lactate_value The lactate value.
#'
#' @return the intensity associated with the estimated lactate threshold
#' @keywords internal
#' @importFrom stats splinefun approx
#' @importFrom utils tail
retrieve_intensity <- function(data_augmented, fit, model, lactate_value) {

  if(fit == "B-spline") {

    # spline function
    f1 <- splinefun(data_augmented$intensity, data_augmented$.fitted)

    # get intensity
    method_intensity <- pracma::findzeros(
      function(x)
        f1(x) - lactate_value, min(data_augmented$intensity), max(data_augmented$intensity)
      )

  } else {

    method_intensity <- approx(
      x = model$fitted.values,
      y = model$model[, 2][, 1],
      xout = lactate_value, ties = "ordered"
    )$y

    ## the following is necessary in case the lactate curve goes down and then increases again
    ## if the above is NA, it means that two values were found, so the following will retrieve the higher value
    if(is.na(method_intensity)) {

      ## find where lactate curve start to increase again after going down
      start_increasing <- dplyr::tibble(.fitted = model$fitted.values) %>%
        dplyr::mutate(rowid = seq_along(.fitted), .before = 1) %>%
        dplyr::mutate(diffs = .fitted - dplyr::lag(.fitted)) %>%
        tidyr::drop_na() %>%
        dplyr::filter(diffs < 0) %>%
        dplyr::pull(rowid) %>%
        tail(1)

      n_row_data <- length(model$fitted.values)

      method_intensity <- approx(
        x = model$fitted.values[start_increasing:n_row_data],
        y = model$model[, 2][, 1][start_increasing:n_row_data],
        xout = lactate_value, ties = "ordered"
      )$y
    }

  }

  ## this will make sure that if two values were found, the highest value is retrieved
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
#' Retrieves the heart rate associated to a given intensity value.
#' It is mainly for internal use but it is exported for possible extensions.
#'
#' @param raw_data The raw data.
#' @param intensity_value The intensity value.
#'
#' @return the heart rate associated with the estimated lactate threshold
#' @keywords internal
#' @importFrom stats predict.lm
retrieve_heart_rate <- function(raw_data, intensity_value) {

  ## remove baseline value
  raw_data <- raw_data[-1, ]

  ## linear model heart rate vs intensity
  model_heart_rate <- lm(heart_rate ~ intensity, data = raw_data)

  ## predict heart rate based on intensity value
  out <- predict.lm(model_heart_rate, newdata = data.frame(intensity = intensity_value))

  out <- round(x = out, digits = 0)

  out
}
