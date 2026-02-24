#' Onset of Blood Lactate Accumulation (OBLA)
#'
#' It applies the `OBLA` methods: `OBLA 2.0 mmol/L`, `OBLA 2.5 mmol/L`, `OBLA 3.0 mmol/L`,
#' `OBLA 3.5 mmol/L`, and `OBLA 4.0 mmol/L`.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @return a tibble with the following columns:
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{heart_rate}{the heart rate associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @keywords internal
method_obla <- function(data_prepared, fit, sport, plot) {
  model_heart_rate <- NULL
  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    model_heart_rate <- build_heart_rate_model(raw_data = data_prepared$data[[1]])
  }

  obla_values <- c(2, 2.5, 3, 3.5, 4)
  out <- dplyr::tibble(
    method = obla_values,
    results = lapply(obla_values, function(obla) {
      obla_helper(
        raw_data = data_prepared$data[[1]],
        data_augmented = data_prepared$data_augmented[[1]],
        fit = fit,
        model = data_prepared$model[[1]],
        obla = obla,
        model_heart_rate = model_heart_rate
      )
    })
  ) %>%
    dplyr::mutate(method = sprintf("%.1f", round(method, 1)),
                  method = paste("OBLA", method)) %>%
    tidyr::unnest(cols = results) %>%
    dplyr::bind_cols(data = data_prepared) %>%
    dplyr::mutate(fitting = paste(fit, "(user-defined)", sep = " "))

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

#' OBLA helper
#'
#' For internal use only and it won't be exported.
#'
#' @param .data The raw data.
#' @param data_augmented The augmented data from the model.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param model The model chosen in `fit`,
#' @param obla The lactate value to be analyzed.
#'
#' @return The results (intensity, lactate, heart rate) from the lactate threshold method.
#' @keywords internal
obla_helper <- function(
  raw_data,
  data_augmented,
  fit,
  model,
  obla,
  model_heart_rate = NULL
) {

  # check if fit is possible
  if(obla > max(model$fitted.values) || obla < min(model$fitted.values)) {
    out <- dplyr::tibble(
      intensity = NA,
      lactate = obla
    )

    return(out)
  }

  method_intensity <- retrieve_intensity(
    data_augmented = data_augmented,
    fit = fit,
    model = model,
    lactate_value = obla
  )

  out <- dplyr::tibble(
    intensity = method_intensity,
    lactate = obla
  )

  if("heart_rate" %in% colnames(raw_data)) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
      )
  }

  out
}
