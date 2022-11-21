#' Individual Anaerobic Threshold (IAT)
#'
#' It applies the `IAT` method.
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
method_iat <- function(data_prepared, fit, sport, plot) {
  ## calculate LTratio
  results_ltratio <- method_ltratio(
    data_prepared = {{data_prepared}},
    fit = {{fit}},
    sport = {{sport}},
    plot = FALSE
  )
  ## calculate IAT
  data_processed <- dplyr::tibble(
    method = c("IAT (+1.0 mmol/L)", "IAT (+1.5 mmol/L)"),
    lactate = c(results_ltratio$lactate + 1, results_ltratio$lactate + 1.5)
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      results = obla_helper(
        .data = data_prepared$data[[1]],
        data_augmented = data_prepared$data_augmented[[1]],
        fit = fit,
        model = data_prepared$model[[1]],
        obla = lactate
      ) %>% list()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-lactate) %>%
    tidyr::unnest(cols = results) %>%
    dplyr::bind_cols(data = data_prepared) %>%
    dplyr::mutate(fitting = paste(fit, "(user-defined)", sep = " ")) %>%
    tidyr::nest(data = -method)

  if(plot) {
    out <- data_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        plot = list(plot_lactate(data_processed = data, method = method))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = data)
  } else {
    out <- data_processed %>%
      tidyr::unnest(cols = data)
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
