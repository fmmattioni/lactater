#' Lactate Turning Point (LTP)
#'
#' It applies the `LTP` methods: `LTP1`, and `LTP2`.
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
#' @importFrom stats lm
method_ltp <- function(data_prepared, fit, sport, plot) {

  data_interpolated <- data_prepared$data_interpolated[[1]]

  linear_fit <- lm(lactate ~ intensity, data = data_interpolated)

  segmented_fit <- segmented::segmented(
    obj = linear_fit,
    npsi = 2 # 2 breakpoints
  )

  method_intensity <- segmented_fit$psi[,2]

  data_processed <- data_prepared %>%
    tidyr::crossing(
      method = c("LTP1", "LTP2"),
      fitting = paste({{ fit }}, "(user-defined)", sep = " ")
    ) %>%
    dplyr::mutate(intensity = method_intensity) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(lactate = retrieve_lactate(model = model, intensity_value = intensity)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(segmented_object = list(segmented_fit)) %>%
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

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
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
