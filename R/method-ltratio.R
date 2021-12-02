#' Minimum Lactate-Intensity Ratio (LTratio)
#'
#' It applies the `LTratio` method.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @return a [tibble][tibble::tibble-package] with the following columns:
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{heart_rate}{the heart rate associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @keywords internal
method_ltratio <- function(data_prepared, fit, sport, plot) {

  data_prepared_ltratio <- prepare_fit(
    .data = data_prepared$data[[1]],
    fit = "B-spline",
    include_baseline = data_prepared$bsln,
    sport = {{ sport }}
  )

  data_augmented <- data_prepared_ltratio$data_augmented[[1]]

  ## get min ratio
  data_min_ratio <- data_augmented %>%
    dplyr::mutate(ratio = .fitted / intensity) %>%
    dplyr::slice(which.min(ratio))

  data_processed <- data_prepared %>%
    dplyr::mutate(
      method = "LTratio",
      fitting = "B-Spline (default)",
      intensity = data_min_ratio$intensity,
      lactate = round(x = data_min_ratio$.fitted, digits = 1)
    ) %>%
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
