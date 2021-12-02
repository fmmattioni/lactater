#' Log-log
#'
#' It applies the `Log-log` method.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
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
#' @importFrom stats lm
method_loglog <- function(data_prepared, fit, sport, loglog_restrainer = 1, plot) {

  data_interpolated <- data_prepared$data_interpolated[[1]] %>%
    dplyr::mutate(lactate = log(lactate)) %>%
    ## filter data according to loglog_restrainer
    dplyr::slice(seq(loglog_restrainer * dplyr::n()))

  linear_fit <- lm(lactate ~ intensity, data = data_interpolated)

  segmented_fit <- segmented::segmented(
    obj = linear_fit,
    npsi = 1, # 1 breakpoint
    control = segmented::seg.control(it.max = 1000)
  )

  method_intensity <- segmented_fit$psi[,2]

  data_processed <- data_prepared %>%
    dplyr::mutate(
      method = "Log-log",
      fitting = paste({{ fit }}, "(user-defined)", sep = " "),
      intensity = method_intensity
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(lactate = retrieve_lactate(model = model, intensity_value = intensity)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(segmented_object = list(segmented_fit)) %>%
    dplyr::mutate(loglog_restrainer = loglog_restrainer) %>%
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
