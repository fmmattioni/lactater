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
  model_heart_rate <- NULL
  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    model_heart_rate <- build_heart_rate_model(raw_data = data_prepared$data[[1]])
  }

  data_for_segmented <- data_prepared$data_augmented[[1]]
  if(nrow(data_for_segmented) < 5) {
    stop("LTP method requires at least 5 data points from the model fit.", call. = FALSE)
  }

  linear_fit <- lm(.fitted ~ intensity, data = data_for_segmented)

  segmented_fit <- tryCatch(
    segmented::segmented(
      obj = linear_fit,
      npsi = 2 # 2 breakpoints
    ),
    error = function(e) {
      stop("LTP breakpoint estimation failed. Please verify that your test contains enough progressive lactate increase.", call. = FALSE)
    }
  )

  method_intensity <- segmented_fit$psi[,2]

  out <- data_prepared %>%
    tidyr::crossing(
      method = c("LTP1", "LTP2"),
      fitting = paste({{ fit }}, "(user-defined)", sep = " ")
    ) %>%
    dplyr::mutate(
      intensity = method_intensity,
      lactate = unlist(Map(retrieve_lactate, model = model, intensity_value = intensity))
    ) %>%
    dplyr::mutate(segmented_object = list(segmented_fit))

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

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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
