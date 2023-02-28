#' Dmax
#'
#' It applies the `Dmax` methods: `"Dmax"`, `"ModDmax"`, `"Exp-Dmax"`, `"Log-Poly-ModDmax"`, and `"Log-Exp-ModDmax"`.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method - This is going to be used in the `Log-Poly-ModDmax` and `Log-Exp-ModDmax` methods only.
#' For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' ## Fit
#'
#' The `method_dmax()` function does not have a `fit` argument because all the `Dmax` methods have their own default fitting methods:
#'
#' * Dmax = 3rd degree polynomial
#' * ModDmax = 3rd degree polynomial
#' * Exp-Dmax = exponential
#' * Log-Poly-ModDmax = 3rd degree polynomial
#' * Log-Exp-ModDmax = exponential
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
method_dmax <- function(data_prepared, sport, loglog_restrainer = 1, plot) {

  data_processed <- dplyr::tibble(method = c("Dmax", "ModDmax", "Exp-Dmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      results = switch (
        method,
        "Dmax" = helper_dmax_dmax(data_prepared = data_prepared, sport = sport, plot = plot) %>% list(),
        "ModDmax" = helper_dmax_moddmax(data_prepared = data_prepared, sport = sport, plot = plot) %>% list(),
        "Exp-Dmax" = helper_dmax_exp_dmax(data_prepared = data_prepared, sport = sport, plot = plot) %>% list(),
        "Log-Poly-ModDmax" = helper_dmax_log_poly_moddmax(data_prepared = data_prepared, sport = sport,  loglog_restrainer = loglog_restrainer, plot = plot) %>% list(),
        "Log-Exp-ModDmax" = helper_dmax_log_exp_moddmax(data_prepared = data_prepared, sport = sport,  loglog_restrainer = loglog_restrainer, plot = plot) %>% list()
      )
    ) %>%
    dplyr::ungroup()

  if(plot) {
    out <- data_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        plot = list(suppressWarnings(plot_lactate(data_processed = results, method = method)))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = results) %>%
      dplyr::select(dplyr::any_of(c("method", "fitting", "intensity", "lactate", "heart_rate", "plot")))
  } else {
    out <- data_processed %>%
      tidyr::unnest(cols = results) %>%
      dplyr::select(dplyr::any_of(c("method", "fitting", "intensity", "lactate", "heart_rate")))
  }

  if(sport == "swimming") {
    out <- out %>%
      dplyr::mutate(pace = convert_to_pace(speed = intensity))
  }

  out <- out %>%
    dplyr::select(dplyr::any_of(c("method", "fitting", "intensity", "pace", "lactate", "heart_rate", "plot"))) %>%
    dplyr::mutate(method = factor(method, levels = c("Dmax", "ModDmax", "Exp-Dmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax"))) %>%
    dplyr::arrange(method)

  out
}

#' Helper - Dmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @keywords internal
helper_dmax_dmax <- function(data_prepared, sport, plot) {

  data_prepared_dmax <- prepare_fit(
    .data = data_prepared$data[[1]],
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = {{ sport }}
  )

  model_coefficients <- broom::tidy(data_prepared_dmax$model[[1]]) %>%
    dplyr::pull(estimate)

  data_dmax <- data_prepared_dmax$data[[1]][-1, ]

  ## get the difference between the first and last lactate and intensity values
  diff_lactate <- range(data_dmax$lactate) %>% diff()
  diff_intensity <- range(data_dmax$intensity) %>% diff()

  ## get the maximal intensity
  max_intensity <- max(data_dmax$intensity)

  ## the following was based on Lactate-R (http://www.uiginn.com/lactate/lactate-r.html)

  lin_beta <- diff_lactate / diff_intensity

  ### find where the first derivative of the polynomial fit equals the slope of the line
  d_max <- c(model_coefficients[2] - lin_beta, 2 * model_coefficients[3], 3 * model_coefficients[4]) %>%
    polyroot() %>%
    Re()

  model_intensity <- max(d_max[d_max <= max_intensity])
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 5) {
    model_intensity <- min(d_max[d_max <= max_intensity])
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
  }

  if(plot) {
    ## get data for using in plot later on
    data_plot_line <- data_dmax %>%
      dplyr::filter(dplyr::row_number() %in% c(1, dplyr::n()))

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate,
        data_plot_line = list(data_plot_line)
      )

  } else {

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate
      )

  }

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
      )
  }

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
}

#' Helper - ModDmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @keywords internal
#' @importFrom utils head tail
helper_dmax_moddmax <- function(data_prepared, sport, plot) {

  data_dmax <- data_prepared$data[[1]][-1, ]

  ## find the first rise in blood lactate greater than 0.4 mmol/L
  data_first_rise <- data_dmax %>%
    dplyr::mutate(diffs = dplyr::lead(lactate) - lactate) %>%
    dplyr::filter(diffs >= 0.4) %>%
    head(1)

  if(nrow(data_first_rise) == 0) {
    out <- dplyr::tibble(
      fitting = "3rd degree polynomial (default)",
      intensity = NA,
      lactate = NA
    )

    return(out)
  }

  data_prepared_dmax <- prepare_fit_dmax_mods(
    .data = data_prepared$data[[1]],
    fit = "3rd degree polynomial",
    intensity_to_start = data_first_rise$intensity,
    sport = {{ sport }}
  )

  model_coefficients <- broom::tidy(data_prepared_dmax$model[[1]]) %>%
    dplyr::pull(estimate)

  ## get the difference between the first rise greater than 0.4 mmol/L and last lactate and intensity values
  diff_lactate <- max(data_dmax$lactate) - data_first_rise$lactate
  diff_intensity <- max(data_dmax$intensity) - data_first_rise$intensity

  ## get the maximal intensity
  max_intensity <- max(data_dmax$intensity)

  ## the following was based on Lactate-R (http://www.uiginn.com/lactate/lactate-r.html)

  lin_beta <- diff_lactate / diff_intensity

  ### find where the first derivative of the polynomial fit equals the slope of the line
  d_max <- c(model_coefficients[2] - lin_beta, 2 * model_coefficients[3], 3 * model_coefficients[4]) %>%
    polyroot() %>%
    Re()

  model_intensity <- max(d_max[d_max <= max_intensity])
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 5) {
    model_intensity <- min(d_max[d_max <= max_intensity])
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
  }

  if(plot) {
    ## get data for using in plot later on
    data_plot_line <- data_dmax %>%
      tail(1) %>%
      dplyr::bind_rows(data_first_rise, .) %>%
      dplyr::select(-diffs)

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate,
        data_plot_line = list(data_plot_line)
      )

  } else {

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate
      )

  }

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
      )
  }

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
}

#' Helper - Exp-Dmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @keywords internal
#' @importFrom utils head tail
helper_dmax_exp_dmax <- function(data_prepared, sport, plot) {

  data_prepared_dmax <- prepare_fit(
    .data = data_prepared$data[[1]],
    fit = "Exponential",
    include_baseline = FALSE,
    sport = {{ sport }}
  )

  model_coefficients <- broom::tidy(data_prepared_dmax$model[[1]]) %>%
    dplyr::pull(estimate)

  data_dmax <- data_prepared_dmax$data[[1]][-1, ]

  exponential_dmax <- function(c, si, sf) {
    log((exp(c * sf) - exp(c * si)) / ((c * sf) - (c * si))) / c
  }

  first_intensity <- head(data_dmax$intensity, 1)
  last_intensity <- tail(data_dmax$intensity, 1)

  model_intensity <- exponential_dmax(c = model_coefficients[3], si = first_intensity, sf = last_intensity)

  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  if(plot) {
    ## get data for using in plot later on
    data_plot_line <- data_dmax %>%
      dplyr::filter(dplyr::row_number() %in% c(1, dplyr::n()))

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "Exponential (default)",
        intensity = model_intensity,
        lactate = model_lactate,
        data_plot_line = list(data_plot_line)
      )

  } else {

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "Exponential (default)",
        intensity = model_intensity,
        lactate = model_lactate
      )

  }

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
      )
  }

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
}

#' Helper - Log-Poly-ModDmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @keywords internal
#' @importFrom utils tail
helper_dmax_log_poly_moddmax <- function(data_prepared, sport, loglog_restrainer, plot) {

  ## calculate Log-log
  results_loglog <- method_loglog(data_prepared = {{ data_prepared }}, fit = "3rd degree polynomial", sport = {{ sport }}, loglog_restrainer = {{ loglog_restrainer }}, plot = FALSE)

  ## prepare data again, but this time only with the necessary data for the Dmax fit
  data_prepared_dmax <- prepare_fit_dmax_mods(
    .data = data_prepared$data[[1]],
    fit = "3rd degree polynomial",
    intensity_to_start = results_loglog$intensity,
    sport = {{ sport }}
  )

  model_coefficients <- broom::tidy(data_prepared_dmax$model[[1]]) %>%
    dplyr::pull(estimate)

  data_dmax <- data_prepared_dmax$data[[1]][-1, ]

  ## get the last lactate and intensity values
  last_lactate <- tail(data_dmax$lactate, 1)
  last_intensity <- tail(data_dmax$intensity, 1)

  ## get the maximal intensity
  max_intensity <- max(data_dmax$intensity)

  ## the following was based on Lactate-R (http://www.uiginn.com/lactate/lactate-r.html)

  lin_beta <- (last_lactate - results_loglog$lactate) / (last_intensity - results_loglog$intensity)

  ### find where the first derivative of the polynomial fit equals the slope of the line
  d_max <- c(model_coefficients[2] - lin_beta, 2 * model_coefficients[3], 3 * model_coefficients[4]) %>%
    polyroot() %>%
    Re()

  model_intensity <- max(d_max[d_max <= max_intensity])
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 5) {
    model_intensity <- min(d_max[d_max <= max_intensity])
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
  }

  if(plot) {
    ## get data for using in plot later on
    data_plot_line <- data_dmax %>%
      tail(1) %>%
      dplyr::bind_rows(dplyr::select(results_loglog, lactate, intensity), .)

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate,
        data_plot_line = list(data_plot_line)
      )

  } else {

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = model_intensity,
        lactate = model_lactate
      )

  }

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
      )
  }

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
}

#' Helper - Log-Exp-ModDmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared The data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#'
#' @keywords internal
#' @importFrom utils tail
helper_dmax_log_exp_moddmax <- function(data_prepared, sport, loglog_restrainer, plot) {

  ## calculate Log-log
  results_loglog <- method_loglog(data_prepared = {{ data_prepared }}, fit = "Exponential", sport = {{ sport }}, loglog_restrainer = {{ loglog_restrainer }}, plot = FALSE)

  ## prepare data again, but this time only with the necessary data for the Dmax fit
  data_prepared_dmax <- prepare_fit_dmax_mods(
    .data = data_prepared$data[[1]],
    fit = "Exponential",
    intensity_to_start = results_loglog$intensity,
    sport = {{ sport }}
  )

  model_coefficients <- broom::tidy(data_prepared_dmax$model[[1]]) %>%
    dplyr::pull(estimate)

  data_dmax <- data_prepared_dmax$data[[1]][-1, ]

  exponential_dmax <- function(c, si, sf) {
    log((exp(c * sf) - exp(c * si)) / ((c * sf) - (c * si))) / c
  }


  ## get last intensity
  last_intensity <- tail(data_dmax$intensity, 1)

  model_intensity <- exponential_dmax(c = model_coefficients[3], si = results_loglog$intensity, sf = last_intensity)

  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  if(plot) {
    ## get data for using in plot later on
    data_plot_line <- data_dmax %>%
      tail(1) %>%
      dplyr::bind_rows(dplyr::select(results_loglog, lactate, intensity), .)

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "Exponential (default)",
        intensity = model_intensity,
        lactate = model_lactate,
        data_plot_line = list(data_plot_line)
      )

  } else {

    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "Exponential (default)",
        intensity = model_intensity,
        lactate = model_lactate
      )

  }

  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = retrieve_heart_rate(raw_data = data_prepared$data[[1]], intensity_value = intensity)
      )
  }

  switch (
    sport,
    "cycling" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 1)),
    "running" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 2)),
    "swimming" = out <- out %>% dplyr::mutate(intensity = round(x = intensity, digits = 3))
  )

  out
}

