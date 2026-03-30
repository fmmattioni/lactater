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
  model_heart_rate <- NULL
  if("heart_rate" %in% colnames(data_prepared$data[[1]])) {
    model_heart_rate <- build_heart_rate_model(raw_data = data_prepared$data[[1]])
  }
  data_prepared_poly <- prepare_fit(
    .data = data_prepared$data[[1]],
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = {{ sport }}
  )
  data_prepared_exp <- prepare_fit(
    .data = data_prepared$data[[1]],
    fit = "Exponential",
    include_baseline = FALSE,
    sport = {{ sport }}
  )
  data_dmax <- data_prepared$data[[1]][-1, ]
  data_first_rise <- data_dmax %>%
    dplyr::mutate(diffs = dplyr::lead(lactate) - lactate) %>%
    dplyr::filter(diffs >= 0.4) %>%
    head(1)

  data_prepared_mod <- if(nrow(data_first_rise) > 0) {
    prepare_fit_dmax_mods(
      .data = data_prepared$data[[1]],
      fit = "3rd degree polynomial",
      intensity_to_start = data_first_rise$intensity,
      sport = {{ sport }}
    )
  } else {
    NULL
  }

  results_loglog_poly <- method_loglog(
    data_prepared = {{ data_prepared }},
    fit = "3rd degree polynomial",
    sport = {{ sport }},
    loglog_restrainer = {{ loglog_restrainer }},
    plot = FALSE
  )
  data_prepared_log_poly <- prepare_fit_dmax_mods(
    .data = data_prepared$data[[1]],
    fit = "3rd degree polynomial",
    intensity_to_start = results_loglog_poly$intensity,
    sport = {{ sport }}
  )

  results_loglog_exp <- method_loglog(
    data_prepared = {{ data_prepared }},
    fit = "Exponential",
    sport = {{ sport }},
    loglog_restrainer = {{ loglog_restrainer }},
    plot = FALSE
  )
  data_prepared_log_exp <- prepare_fit_dmax_mods(
    .data = data_prepared$data[[1]],
    fit = "Exponential",
    intensity_to_start = results_loglog_exp$intensity,
    sport = {{ sport }}
  )

  methods <- c("Dmax", "ModDmax", "Exp-Dmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax")
  data_processed <- dplyr::tibble(
    method = methods,
    results = lapply(methods, function(method) {
      switch (
        method,
        "Dmax" = helper_dmax_dmax(data_prepared_dmax = data_prepared_poly, data_raw = data_prepared$data[[1]], sport = sport, plot = plot, model_heart_rate = model_heart_rate),
        "ModDmax" = helper_dmax_moddmax(data_prepared = data_prepared, data_first_rise = data_first_rise, data_prepared_dmax = data_prepared_mod, sport = sport, plot = plot, model_heart_rate = model_heart_rate),
        "Exp-Dmax" = helper_dmax_exp_dmax(data_prepared_dmax = data_prepared_exp, data_raw = data_prepared$data[[1]], sport = sport, plot = plot, model_heart_rate = model_heart_rate),
        "Log-Poly-ModDmax" = helper_dmax_log_poly_moddmax(data_prepared = data_prepared, results_loglog = results_loglog_poly, data_prepared_dmax = data_prepared_log_poly, sport = sport, plot = plot, model_heart_rate = model_heart_rate),
        "Log-Exp-ModDmax" = helper_dmax_log_exp_moddmax(data_prepared = data_prepared, results_loglog = results_loglog_exp, data_prepared_dmax = data_prepared_log_exp, sport = sport, plot = plot, model_heart_rate = model_heart_rate)
      )
    })
  )

  if(plot) {
    out <- data_processed %>%
      dplyr::mutate(
        plot = Map(function(result, method) suppressWarnings(plot_lactate(data_processed = result, method = method)), results, method)
      ) %>%
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

  ## attach per-method fit statistics (polynomial for poly methods, exponential for exp methods)
  poly_fs <- if ("fit_stats" %in% names(data_prepared_poly)) data_prepared_poly$fit_stats[[1]] else NULL
  exp_fs  <- if ("fit_stats" %in% names(data_prepared_exp))  data_prepared_exp$fit_stats[[1]]  else NULL

  out$fit_rmse <- NA_real_
  out$fit_r_squared <- NA_real_
  out$fit_see <- NA_real_
  out$fit_mae <- NA_real_
  out$fit_n_points <- NA_integer_
  out$fit_type <- NA_character_

  for (i in seq_len(nrow(out))) {
    m <- as.character(out$method[i])
    if (m %in% c("Exp-Dmax", "Log-Exp-ModDmax") && !is.null(exp_fs)) {
      out$fit_rmse[i]      <- exp_fs$rmse
      out$fit_r_squared[i] <- exp_fs$r_squared
      out$fit_see[i]       <- exp_fs$see
      out$fit_mae[i]       <- exp_fs$mae
      out$fit_n_points[i]  <- exp_fs$n_points
      out$fit_type[i]      <- "Exponential"
    } else if (!is.null(poly_fs)) {
      out$fit_rmse[i]      <- poly_fs$rmse
      out$fit_r_squared[i] <- poly_fs$r_squared
      out$fit_see[i]       <- poly_fs$see
      out$fit_mae[i]       <- poly_fs$mae
      out$fit_n_points[i]  <- poly_fs$n_points
      out$fit_type[i]      <- "3rd degree polynomial"
    }
  }

  out
}

#' Helper - Dmax
#'
#' For internal use only and it won't be exported.
#'
#' @param data_prepared_dmax The prepared polynomial fit for Dmax.
#' @param data_raw The raw data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param model_heart_rate A pre-fit heart-rate model.
#'
#' @keywords internal
helper_dmax_dmax <- function(data_prepared_dmax, data_raw, sport, plot, model_heart_rate = NULL) {

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
  d_max <- d_max[d_max > 0]

  valid_candidates <- d_max[d_max <= max_intensity]
  if(length(valid_candidates) == 0) {
    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = NA_real_,
        lactate = NA_real_
      )
    return(out)
  }

  model_intensity <- max(valid_candidates)
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 8 && length(valid_candidates) > 1) {
    model_intensity <- min(valid_candidates)
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
    warning("Dmax estimate exceeded plausible lactate range; lower root selected.", call. = FALSE)
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

  if("heart_rate" %in% colnames(data_raw)) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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
#' @param data_first_rise The first rise in lactate data point used for ModDmax.
#' @param data_prepared_dmax The prepared polynomial fit for ModDmax.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param model_heart_rate A pre-fit heart-rate model.
#'
#' @keywords internal
#' @importFrom utils head tail
helper_dmax_moddmax <- function(data_prepared, data_first_rise, data_prepared_dmax, sport, plot, model_heart_rate = NULL) {

  data_dmax <- data_prepared$data[[1]][-1, ]

  if(nrow(data_first_rise) == 0 || is.null(data_prepared_dmax)) {
    out <- dplyr::tibble(
      fitting = "3rd degree polynomial (default)",
      intensity = NA,
      lactate = NA
    )

    return(out)
  }

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
  d_max <- d_max[d_max > 0]

  valid_candidates <- d_max[d_max <= max_intensity]
  if(length(valid_candidates) == 0) {
    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = NA_real_,
        lactate = NA_real_
      )
    return(out)
  }

  model_intensity <- max(valid_candidates)
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 8 && length(valid_candidates) > 1) {
    model_intensity <- min(valid_candidates)
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
    warning("Modified Dmax estimate exceeded plausible lactate range; lower root selected.", call. = FALSE)
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
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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
#' @param data_prepared_dmax The prepared exponential fit for Exp-Dmax.
#' @param data_raw The raw data retrieved from `prepare_data()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param model_heart_rate A pre-fit heart-rate model.
#'
#' @keywords internal
#' @importFrom utils head tail
helper_dmax_exp_dmax <- function(data_prepared_dmax, data_raw, sport, plot, model_heart_rate = NULL) {

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

  if("heart_rate" %in% colnames(data_raw)) {
    out <- out %>%
      dplyr::mutate(
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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
#' @param results_loglog Results from the Log-log method using polynomial fit.
#' @param data_prepared_dmax The prepared polynomial fit starting at Log-log intensity.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param model_heart_rate A pre-fit heart-rate model.
#'
#' @keywords internal
#' @importFrom utils tail
helper_dmax_log_poly_moddmax <- function(data_prepared, results_loglog, data_prepared_dmax, sport, plot, model_heart_rate = NULL) {

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
  d_max <- d_max[d_max > 0]

  valid_candidates <- d_max[d_max <= max_intensity]
  if(length(valid_candidates) == 0) {
    out <- data_prepared_dmax %>%
      dplyr::mutate(
        fitting = "3rd degree polynomial (default)",
        intensity = NA_real_,
        lactate = NA_real_
      )
    return(out)
  }

  model_intensity <- max(valid_candidates)
  model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)

  ## this is a workaround to avoid unplausible estimations
  ## a more elegant and definitive solution needs to be done
  if(model_lactate > 8 && length(valid_candidates) > 1) {
    model_intensity <- min(valid_candidates)
    model_lactate <- retrieve_lactate(model = data_prepared_dmax$model[[1]], intensity_value = model_intensity)
    warning("Log-Poly-ModDmax estimate exceeded plausible lactate range; lower root selected.", call. = FALSE)
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
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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
#' @param results_loglog Results from the Log-log method using exponential fit.
#' @param data_prepared_dmax The prepared exponential fit starting at Log-log intensity.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param model_heart_rate A pre-fit heart-rate model.
#'
#' @keywords internal
#' @importFrom utils tail
helper_dmax_log_exp_moddmax <- function(data_prepared, results_loglog, data_prepared_dmax, sport, plot, model_heart_rate = NULL) {

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
        heart_rate = round(stats::predict(model_heart_rate, newdata = data.frame(intensity = intensity)), digits = 0)
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

