#' Prepare fit
#'
#' This is an obligatory step before applying any lactate threshold method.
#' This function will model the raw data as well as make all the necessary data wrangling.
#'
#' @param .data The data retrieved from `prepare_data()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' Please, note that a few lactate thresholds have default methods for this and cannot be changed. See `Details`.
#' @param include_baseline A boolean to indicate whether to include the baseline value in the fit.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @return a tibble with the following nested columns:
#' \item{data}{The raw data.}
#' \item{data_interpolated}{The interpolated data.}
#' \item{model}{The model chosen in the `fit` parameter.}
#' \item{data_augmented}{The augmented data retrieved from the model.}
#' \item{bsln}{A boolean indicating the `include_baseline` argument.}
#'
#' @keywords internal
#'
#' @importFrom stats lm glm
prepare_fit <- function(
  .data,
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline", "Exponential"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming")
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  if(!any(colnames(.data) %in% c("intensity", "lactate")))
    stop("It looks like you didn't prepare your data. Please, call `prepare_data()` before.", call. = FALSE)
  if(!all(c("intensity", "lactate") %in% colnames(.data)))
    stop("The data must contain both `intensity` and `lactate` columns.", call. = FALSE)
  if(nrow(.data) < 3)
    stop("At least 3 rows are required to fit lactate curves reliably.", call. = FALSE)
  if(anyNA(.data$intensity) || anyNA(.data$lactate))
    stop("Missing values in `intensity`/`lactate` are not supported. Please remove or impute NAs first.", call. = FALSE)
  if(!is.numeric(.data$intensity) || !is.numeric(.data$lactate))
    stop("`intensity` and `lactate` columns must be numeric.", call. = FALSE)
  if(any(diff(.data$intensity) <= 0))
    stop("`intensity` must be strictly increasing.", call. = FALSE)

  fit <- match.arg(arg = fit)
  sport <- match.arg(arg = sport)

  ## here we take the baseline intensity
  ## and create it as a previous step of step 1
  ## this is mainly important to correctly plot the baseline value as the previous step of step 1
  to_subtract <- .data$intensity[3] - .data$intensity[2]
  .data[.data$intensity == 0, ]$intensity <- .data$intensity[2] - to_subtract


  switch (
    sport,
    "cycling" = interpolation_factor <- 0.1,
    "running" = interpolation_factor <- 0.1,
    "swimming" = interpolation_factor <- 0.01
  )

  if(include_baseline) {
    data_for_modeling <- .data
  } else {
    data_for_modeling <- .data[-1, ]
  }

  data_pre_processed <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data_interpolated = interpolate_intensity(.data = data_for_modeling, interpolation_factor = interpolation_factor) %>% list()) %>%
    dplyr::ungroup()

  new_data_model <- data.frame(intensity = seq(min(data_for_modeling$intensity), max(data_for_modeling$intensity), interpolation_factor))

  if(fit == "3rd degree polynomial") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = lm(lactate ~ poly(intensity, degree = 3, raw = TRUE), data = data_for_modeling) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  } else if(fit == "4th degree polynomial") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = lm(lactate ~ poly(intensity, degree = 4, raw = TRUE), data = data_for_modeling) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  } else if(fit == "B-spline") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = glm(lactate ~ splines::ns(intensity, 4), data = data_for_modeling) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  } else if(fit == "Exponential") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = tryCatch(
          minpack.lm::nlsLM(
            formula = lactate ~ a + (b * exp(c * intensity)),
            data = data_for_modeling,
            start = list(a = 0, b = 1, c = 0),
            control = list(maxiter = 1000)
          ),
          error = function(e) {
            stop("Exponential fit failed to converge. Try another fit method or verify your input data.", call. = FALSE)
          }
        ) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  }

  ## compute goodness-of-fit statistics (residuals evaluated on raw data points)
  out <- out %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      fit_stats = compute_fit_stats(model, data, bsln = include_baseline) %>% list()
    ) %>%
    dplyr::ungroup()

  out <- out %>%
    dplyr::mutate(bsln = include_baseline)

  out
}

#' Compute goodness-of-fit statistics for a lactate model
#'
#' Evaluates how well the fitted curve represents the measured data points
#' by computing residuals, RMSE, R², and SEE on the raw (non-interpolated) data.
#'
#' @param model The fitted model object (lm, glm, or nls).
#' @param raw_data The raw data tibble (from `data` column in prepare_fit output).
#' @param bsln Whether baseline was included in fitting.
#'
#' @return A tibble with one row containing:
#' \item{n_points}{Number of data points used for evaluation.}
#' \item{rmse}{Root mean square error (mmol/L).}
#' \item{r_squared}{Coefficient of determination.}
#' \item{see}{Standard error of estimate (mmol/L).}
#' \item{mae}{Mean absolute error (mmol/L).}
#' \item{max_abs_error}{Maximum absolute residual (mmol/L).}
#' \item{residuals}{List column with individual residuals per data point.}
#'
#' @keywords internal
compute_fit_stats <- function(model, raw_data, bsln = FALSE) {
  if (is.null(model) || is.null(raw_data)) {
    return(dplyr::tibble(
      n_points = NA_integer_, rmse = NA_real_, r_squared = NA_real_,
      see = NA_real_, mae = NA_real_, max_abs_error = NA_real_,
      residuals = list(NULL)
    ))
  }

  # Use same data subset as fitting (with or without baseline)
  if (bsln) {
    eval_data <- raw_data
  } else {
    eval_data <- raw_data[-1, ]
  }

  # Predict on raw data points
  predicted <- tryCatch(
    stats::predict(model, newdata = data.frame(intensity = eval_data$intensity)),
    error = function(e) rep(NA_real_, nrow(eval_data))
  )

  residuals <- eval_data$lactate - predicted
  n <- length(residuals)

  if (n < 2 || all(is.na(residuals))) {
    return(dplyr::tibble(
      n_points = n, rmse = NA_real_, r_squared = NA_real_,
      see = NA_real_, mae = NA_real_, max_abs_error = NA_real_,
      residuals = list(residuals)
    ))
  }

  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((eval_data$lactate - mean(eval_data$lactate, na.rm = TRUE))^2, na.rm = TRUE)
  n_valid <- sum(!is.na(residuals))
  p <- length(stats::coef(model))  # number of parameters

  rmse <- sqrt(ss_res / n_valid)
  r_squared <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)
  see <- ifelse(n_valid > p, sqrt(ss_res / (n_valid - p)), NA_real_)
  mae <- mean(abs(residuals), na.rm = TRUE)
  max_abs_error <- max(abs(residuals), na.rm = TRUE)

  dplyr::tibble(
    n_points = n_valid,
    rmse = rmse,
    r_squared = r_squared,
    see = see,
    mae = mae,
    max_abs_error = max_abs_error,
    residuals = list(residuals)
  )
}

#' Prepare modified Dmax fits
#'
#' This is a function for internal use only and it won't be exported.
#'
#' @param .data The data retrieved from `prepare_data()`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' Please, note that a few lactate thresholds have default methods for this and cannot be changed. See `Details`.
#' @param intensity_to_start A double indicating the intensity to start the fit.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @return a tibble with the following nested columns:
#' \item{data}{The raw data.}
#' \item{data_interpolated}{The interpolated data.}
#' \item{model}{The model chosen in the `fit` parameter.}
#' \item{data_augmented}{The augmented data retrieved from the model.}
#' \item{bsln}{A boolean indicating the `include_baseline` argument.}
#'
#' @keywords internal
#' @importFrom stats lm
prepare_fit_dmax_mods <- function(
  .data,
  fit = c("3rd degree polynomial", "Exponential"),
  intensity_to_start,
  sport = c("cycling", "running", "swimming")
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  if(!any(colnames(.data) %in% c("intensity", "lactate")))
    stop("It looks like you didn't prepare your data. Please, call `prepare_data()` before.", call. = FALSE)
  if(!all(c("intensity", "lactate") %in% colnames(.data)))
    stop("The data must contain both `intensity` and `lactate` columns.", call. = FALSE)
  if(nrow(.data) < 3)
    stop("At least 3 rows are required to fit modified Dmax curves reliably.", call. = FALSE)
  if(anyNA(.data$intensity) || anyNA(.data$lactate))
    stop("Missing values in `intensity`/`lactate` are not supported. Please remove or impute NAs first.", call. = FALSE)
  if(!is.numeric(.data$intensity) || !is.numeric(.data$lactate))
    stop("`intensity` and `lactate` columns must be numeric.", call. = FALSE)
  if(any(diff(.data$intensity) <= 0))
    stop("`intensity` must be strictly increasing.", call. = FALSE)

  if(missing(intensity_to_start))
    stop("You need to indicate at which intensity the fit should start.", call. = FALSE)

  fit <- match.arg(arg = fit)
  sport <- match.arg(arg = sport)

  ## here we take the baseline intensity
  ## and create it as a previous step of step 1
  ## this is mainly important to correctly plot the baseline value as the previous step of step 1
  to_subtract <- .data$intensity[3] - .data$intensity[2]
  .data[.data$intensity == 0, ]$intensity <- .data$intensity[2] - to_subtract


  switch (
    sport,
    "cycling" = interpolation_factor <- 0.1,
    "running" = interpolation_factor <- 0.1,
    "swimming" = interpolation_factor <- 0.01
  )

  data_for_modeling <- .data[-1, ]

  data_pre_processed <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data_interpolated = interpolate_intensity(.data = data_for_modeling, interpolation_factor = interpolation_factor) %>% list()) %>%
    dplyr::ungroup()

  new_data_model <- dplyr::tibble(intensity = seq(min(data_for_modeling$intensity), max(data_for_modeling$intensity), interpolation_factor)) %>%
    dplyr::filter(intensity >= intensity_to_start)

  if(fit == "3rd degree polynomial") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = lm(lactate ~ poly(intensity, degree = 3, raw = TRUE), data = data_for_modeling) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  } else if(fit == "Exponential") {
    out <- data_pre_processed %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = tryCatch(
          minpack.lm::nlsLM(
            formula = lactate ~ a + (b * exp(c * intensity)),
            data = data_for_modeling,
            start = list(a = 0, b = 1, c = 0),
            control = list(maxiter = 1000)
          ),
          error = function(e) {
            stop("Exponential fit for modified Dmax failed to converge.", call. = FALSE)
          }
        ) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  }

  out <- out %>%
    dplyr::mutate(bsln = FALSE)

  out
}

