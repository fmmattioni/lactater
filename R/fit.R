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
#' @return a [tibble][tibble::tibble-package] with the following nested columns:
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
        model = minpack.lm::nlsLM(
          formula = lactate ~ a + (b * exp(c * intensity)),
          data = data_for_modeling,
          start = list(a = 0, b = 1, c = 0),
          control = list(maxiter = 1000)
        ) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  }

  out <- out %>%
    dplyr::mutate(bsln = include_baseline)

  out
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
#' @return a [tibble][tibble::tibble-package] with the following nested columns:
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
        model = minpack.lm::nlsLM(
          formula = lactate ~ a + (b * exp(c * intensity)),
          data = data_for_modeling,
          start = list(a = 0, b = 1, c = 0),
          control = list(maxiter = 1000)
        ) %>% list(),
        data_augmented = broom::augment(model, newdata = new_data_model, type.predict = "response") %>% list()
      ) %>%
      dplyr::ungroup()

  }

  out <- out %>%
    dplyr::mutate(bsln = FALSE)

  out
}

