#' Lactate curve
#'
#' It retrieves the lactate curve for plotting purposes.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if applicable.
#' @param fit The fit you would like to use for plotting the lactate curve.
#' Options are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline`.
#' @param include_baseline A boolean to indicate whether to include the baseline value in the fit.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @return a list with the following elements:
#' \item{data}{a tibble containing the raw data with the columns `intensity`, `lactate`, and `heart_rate`.}
#' \item{lactate_curve}{a tibble containing the data with the columns `intensity` and `lactate` for plotting the lactate curve according to the `fit` method chosen.}
#' \item{heart_rate_response}{a tibble containing the data with the columns `intensity` and `heart_rate` for plotting the heart rate response using the linear method.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lactate_curve(
#'   .data = demo_data,
#'   intensity_column = "intensity",
#'   lactate_column = "lactate",
#'   heart_rate_column = "heart_rate",
#'   fit = "3rd degree polynomial",
#'   include_baseline = TRUE,
#'   sport = "cycling"
#' )
#' }
lactate_curve <- function(
    .data,
    intensity_column,
    lactate_column,
    heart_rate_column,
    fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
    include_baseline = FALSE,
    sport = c("cycling", "running", "swimming")
) {
  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  intensity_column <- rlang::ensym(intensity_column)
  lactate_column <- rlang::ensym(lactate_column)

  fit <- match.arg(arg = fit, several.ok = FALSE)
  sport <- match.arg(arg = sport, several.ok = FALSE)

  if(!missing(heart_rate_column)) {
    heart_rate_column <- rlang::ensym(heart_rate_column)
    data_prepared <- prepare_data(
      .data = .data,
      intensity_column = {{ intensity_column }},
      lactate_column = {{ lactate_column }},
      heart_rate_column = {{ heart_rate_column }}
    )
  } else {
    data_prepared <- prepare_data(
      .data = .data,
      intensity_column = {{ intensity_column }},
      lactate_column = {{ lactate_column }}
    )
  }

  data_fit_prepared <- data_prepared %>%
    prepare_fit(
      .data = .,
      fit = fit,
      include_baseline = include_baseline,
      sport = sport
    ) %>%
    dplyr::select(data, lactate_curve = data_augmented)

  out <- list(
    data = data_fit_prepared$data[[1]],
    lactate_curve = data_fit_prepared$lactate_curve[[1]] %>% dplyr::rename(lactate = .fitted)
  )

  # if heart rate data was added, then retrieve the line for plotting too
  if(!missing(heart_rate_column)) {
    # define interpolation factor
    switch (
      sport,
      "cycling" = interpolation_factor <- 0.1,
      "running" = interpolation_factor <- 0.1,
      "swimming" = interpolation_factor <- 0.01
    )
    # define whether to include the baseline value
    if(include_baseline) {
      data_for_modeling <- data_prepared
    } else {
      data_for_modeling <- data_prepared[-1, ]
    }
    # linear model
    heart_rate_model = lm(heart_rate ~ intensity, data = data_for_modeling)
    # data for retrieving response
    new_data_model <- data.frame(intensity = seq(min(data_for_modeling$intensity), max(data_for_modeling$intensity), interpolation_factor))
    # get response data
    heart_rate_response <- broom::augment(heart_rate_model, newdata = new_data_model, type.predict = "response") %>%
      dplyr::rename(heart_rate = .fitted)
    # add to the output
    out$heart_rate_response <- heart_rate_response
  }
  out
}
