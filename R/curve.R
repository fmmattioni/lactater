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
#' @param include_baseline A boolean to indicate whether to include the resting (baseline) sample in the fit. The resting sample is
#' the row with intensity 0; it is placed one median step before the first stage. Data without a resting sample is fine.
#' Rows with a missing intensity or lactate are dropped with a warning.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @return a list with the following elements:
#' \item{data}{a tibble containing the raw data with the columns `intensity`, `lactate`, and `heart_rate`.}
#' \item{lactate_curve}{a tibble containing the data with the columns `intensity` and `lactate` for plotting the lactate curve according to the `fit` method chosen.}
#' \item{heart_rate_response}{a tibble with the columns `intensity` and `heart_rate` at each exercise stage. A line through
#' these points is the interpolation used for the heart rate at each threshold.}
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

  fit <- match.arg(arg = fit, several.ok = FALSE)
  sport <- match.arg(arg = sport, several.ok = FALSE)

  ## column names work quoted or unquoted
  data <- prepare_data(
    .data = .data,
    intensity_column = rlang::as_name(rlang::ensym(intensity_column)),
    lactate_column = rlang::as_name(rlang::ensym(lactate_column)),
    heart_rate_column = if(!missing(heart_rate_column)) rlang::as_name(rlang::ensym(heart_rate_column))
  )
  curve <- fit_curve(modeling_data(data, include_baseline), fit)
  intensity <- seq(curve$domain[1], curve$domain[2], by = switch(sport, cycling = 0.1, running = 0.1, swimming = 0.01))

  out <- list(
    data = position_rest(data),
    lactate_curve = tibble::tibble(intensity = intensity, lactate = curve$predict(intensity))
  )

  if("heart_rate" %in% names(data)) {
    stages <- data[data$intensity > 0 & !is.na(data$heart_rate), ]
    out$heart_rate_response <- tibble::tibble(intensity = stages$intensity, heart_rate = stages$heart_rate)
  }
  out
}
