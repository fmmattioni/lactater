#' Prepare raw data
#'
#' It organizes and renames the raw data.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if applicable.
#'
#' @return a [tibble][tibble::tibble-package] with the following columns:
#' \item{intensity}{The intensity column.}
#' \item{lactate}{The lactate column.}
#' \item{heart_rate}{The heart rate column, if applicable.}
#'
#' @keywords internal
prepare_data <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  intensity_column <- rlang::ensym(intensity_column)
  lactate_column <- rlang::ensym(lactate_column)

  out <- .data %>%
    dplyr::rename(
      intensity = {{ intensity_column }},
      lactate = {{ lactate_column }}
    )

  if(missing(heart_rate_column) == FALSE) {
    heart_rate_column <- rlang::ensym(heart_rate_column)

    out <- out %>%
      dplyr::rename(heart_rate = {{ heart_rate_column }}) %>%
      dplyr::select(intensity, lactate, heart_rate)
  } else {
    out <- out %>%
      dplyr::select(intensity, lactate)
  }

  out
}
