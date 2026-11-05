#' Prepare raw data
#'
#' It organizes and renames the raw data.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column, as a string.
#' @param lactate_column The name of the lactate column, as a string.
#' @param heart_rate_column The name of the heart rate column, as a string, or `NULL`.
#' @param rpe_column The name of the rating of perceived exertion column, as a string, or `NULL`.
#' @param rpe_scale The RPE scale, `"6-20"` or `"CR10"`, used to check the values.
#'
#' @return a tibble with the following columns:
#' \item{intensity}{The intensity column.}
#' \item{lactate}{The lactate column.}
#' \item{heart_rate}{The heart rate column, if applicable.}
#' \item{rpe}{The rating of perceived exertion column, if applicable.}
#'
#' @keywords internal
prepare_data <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column = NULL,
  rpe_column = NULL,
  rpe_scale = "6-20"
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  columns <- c(intensity = intensity_column, lactate = lactate_column, heart_rate = heart_rate_column, rpe = rpe_column)
  not_found <- setdiff(columns, names(.data))
  if(length(not_found) > 0)
    stop(sprintf("Column(s) not found in the data: %s.", paste0("`", not_found, "`", collapse = ", ")), call. = FALSE)
  out <- tibble::as_tibble(stats::setNames(as.list(.data)[columns], names(columns)))
  ## a column left empty, such as RPE not recorded on the form, is as if it was not given
  for(column in intersect(c("heart_rate", "rpe"), names(out))) {
    if(all(is.na(out[[column]]))) out[[column]] <- NULL
  }

  if(!is.numeric(out$intensity) || !is.numeric(out$lactate))
    stop("`intensity` and `lactate` columns must be numeric.", call. = FALSE)
  ## a missed sample should not void the whole test
  missing <- is.na(out$intensity) | is.na(out$lactate)
  if(any(missing)) {
    warning(sprintf("Dropped %d row(s) with a missing `intensity` or `lactate` value.", sum(missing)), call. = FALSE)
    out <- out[!missing, ]
  }
  if(nrow(out) < 3)
    stop("At least 3 rows are required for lactate threshold estimation.", call. = FALSE)
  if(!all(out$intensity >= 0))
    stop("`intensity` values must be >= 0.", call. = FALSE)
  if(!all(out$lactate > 0))
    stop("`lactate` values must be > 0 (a 0 is usually an empty cell).", call. = FALSE)
  if("rpe" %in% names(out)) {
    ## CR10 allows ratings above 10 for an effort harder than any felt before (absolute maximum);
    ## shortcut: above 12 is taken as a 6-20 rating entered on the wrong scale
    limits <- if(rpe_scale == "6-20") c(6, 20) else c(0, 12)
    if(!is.numeric(out$rpe) || any(out$rpe < limits[1] | out$rpe > limits[2], na.rm = TRUE))
      stop(
        sprintf(
          "`rpe` values must be between %g and %g on the %s scale (check `rpe_scale`).", limits[1], limits[2], rpe_scale
        ),
        call. = FALSE
      )
  }
  if(any(diff(out$intensity) <= 0))
    stop(
      "`intensity` must be strictly increasing (the resting sample, if any, is the row with intensity 0).",
      call. = FALSE
    )

  out
}

#' Position the resting sample
#'
#' The resting sample (intensity 0) is placed one step before the first stage, so it can be
#' plotted next to the curve and, with `include_baseline = TRUE`, fitted. The step is the median
#' step between stages, so a missed stage or a short last stage does not shift it.
#'
#' @param .data The data retrieved from `prepare_data()`.
#'
#' @return `.data` with the resting intensity replaced; unchanged when there is no resting sample.
#' @keywords internal
position_rest <- function(.data) {
  rest <- .data$intensity == 0
  stages <- .data$intensity[!rest]
  .data$intensity[rest] <- min(stages) - stats::median(diff(stages))
  .data
}
