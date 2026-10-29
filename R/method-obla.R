#' Onset of Blood Lactate Accumulation (OBLA)
#'
#' It applies the `OBLA` methods: the intensity at which the fitted curve reaches 2.0, 2.5, 3.0,
#' 3.5, and 4.0 mmol/L.
#'
#' @param data The data retrieved from `prepare_data()`.
#' @param curve The user-selected curve from `fit_curve()`.
#' @param settings The analysis settings.
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_obla <- function(data, curve, settings) {
  obla <- c(2, 2.5, 3, 3.5, 4)
  method_result(
    method = sprintf("OBLA %.1f", obla),
    fitting = user_fitting(settings),
    intensity = vapply(obla, retrieve_intensity, numeric(1), curve = curve),
    lactate = obla,
    plot_data = list(curve = curve_points(curve), fitting = curve$fit)
  )
}

#' Method result
#'
#' The common shape of every `method_*()` result. For internal use only.
#'
#' @param method,fitting,intensity,lactate The result columns.
#' @param plot_data What the plot needs, either one list shared by all rows or a list per row.
#'
#' @keywords internal
method_result <- function(method, fitting, intensity, lactate, plot_data) {
  rows <- data.frame(method = method, fitting = fitting, intensity = intensity, lactate = lactate)
  if(!is.null(names(plot_data))) plot_data <- rep(list(plot_data), nrow(rows))
  list(rows = rows, plot_data = plot_data)
}

#' Label of the user-selected fit
#'
#' @param settings The analysis settings.
#' @keywords internal
user_fitting <- function(settings) {
  paste(settings$fit, "(user-defined)")
}
