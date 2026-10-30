#' Onset of Blood Lactate Accumulation (OBLA)
#'
#' It applies the `OBLA` methods: the intensity at which the fitted curve reaches each fixed
#' concentration in `settings$fixed` (2.0, 2.5, 3.0, 3.5, and 4.0 mmol/L by default).
#'
#' @param data The data retrieved from `prepare_data()`.
#' @param curve The user-selected curve from `fit_curve()`.
#' @param settings The analysis settings.
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_obla <- function(data, curve, settings) {
  obla <- settings$fixed
  method_result(
    method = paste("OBLA", target_label(obla)),
    fitting = user_fitting(settings),
    intensity = vapply(obla, retrieve_intensity, numeric(1), curve = curve),
    lactate = obla,
    plot_data = list(curve = curve_points(curve), fitting = curve$fit)
  )
}

#' Target label
#'
#' A lactate value as it appears in a method name, with at least one decimal (`4` gives `"4.0"`,
#' `2.25` gives `"2.25"`).
#'
#' @param x Lactate values.
#' @keywords internal
target_label <- function(x) {
  vapply(x, format, character(1), nsmall = 1)
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
