#' Bsln+
#'
#' It applies the `Bsln+` methods: the intensity at which the fitted curve reaches 0.5, 1.0, and
#' 1.5 mmol/L above the baseline. The baseline is the first sample: the resting sample when there
#' is one, otherwise the first stage.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_bsln_plus <- function(data, curve, settings) {
  plus <- c(0.5, 1, 1.5)
  target <- data$lactate[1] + plus
  method_result(
    method = sprintf("Bsln + %.1f", plus),
    fitting = user_fitting(settings),
    intensity = vapply(target, retrieve_intensity, numeric(1), curve = curve),
    lactate = target,
    plot_data = list(curve = curve_points(curve), fitting = curve$fit)
  )
}
