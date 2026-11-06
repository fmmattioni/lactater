#' Bsln+
#'
#' It applies the `Bsln+` methods: the intensity at which the fitted curve reaches each rise in
#' `settings$baseline_plus` (0.5, 1.0, and 1.5 mmol/L by default) above the baseline. The baseline is the
#' first sample: the resting sample when there is one, otherwise the first stage.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_bsln_plus <- function(data, curve, settings) {
  plus <- settings$baseline_plus
  target <- data$lactate[1] + plus
  method_result(
    method = paste("Bsln +", target_label(plus)),
    fitting = user_fitting(settings),
    intensity = vapply(target, retrieve_intensity, numeric(1), curve = curve),
    lactate = round(target, 1),
    plot_data = lapply(plus, function(rise) {
      list(curve = curve_points(curve), baseline = data$lactate[1], rise = rise, fitting = curve$fit)
    })
  )
}
