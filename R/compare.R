#' Compare two lactate tests
#'
#' Compares the thresholds of two tests of the same athlete (e.g. before and after a training block). A
#' rightward shift of the lactate curve shows as higher intensities at the fixed concentrations (`OBLA`)
#' and at the curve-based thresholds.
#'
#' @param before,after The results of [lactate_threshold()] for the two tests, with the same sport.
#'
#' @return a `lactate_comparison` tibble with one row per method found in both tests: `method`, `before`,
#' `after`, `change`, and `change_percent`. Plot both curves with [autoplot()].
#' @export
#'
#' @examples
#' before <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
#' after_data <- transform(demo_data, intensity = intensity * 1.05)
#' after <- lactate_threshold(after_data, intensity, lactate, plot = FALSE)
#' compare_tests(before, after)
compare_tests <- function(before, after) {
  info_before <- lactater_info(before)
  info_after <- lactater_info(after)
  if(info_before$settings$sport != info_after$settings$sport)
    stop("Both tests must be from the same sport.", call. = FALSE)

  methods <- intersect(as.character(before$method), as.character(after$method))
  if(length(methods) == 0) stop("The two tests have no method in common.", call. = FALSE)
  value_before <- before$intensity[match(methods, before$method)]
  value_after <- after$intensity[match(methods, after$method)]
  out <- tibble::tibble(
    method = factor(methods, levels = methods),
    before = value_before,
    after = value_after,
    change = value_after - value_before,
    change_percent = round(100 * (value_after - value_before) / value_before, 1)
  )
  attr(out, "lactater_comparison") <- list(before = info_before, after = info_after)
  class(out) <- c("lactate_comparison", class(out))
  out
}

#' @rdname compare_tests
#' @param object The result of [compare_tests()].
#' @param ... Not used.
#' @export
autoplot.lactate_comparison <- function(object, ...) {
  tests <- attr(object, "lactater_comparison")
  labelled <- function(.data, label) transform(.data, test = factor(label, levels = c("Before", "After")))
  curves <- rbind(labelled(curve_points(tests$before$curve), "Before"), labelled(curve_points(tests$after$curve), "After"))
  points <- rbind(
    labelled(position_rest(tests$before$data)[c("intensity", "lactate")], "Before"),
    labelled(position_rest(tests$after$data)[c("intensity", "lactate")], "After")
  )
  colours <- c("Before" = lactater_colours[["faint"]], "After" = lactater_colours[["curve"]])

  ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate, colour = test), curves, linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(intensity, lactate, fill = test), points,
      shape = 21, size = 3, stroke = 0.9, colour = lactater_colours[["paper"]]
    ) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::labs(
      title = "Lactate curves compared",
      subtitle = sprintf("Median change across methods: %+.1f%%", stats::median(object$change_percent, na.rm = TRUE)),
      x = intensity_label(tests$before$settings$sport),
      y = quote(Lactate ~ (mmol %.% L^-1))
    ) +
    theme_lactater()
}
