#' Plot lactate
#'
#' Plots the lactate method. For internal use only.
#'
#' @param data_processed The processed data retrieve within one of the `method_*` functions.
#' @param method The lactate threshold method.
#'
#' @return a `ggplot2` object.
#' @keywords internal
#' @importFrom stats predict
plot_lactate <- function(data_processed, method) {

  has_rest <- any(data_processed$data[[1]]$intensity == 0)
  data <- position_rest(data_processed$data[[1]])
  data_augmented <- data_processed$data_augmented[[1]]
  method_intensity <- data_processed$intensity
  method_lactate <- data_processed$lactate

  if(is.na(method_intensity)) {
    return(NA)
  }

  if(method %in% c("Log-log", "LTP1", "LTP2")) {

    ## the breakpoint fit is in the space it was fitted in (log-log for Log-log)
    breakpoint_fit <- data_processed$breakpoint_fit[[1]]
    fitted_values <- data.frame(intensity = seq(min(breakpoint_fit$x), max(breakpoint_fit$x), length.out = 200))
    fitted_values$value <- predict_breakpoints(breakpoint_fit, fitted_values$intensity)

    x_axis_label <- "intensity"
    y_axis_label <- "&#91;La<sup>-</sup>&#93; (mmol\u00b7L<sup>-1</sup>)"
    if(method == "Log-log") {
      method_intensity <- log(method_intensity)
      data <- data %>%
        dplyr::filter(intensity > 0) %>%
        dplyr::mutate(
          intensity = log(intensity),
          lactate = log(lactate)
        )

      x_axis_label <- "Log intensity"
      y_axis_label <- "&#91;La<sup>-</sup>&#93; (Log mmol\u00b7L<sup>-1</sup>)"
    }
    breakpoint_y_value <- predict_breakpoints(breakpoint_fit, method_intensity)

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(intensity, lactate), shape = 21, size = 4) +
      ggplot2::geom_line(data = fitted_values, ggplot2::aes(intensity, value), color = "red", alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(x = method_intensity, y = breakpoint_y_value), color = "blue", alpha = 0.4, size = 4) +
      ggplot2::theme_light() +
      ggplot2::labs(
        title = method,
        x = x_axis_label,
        y = y_axis_label
      ) +
      ggplot2::theme(axis.title.y = ggtext::element_markdown())

  } else if(method == "LTratio") {

    data_ratio <- data_augmented %>%
      dplyr::filter(intensity >= min(data$intensity[data$intensity > 0])) %>%
      dplyr::mutate(ratio = .fitted / intensity)

    data_min_ratio <- data_ratio %>%
      dplyr::slice(which.min(ratio))

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = data_ratio, ggplot2::aes(intensity, ratio), color = "red", alpha = 0.4) +
      ggplot2::geom_point(data = data_min_ratio, ggplot2::aes(intensity, ratio), color = "blue", alpha = 0.4, size = 4) +
      ggplot2::theme_light() +
      ggplot2::labs(title = "LT<sub>ratio</sub>",
                    y = "Ratio &#91;La<sup>-</sup>&#93;/intensity") +
      ggplot2::theme(plot.title =  ggtext::element_markdown(),
                     axis.title.y = ggtext::element_markdown())

  } else if(grepl(pattern = "Dmax", x = {{ method }})) {

    data_plot_line <- data_processed$data_plot_line[[1]]

    method_label <- switch (
      method,
      "Dmax" = "D<sub>max</sub>",
      "ModDmax" = "ModD<sub>max</sub>",
      "Exp-Dmax" = "Exp-D<sub>max</sub>",
      "Log-Poly-ModDmax" = "Log-Poly-ModD<sub>max</sub>",
      "Log-Exp-ModDmax" = "Log-Exp-ModD<sub>max</sub>"
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(intensity, lactate), shape = 21, size = 4) +
      ggplot2::geom_line(data = data_augmented, ggplot2::aes(intensity, .fitted), color = "red", alpha = 0.4) +
      ggplot2::geom_smooth(data = data_plot_line, ggplot2::aes(intensity, lactate), method = "lm", formula = "y ~ x", color = "black", size = 0.75) +
      ggplot2::geom_point(ggplot2::aes(x = method_intensity, y = method_lactate), color = "blue", alpha = 0.4, size = 4) +
      ggplot2::theme_light() +
      ggplot2::labs(title = method_label,
                    y = "&#91;La<sup>-</sup>&#93; (mmol\u00b7L<sup>-1</sup>)") +
      ggplot2::theme(
        plot.title =  ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()
      )

  } else {

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(intensity, lactate), shape = 21, size = 4) +
      ggplot2::geom_line(data = data_augmented, ggplot2::aes(intensity, .fitted), color = "red", alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(x = method_intensity, y = method_lactate), color = "blue", alpha = 0.4, size = 4) +
      ggplot2::theme_light() +
      ggplot2::labs(title = method,
                    y = "&#91;La<sup>-</sup>&#93; (mmol\u00b7L<sup>-1</sup>)") +
      ggplot2::theme(axis.title.y = ggtext::element_markdown())

  }

  breaks_and_labels <- adjust_x_axis_plot(.data = data, has_rest = has_rest)

  p <- p +
    ggplot2::scale_x_continuous(breaks = breaks_and_labels$breaks, labels = breaks_and_labels$labels)

  p
}

#' Combine lactate threshold methods into one plot
#'
#' @param plots The `ggplot2` objects to be combined.
#' @param ... Additional arguments passed onto `patchwork::wrap_plots()`.
#'
#' @return a `patchwork` object
#' @export
plot_methods <- function(plots, ...) {
  patchwork::wrap_plots(... = plots, ...)
}

#' Adjust x-axis
#'
#' Adjusts the x-axis to show rest value at the beginning. Internal use only.
#'
#' @param .data The raw data, with the resting sample positioned (see `position_rest()`).
#' @param has_rest Whether the first row is a resting sample.
#'
#' @keywords internal
adjust_x_axis_plot <- function(
  .data,
  has_rest = TRUE
) {
  x_values <- .data$intensity[!is.na(.data$intensity)]
  if(!has_rest) {
    breaks <- pretty(range(x_values), n = 5)
    return(list(breaks = breaks, labels = as.character(breaks)))
  }
  rest_value <- x_values[1]
  x_values <- x_values[-1]

  pretty_breaks <- if(length(x_values) > 0) {
    pretty(range(x_values), n = 5)
  } else {
    numeric(0)
  }

  breaks <- unique(c(rest_value, pretty_breaks))
  labels <- as.character(breaks)
  labels[breaks == rest_value] <- "rest"

  list(breaks = breaks, labels = labels)
}
