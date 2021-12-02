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

  data <- data_processed$data[[1]]
  data_augmented <- data_processed$data_augmented[[1]]
  method_intensity <- data_processed$intensity
  method_lactate <- data_processed$lactate

  if(is.na(method_intensity)) {
    return(NA)
  }

  if(method %in% c("Log-log", "LTP1", "LTP2")) {

    segmented_object <- data_processed$segmented_object[[1]]
    data_interpolated <- data_processed$data_interpolated[[1]]

    if(method == "Log-log") {
      loglog_restrainer <- data_processed$loglog_restrainer

      data_interpolated <- data_interpolated %>%
        dplyr::slice(seq(loglog_restrainer * dplyr::n()))
    }

    breakpoint_y_value <- predict(object = segmented_object, newdata = data.frame(intensity = method_intensity))

    fitted_values <- segmented::broken.line(ogg = segmented_object)$fit %>%
      dplyr::as_tibble() %>%
      dplyr::bind_cols(data_interpolated, .)

    if(method == "Log-log")
      data <- data %>%
      dplyr::mutate(lactate = log(lactate))

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(intensity, lactate), shape = 21, size = 4) +
      ggplot2::geom_line(data = fitted_values, ggplot2::aes(intensity, value), color = "red", alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(x = method_intensity, y = breakpoint_y_value), color = "blue", alpha = 0.4, size = 4) +
      ggplot2::theme_light() +
      ggplot2::labs(title = method,
                    y = "&#91;La<sup>-</sup>&#93; (Log mmol\u00b7L<sup>-1</sup>)") +
      ggplot2::theme(axis.title.y = ggtext::element_markdown())

  } else if(method == "LTratio") {

    data_ratio <- data_augmented %>%
      dplyr::mutate(ratio = .fitted / intensity) %>%
      dplyr::filter(!is.infinite(ratio))

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

  breaks_and_labels <- adjust_x_axis_plot(.data = data, plot = p)

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
#' @param .data The raw data.
#' @param plot A `ggplot2` object.
#'
#' @keywords internal
adjust_x_axis_plot <- function(
  .data,
  plot
) {

  p_build <- ggplot2::ggplot_build(plot)

  seq_plot <- p_build$layout$panel_params[[1]]$x$minor_breaks

  out <- NULL

  out$breaks <- c({{.data}}$intensity[1], seq_plot[-1])
  out$labels <- c("rest", seq_plot[-1])

  out
}
