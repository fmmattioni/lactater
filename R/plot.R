## colours: categorical hues validated for colour-vision deficiency (dataviz reference palette);
## text and measured points use neutral ink
lactater_colours <- c(
  curve = "#e34948",
  threshold = "#2a78d6",
  points = "#52514e",
  ink = "#0b0b0b",
  muted = "#52514e",
  faint = "#8a8984",
  grid = "#e6e5e0",
  paper = "#fcfcfb"
)

#' lactater ggplot2 theme
#'
#' The theme used by every lactater plot: a quiet canvas with hairline grids, so the lactate
#' curve and the thresholds carry the colour. Use it to make your own plots match.
#'
#' @param base_size Base font size, in points.
#' @param base_family Base font family.
#'
#' @return a `ggplot2` theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(demo_data, aes(intensity, lactate)) +
#'   geom_point() +
#'   theme_lactater()
theme_lactater <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    ink = lactater_colours[["ink"]],
    paper = lactater_colours[["paper"]]
  ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = lactater_colours[["grid"]], linewidth = 0.3),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = lactater_colours[["muted"]]),
      axis.title = ggplot2::element_text(colour = lactater_colours[["muted"]], size = ggplot2::rel(0.9)),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2)),
      plot.subtitle = ggplot2::element_text(colour = lactater_colours[["muted"]], margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = lactater_colours[["faint"]], size = ggplot2::rel(0.75), hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin_auto(12)
    )
}

#' Plot one lactate threshold
#'
#' For internal use only.
#'
#' @param method The method label.
#' @param intensity,lactate,heart_rate The threshold values shown in the subtitle.
#' @param plot_data What the method drew on: the fitted `curve`, a Dmax `line`, a `breakpoints` fit,
#' or a `ratio` curve, and the `fitting` label.
#' @param data The data retrieved from `prepare_data()`.
#' @param settings The analysis settings (`sport`).
#' @param interval The confidence interval of the intensity (`low`, `high`), or `NULL`.
#'
#' @return a `ggplot2` object, or `NA` when the threshold could not be estimated.
#' @keywords internal
plot_threshold <- function(method, intensity, lactate, heart_rate, plot_data, data, settings, interval = NULL) {
  if(is.na(intensity)) return(NA)

  points <- position_rest(data)
  y_label <- quote(Lactate ~ (mmol %.% L^-1))
  p <- ggplot2::ggplot()
  if(!is.null(interval) && !anyNA(interval)) {
    p <- p +
      ggplot2::annotate(
        "rect", xmin = interval[["low"]], xmax = interval[["high"]], ymin = -Inf, ymax = Inf,
        fill = lactater_colours[["threshold"]], alpha = 0.12
      )
  }

  if(!is.null(plot_data$ratio)) {
    ## LTratio: the minimum of lactate / intensity
    marker <- data.frame(intensity = intensity, value = plot_data$ratio_min)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(intensity, ratio), plot_data$ratio, colour = lactater_colours[["curve"]], linewidth = 0.8)
    y_label <- "Lactate / intensity"
  } else if(!is.null(plot_data$breakpoints)) {
    ## Log-log and LTP: the fitted straight segments
    fit <- plot_data$breakpoints
    segments <- data.frame(x = seq(min(fit$x), max(fit$x), length.out = 200))
    segments$y <- predict_breakpoints(fit, segments$x)
    at <- if(plot_data$log) log(intensity) else intensity
    marker <- data.frame(intensity = intensity, value = predict_breakpoints(fit, at))
    if(plot_data$log) {
      segments <- exp(segments)
      marker$value <- exp(marker$value)
      points <- points[points$intensity > 0, ]
    }
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x, y), segments, colour = lactater_colours[["curve"]], linewidth = 0.8)
  } else {
    marker <- data.frame(intensity = intensity, value = lactate)
    if(!is.null(plot_data$line)) {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(intensity, lactate), plot_data$line, colour = lactater_colours[["muted"]], linewidth = 0.5)
    }
    p <- p +
      ggplot2::geom_line(ggplot2::aes(intensity, lactate), plot_data$curve, colour = lactater_colours[["curve"]], linewidth = 0.8)
  }

  if(is.null(plot_data$ratio)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(intensity, lactate), points,
        shape = 21, size = 3, stroke = 0.9,
        fill = lactater_colours[["points"]], colour = lactater_colours[["paper"]]
      )
  }

  ## thin guides from the threshold to both axes (-Inf has no logarithm, so log axes start at the data)
  log_axes <- isTRUE(plot_data$log)
  marker$x_from <- if(log_axes) min(points$intensity) else -Inf
  marker$y_from <- if(log_axes) min(points$lactate) else -Inf

  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes(x = intensity, xend = intensity, y = y_from, yend = value), marker,
      colour = lactater_colours[["threshold"]], linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_from, xend = intensity, y = value, yend = value), marker,
      colour = lactater_colours[["threshold"]], linewidth = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(intensity, value), marker,
      shape = 21, size = 4.8, stroke = 1.3,
      fill = lactater_colours[["threshold"]], colour = lactater_colours[["paper"]]
    ) +
    ggplot2::labs(
      title = method,
      subtitle = threshold_label(intensity, lactate, heart_rate, settings$sport, interval),
      caption = paste("Fit:", plot_data$fitting),
      x = intensity_label(settings$sport),
      y = y_label
    ) +
    theme_lactater()

  if(log_axes) {
    return(p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10())
  }

  axis <- if(is.null(plot_data$ratio)) {
    adjust_x_axis_plot(.data = points, has_rest = any(data$intensity == 0))
  } else {
    adjust_x_axis_plot(.data = plot_data$ratio, has_rest = FALSE)
  }
  p + ggplot2::scale_x_continuous(breaks = axis$breaks, labels = axis$labels)
}

#' Threshold label
#'
#' @param intensity,lactate,heart_rate The threshold values.
#' @param sport The sport, which sets the intensity unit.
#' @param interval The confidence interval of the intensity (`low`, `high`), or `NULL`.
#'
#' @return a label such as `"131.7 W, 3.1 mmol/L, 173 bpm"` (joined with middle dots).
#' @keywords internal
threshold_label <- function(intensity, lactate, heart_rate, sport, interval = NULL) {
  unit <- switch(sport, cycling = "W", rowing = "W", running = "km/h", swimming = "m/s")
  digits <- intensity_digits(sport)
  value <- function(x) format(round(x, digits), nsmall = digits)
  parts <- c(
    paste(value(intensity), unit),
    if(!is.null(interval) && !anyNA(interval)) sprintf("(CI %s-%s)", value(interval[["low"]]), value(interval[["high"]])),
    if(sport != "cycling") format_pace(convert_to_pace(intensity, sport), sport),
    paste(format(lactate, nsmall = 1), "mmol/L"),
    if(!is.null(heart_rate) && !is.na(heart_rate)) paste(heart_rate, "bpm")
  )
  paste(parts, collapse = " \u00b7 ")
}

#' Intensity axis label
#'
#' @param sport The sport.
#' @keywords internal
intensity_label <- function(sport) {
  switch(sport, cycling = "Power (W)", rowing = "Power (W)", running = "Speed (km/h)", swimming = "Speed (m/s)")
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
