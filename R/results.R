## method categories in output order, with their (CVD-validated) colours
category_colours <- c(
  "Log-log" = "#2a78d6",
  "OBLA" = "#eb6834",
  "Bsln+" = "#1baf7a",
  "Dmax" = "#eda100",
  "LTP" = "#e87ba4",
  "LTratio" = "#008300",
  "IAT" = "#6250d6"
)

#' Build a lactate threshold result
#'
#' Adds the class and keeps what plotting needs (data, curve, settings, unrounded intensities).
#' For internal use only.
#'
#' @param out The result tibble.
#' @param data The data retrieved from `prepare_data()`.
#' @param curve The user-selected curve.
#' @param settings The analysis settings.
#' @param plot_data What each row's plot needs.
#' @param intensity The unrounded threshold intensities.
#' @param intervals The unrounded confidence intervals (a `low`/`high` matrix), or `NULL`.
#'
#' @keywords internal
new_lactate_thresholds <- function(out, data, curve, settings, plot_data, intensity, intervals = NULL) {
  attr(out, "lactater") <- list(
    data = data, curve = curve, settings = settings, plot_data = plot_data, intensity = intensity, intervals = intervals
  )
  class(out) <- c("lactate_thresholds", class(out))
  out
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_thresholds <- function(x, ...) {
  info <- attr(x, "lactater")
  if(is.null(info)) return(NextMethod())
  stages <- sum(info$data$intensity > 0)
  warnings <- sum(lactate_checks(x)$status == "warning")
  c(
    "Lactate thresholds" = sprintf(
      "%s, %s, %d stages%s%s",
      info$settings$sport,
      info$settings$fit,
      stages,
      if(any(info$data$intensity == 0)) " + rest" else "",
      if(info$settings$sample != "blood") paste0(", ", info$settings$sample) else ""
    ),
    if(warnings > 0) c("Checks" = sprintf("%d warning%s, see lactate_checks()", warnings, if(warnings > 1) "s" else ""))
  )
}

#' Plot lactate thresholds
#'
#' Plots the result of [lactate_threshold()]. Without `method`, an overview: the fitted lactate
#' curve on top and every threshold below, one row per method, on the same intensity axis, so you
#' can see at a glance how much the methods agree. With `method`, the plot of that one method.
#'
#' @param object,x The result of [lactate_threshold()].
#' @param method A method to plot (one of `object$method`), or `NULL` for the overview.
#' @param ... Not used.
#'
#' @return a `ggplot2` (one method) or `patchwork` (overview) object.
#' @export
#'
#' @examples
#' results <- lactate_threshold(
#'   demo_data,
#'   intensity_column = "intensity",
#'   lactate_column = "lactate",
#'   heart_rate_column = "heart_rate",
#'   plot = FALSE
#' )
#' autoplot(results)
#' autoplot(results, method = "Dmax")
autoplot.lactate_thresholds <- function(object, method = NULL, ...) {
  info <- attr(object, "lactater")
  if(is.null(info))
    stop("This result has lost its lactater attributes (for example after a dplyr verb). Plot the result of `lactate_threshold()` directly.", call. = FALSE)

  if(is.null(method)) return(plot_overview(object, info))

  i <- match(method, as.character(object$method))
  if(is.na(i))
    stop(sprintf("`method` must be one of: %s.", paste(object$method, collapse = ", ")), call. = FALSE)
  plot_threshold(
    method = method,
    intensity = info$intensity[i],
    lactate = object$lactate[i],
    heart_rate = if("heart_rate" %in% names(object)) object$heart_rate[i] else NA,
    plot_data = info$plot_data[[i]],
    data = info$data,
    settings = info$settings,
    interval = if(!is.null(info$intervals)) info$intervals[i, ]
  )
}

#' @rdname autoplot.lactate_thresholds
#' @export
plot.lactate_thresholds <- function(x, method = NULL, ...) {
  print(autoplot(x, method = method))
  invisible(x)
}

#' Overview plot
#'
#' @param object The result of [lactate_threshold()].
#' @param info Its `lactater` attribute.
#' @keywords internal
plot_overview <- function(object, info) {
  points <- position_rest(info$data)
  thresholds <- data.frame(
    method = factor(object$method, levels = rev(levels(object$method))),
    category = object$method_category,
    intensity = info$intensity,
    label = format(object$intensity)
  )
  if(!is.null(info$intervals)) {
    thresholds$low <- info$intervals[, "low"]
    thresholds$high <- info$intervals[, "high"]
  }
  thresholds <- thresholds[!is.na(thresholds$intensity), ]
  ## labels sit past the whisker, so they never cover it
  thresholds$label_at <- if(is.null(thresholds$high)) thresholds$intensity else pmax(thresholds$intensity, thresholds$high, na.rm = TRUE)
  curve <- curve_points(info$curve)

  limits <- range(points$intensity, curve$intensity, thresholds$intensity, thresholds$low, thresholds$high, na.rm = TRUE)
  axis <- adjust_x_axis_plot(.data = points, has_rest = any(info$data$intensity == 0))
  x_scale <- ggplot2::scale_x_continuous(limits = limits, breaks = axis$breaks, labels = axis$labels)
  fill_scale <- ggplot2::scale_fill_manual(values = category_colours, drop = TRUE)

  top <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = lactater_colours[["curve"]], linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(intensity, lactate), points,
      shape = 21, size = 3, stroke = 0.9,
      fill = lactater_colours[["points"]], colour = lactater_colours[["paper"]]
    ) +
    x_scale +
    ggplot2::labs(x = NULL, y = quote(Lactate ~ (mmol %.% L^-1))) +
    theme_lactater()

  ladder <- ggplot2::ggplot(thresholds, ggplot2::aes(intensity, method))
  if(!is.null(info$intervals)) {
    ladder <- ladder +
      ggplot2::geom_linerange(ggplot2::aes(xmin = low, xmax = high, colour = category), linewidth = 0.6, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = category_colours)
  }
  ladder <- ladder +
    ggplot2::geom_point(
      ggplot2::aes(fill = category),
      shape = 21, size = 3.4, stroke = 1, colour = lactater_colours[["paper"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = label_at, label = label),
      hjust = -0.35, size = 3, colour = lactater_colours[["muted"]]
    ) +
    x_scale +
    fill_scale +
    ggplot2::labs(x = intensity_label(info$settings$sport), y = NULL) +
    theme_lactater() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  patchwork::wrap_plots(top, ladder, ncol = 1, heights = c(1, max(1, nrow(thresholds) / 10)), guides = "collect") +
    patchwork::plot_annotation(
      title = "Lactate thresholds",
      subtitle = sprintf("%d thresholds from %d methods", nrow(thresholds), length(unique(thresholds$category))),
      caption = paste("Fit:", info$settings$fit),
      theme = theme_lactater()
    ) &
    ggplot2::theme(legend.position = "top")
}
