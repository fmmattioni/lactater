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
  ## keyed by method, because row subsetting keeps attributes but changes positions
  names(intensity) <- names(plot_data) <- as.character(out$method)
  if(!is.null(intervals)) rownames(intervals) <- as.character(out$method)
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
#' curve on top and every threshold below, grouped by method family, on the same intensity axis, so
#' you can see at a glance how much the methods agree. With `method`, the plot of that one method:
#' what the method fitted, the measured stages, and the threshold with its intensity, lactate and
#' heart rate.
#'
#' The plots are `ggplot2` objects: add layers, scales or a `theme()` to change them. See
#' [theme_lactater()] for the dark style (`options(lactater.style = "night")`) and the font.
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
  info <- lactater_info(object)

  if(is.null(method)) return(plot_overview(object, info))

  i <- match(method, as.character(object$method))
  if(is.na(i))
    stop(sprintf("`method` must be one of: %s.", paste(object$method, collapse = ", ")), call. = FALSE)
  plot_threshold(
    method = method,
    intensity = info$intensity[[method]],
    lactate = object$lactate[i],
    heart_rate = if("heart_rate" %in% names(object)) object$heart_rate[i] else NA,
    rpe = if("rpe" %in% names(object)) object$rpe[i] else NA,
    plot_data = info$plot_data[[method]],
    data = info$data,
    settings = info$settings,
    interval = if(!is.null(info$intervals)) info$intervals[method, ]
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
#' The fitted curve on top, and below it every threshold as a dot on the same intensity axis, grouped by
#' method family, with the value written next to it (and the confidence interval as a line, when present).
#'
#' @param object The result of [lactate_threshold()].
#' @param info Its `lactater` attribute.
#' @keywords internal
plot_overview <- function(object, info) {
  colours <- lactater_colours()
  sport <- info$settings$sport
  points <- position_rest(info$data)
  methods <- as.character(object$method)
  thresholds <- data.frame(
    method = methods,
    family = as.character(object$method_category),
    intensity = unname(info$intensity[methods])
  )
  if(!is.null(info$intervals)) {
    thresholds$low <- unname(info$intervals[methods, "low"])
    thresholds$high <- unname(info$intervals[methods, "high"])
  }
  thresholds <- thresholds[!is.na(thresholds$intensity), ]
  ## one row per threshold, the first method on top, grouped by family
  thresholds$method <- factor(thresholds$method, levels = rev(thresholds$method))
  thresholds$family <- factor(toupper(thresholds$family), levels = unique(toupper(thresholds$family)))
  ## values sit past the interval, so they never cover it
  thresholds$value_at <- thresholds$intensity
  if(!is.null(thresholds$high)) thresholds$value_at <- pmax(thresholds$intensity, thresholds$high, na.rm = TRUE)
  curve <- curve_points(info$curve)

  limits <- range(
    points$intensity, curve$intensity, thresholds$intensity, thresholds$low, thresholds$high,
    na.rm = TRUE
  )
  ## both panels share one intensity axis; patchwork lines up their panels
  x_scale <- ggplot2::scale_x_continuous(limits = limits + c(-0.02, 0.06) * diff(limits))
  top_lactate <- max(points$lactate, curve$lactate) * 1.06

  top <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9) +
    stage_layers(points, info$data, colours) +
    x_scale +
    ggplot2::scale_y_continuous(limits = c(0, top_lactate), breaks = lactate_breaks(top_lactate), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Blood lactate (mmol/L)") +
    theme_lactater() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank()
    )

  dots <- ggplot2::ggplot(thresholds) +
    ggplot2::geom_segment(
      ggplot2::aes(x = -Inf, xend = intensity, y = method, yend = method),
      colour = colours[["faint"]], linewidth = 0.3, linetype = "13"
    )
  if(!is.null(thresholds$low)) {
    dots <- dots + ggplot2::geom_segment(
      ggplot2::aes(x = low, xend = high, y = method, yend = method),
      colour = colours[["accent"]], linewidth = 0.7, alpha = 0.5, na.rm = TRUE
    )
  }
  dots <- dots +
    ggplot2::geom_point(
      ggplot2::aes(intensity, method), shape = 21, size = 3, stroke = 0.9, fill = colours[["accent"]],
      colour = colours[["paper"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(value_at, method, label = format_intensity(intensity, sport)),
      hjust = -0.35, size = 3.1, colour = colours[["ink"]]
    ) +
    ggplot2::facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y") +
    x_scale +
    ggplot2::labs(x = intensity_label(sport), y = NULL) +
    theme_lactater() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = colours[["ink"]], size = ggplot2::rel(0.9)),
      axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(
        angle = 0, hjust = 1, vjust = 1, face = "bold", size = ggplot2::rel(0.7), colour = colours[["muted"]]
      ),
      panel.spacing.y = ggplot2::unit(6, "pt")
    )

  patchwork::wrap_plots(top, dots, ncol = 1, heights = c(1, max(1, nrow(thresholds) / 11))) +
    patchwork::plot_annotation(
      title = sprintf("%d thresholds from %d methods", nrow(thresholds), nlevels(thresholds$family)),
      subtitle = "Every threshold found for this test, by method family",
      caption = paste("Fit:", info$settings$fit),
      theme = theme_lactater()
    )
}
