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
#' Every threshold of the test on one intensity axis: the fitted curve on top, and below it one row per method,
#' grouped by family, with the threshold as a dot and its confidence interval as a line (when computed). A table on
#' the right prints the lactate, heart rate, RPE, percentage of the peak intensity and of the highest heart rate at
#' each threshold, with the lactate shaded by its value and, with intervals, their width shaded by how wide they are.
#'
#' @param object The result of [lactate_threshold()].
#' @param info Its `lactater` attribute.
#' @keywords internal
plot_overview <- function(object, info) {
  colours <- lactater_colours()
  sport <- info$settings$sport
  points <- position_rest(info$data)
  methods <- as.character(object$method)
  rows <- data.frame(
    method = methods,
    family = as.character(object$method_category),
    intensity = unname(info$intensity[methods]),
    lactate = object$lactate
  )
  if("heart_rate" %in% names(object)) rows$heart_rate <- object$heart_rate
  if("rpe" %in% names(object)) rows$rpe <- object$rpe
  if(!is.null(info$intervals)) {
    rows$low <- unname(info$intervals[methods, "low"])
    rows$high <- unname(info$intervals[methods, "high"])
  }
  rows <- rows[!is.na(rows$intensity), ]
  rows$row <- rev(seq_len(nrow(rows)))
  rows$peak <- 100 * rows$intensity / max(info$data$intensity)
  if(!is.null(rows$heart_rate)) rows$hr_max <- 100 * rows$heart_rate / max(info$data$heart_rate, na.rm = TRUE)
  family_end <- which(rows$family[-1] != rows$family[-nrow(rows)])
  separators <- rows$row[family_end] - 0.5
  curve <- curve_points(info$curve)
  limits <- range(points$intensity, curve$intensity, rows$intensity, rows$low, rows$high, na.rm = TRUE)
  x_scale <- ggplot2::scale_x_continuous(limits = limits, expand = ggplot2::expansion(mult = 0.03))
  y_rows <- ggplot2::scale_y_continuous(breaks = rows$row, labels = method_label(rows$method), expand = c(0, 0.6))
  bands <- ggplot2::annotate(
    "rect", xmin = -Inf, xmax = Inf, ymin = rows$row[rows$row %% 2 == 0] - 0.5,
    ymax = rows$row[rows$row %% 2 == 0] + 0.5, fill = colours[["faint"]], alpha = 0.18
  )
  lines <- ggplot2::geom_hline(yintercept = separators, colour = colours[["faint"]], linewidth = 0.4)

  top <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9) +
    stage_layers(points, info$data, colours) +
    x_scale +
    ggplot2::scale_y_continuous(
      limits = c(0, max(points$lactate, curve$lactate) * 1.08),
      breaks = lactate_breaks(max(points$lactate, curve$lactate) * 1.08), expand = c(0, 0)
    ) +
    ggplot2::labs(x = NULL, y = "Lactate (mmol/L)") +
    theme_lactater()

  dots <- ggplot2::ggplot(rows) + bands + lines
  if(!is.null(rows$low)) {
    dots <- dots + ggplot2::geom_segment(
      ggplot2::aes(x = low, xend = high, y = row, yend = row), colour = colours[["muted"]], alpha = 0.55,
      linewidth = 0.9, lineend = "round", na.rm = TRUE
    )
  }
  dots <- dots +
    ggplot2::geom_point(
      ggplot2::aes(intensity, row), shape = 21, size = 2.8, stroke = 0.7, fill = colours[["curve"]],
      colour = colours[["paper"]]
    ) +
    x_scale + y_rows +
    ggplot2::labs(x = intensity_label(sport), y = NULL) +
    theme_lactater() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = colours[["ink"]], size = ggplot2::rel(0.95)),
      axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank()
    )

  table <- overview_table(rows, colours, sport, intervals = !is.null(rows$low)) + bands + lines
  design <- "AB\nCD"
  patchwork::wrap_plots(
    A = top, B = patchwork::plot_spacer(), C = dots, D = table, design = design,
    widths = c(2, 1.35), heights = c(0.75, max(1.4, nrow(rows) / 7))
  ) +
    patchwork::plot_annotation(
      title = sprintf(
        "%d thresholds from %d methods, %s to %s %s", nrow(rows), length(unique(rows$family)),
        format_intensity(min(rows$intensity), sport), format_intensity(max(rows$intensity), sport),
        intensity_unit(sport)
      ),
      subtitle = paste0(
        "Each method as a dot", if(!is.null(rows$low)) " with its confidence interval" else "",
        ", grouped by family; the table gives the values at each threshold"
      ),
      caption = paste("Fit:", info$settings$fit),
      theme = theme_lactater()
    )
}

#' The table beside the overview plot
#'
#' @param rows The thresholds (one row each, with `row`).
#' @param colours The plot colours.
#' @param sport The sport.
#' @param intervals Whether the thresholds have confidence intervals.
#' @keywords internal
overview_table <- function(rows, colours, sport, intervals) {
  ## headers as plotmath, two lines each, for the subscript of HRmax
  columns <- list(
    list("lactate", quote(atop("Lactate", "mmol/L")), "%.1f", shade = TRUE),
    if(!is.null(rows$heart_rate)) list("heart_rate", quote(atop("Heart rate", "bpm")), "%.0f", shade = FALSE),
    if(!is.null(rows$rpe)) list("rpe", "RPE", "%.1f", shade = FALSE),
    list("peak", bquote(atop("% peak", .(if(sport %in% c("cycling", "rowing")) "PO" else "speed"))), "%.0f",
         shade = FALSE),
    if(!is.null(rows$hr_max)) list("hr_max", quote(atop("%", HR[max])), "%.0f", shade = FALSE),
    if(intervals) list("width", bquote(atop("CI width", .(intensity_unit(sport)))), "%.0f", shade = TRUE)
  )
  columns <- Filter(Negate(is.null), columns)
  if(intervals) rows$width <- rows$high - rows$low
  cells <- do.call(rbind, lapply(seq_along(columns), function(k) {
    column <- columns[[k]]
    values <- rows[[column[[1]]]]
    span <- diff(range(values, na.rm = TRUE))
    data.frame(
      row = rows$row, column = k, label = ifelse(is.na(values), "", sprintf(column[[3]], values)),
      shade = if(column$shade && span > 0) (values - min(values, na.rm = TRUE)) / span else NA_real_
    )
  }))
  tint <- if(identical(colours[["paper"]], "#ffffff")) colours[["curve"]] else colours[["accent"]]
  shaded <- cells[!is.na(cells$shade), ]
  ggplot2::ggplot(cells, ggplot2::aes(column, row)) +
    ggplot2::geom_tile(
      ggplot2::aes(alpha = 0.08 + 0.72 * shade), shaded,
      ## fixed size: otherwise the width is guessed from the gaps between the shaded columns
      width = 0.94, height = 0.92, fill = tint
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), size = 3,
      colour = ifelse(!is.na(cells$shade) & cells$shade > 0.6, colours[["paper"]], colours[["ink"]])
    ) +
    ggplot2::scale_alpha_identity() +
    ## half a column on each side, or the outer columns without shading sit on the panel edge and are clipped
    ggplot2::scale_x_continuous(
      breaks = seq_along(columns), labels = as.expression(lapply(columns, `[[`, 2)), position = "top",
      limits = c(0.5, length(columns) + 0.5), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0.6)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_lactater() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(size = ggplot2::rel(0.75), lineheight = 0.9, vjust = 0)
    )
}
