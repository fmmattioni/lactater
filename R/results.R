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

#' The thresholds that mark LT1 and LT2
#'
#' What the caller asked for, otherwise what [lactate_threshold()] was given, otherwise the package defaults (a result
#' saved before the choice was kept). For internal use only.
#'
#' @param info The `lactater` attribute of a result.
#' @param lt1,lt2 A method, an intensity, or `NULL`.
#' @return a list with `lt1` and `lt2`.
#' @keywords internal
chosen_pair <- function(info, lt1 = NULL, lt2 = NULL) {
  first <- function(...) Find(Negate(is.null), list(...))
  list(lt1 = first(lt1, info$settings$lt1, "Bsln + 0.5"), lt2 = first(lt2, info$settings$lt2, "Log-Poly-ModDmax"))
}

#' @rdname chosen_pair
#' @return `pair_text()`: "83 W (Bsln + 0.5)" for each of the two that was found, named `LT1` and `LT2`.
pair_text <- function(info) {
  pair <- chosen_pair(info)
  text <- vapply(pair, function(marker) {
    value <- if(is.numeric(marker)) marker else if(marker %in% names(info$intensity)) info$intensity[[marker]] else NA
    if(is.na(value)) return(NA_character_)
    sprintf("%s (%s)", intensity_text(value, info$settings$sport), if(is.numeric(marker)) "your value" else marker)
  }, "")
  names(text) <- c("LT1", "LT2")
  text[!is.na(text)]
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
    pair_text(info),
    if(warnings > 0) c("Checks" = sprintf("%d warning%s, see lactate_checks()", warnings, if(warnings > 1) "s" else ""))
  )
}

#' Plot lactate thresholds
#'
#' Plots the result of [lactate_threshold()]. Without `method`, an overview: the fitted lactate
#' curve on top and every threshold below, grouped by what the method aims at, on the same intensity axis, with a
#' shaded band for how far the methods aimed at LT1 and at LT2 lie apart, so you can see at a glance how much the
#' methods agree; the two that mark LT1 and LT2 (`lt1`, `lt2`, as chosen in
#' [lactate_threshold()]) are drawn in green. With `method`, the plot of that one method:
#' what the method fitted, the measured stages, and the threshold with its intensity, lactate and
#' heart rate.
#'
#' The plots are `ggplot2` objects: add layers, scales or a `theme()` to change them. See
#' [theme_lactater()] for the dark style (`options(lactater.style = "night")`) and the font.
#'
#' @param object,x The result of [lactate_threshold()].
#' @param method A method to plot (one of `object$method`), or `NULL` for the overview.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, drawn in green in the overview. Default to those
#'   chosen in [lactate_threshold()].
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
autoplot.lactate_thresholds <- function(object, method = NULL, lt1 = NULL, lt2 = NULL, ...) {
  info <- lactater_info(object)

  if(is.null(method)) return(plot_overview(object, info, chosen = unlist(chosen_pair(info, lt1, lt2))))

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
  print(autoplot(x, method = method, ...))
  invisible(x)
}

#' Overview plot
#'
#' Every threshold of the test on one intensity axis: the fitted curve on top, and below it one row per method,
#' grouped by what it aims at (the first rise, the second rise, or a fixed lactate), with the threshold as a dot, its
#' value beside it, and its confidence interval as a line (when computed). Two shaded bands, through the curve and the
#' rows, show how far the methods aimed at LT1 and at LT2 lie apart. The title gives LT1 and LT2 when both `chosen`
#' methods were computed. A table on the right prints the lactate, heart rate, RPE, percentage of the peak intensity
#' and of the highest heart rate at each threshold, with the lactate shaded by its value.
#'
#' @param object The result of [lactate_threshold()].
#' @param info Its `lactater` attribute.
#' @param chosen The methods that mark LT1 and LT2, drawn in green.
#' @keywords internal
plot_overview <- function(object, info, chosen = character()) {
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
  ## grouped by what the method aims at and sorted by intensity, with a free row above each group for its name
  rows$aim <- droplevels(method_aim(rows$method))
  rows <- rows[order(rows$aim, rows$intensity), ]
  rows$row <- rev(seq_len(nrow(rows))) + nlevels(rows$aim) - as.integer(rows$aim)
  heads <- tapply(rows$row, rows$aim, max) + 1
  rows$chosen <- rows$method %in% chosen
  rows$peak <- 100 * rows$intensity / max(info$data$intensity)
  if(!is.null(rows$heart_rate)) rows$hr_max <- 100 * rows$heart_rate / max(info$data$heart_rate, na.rm = TRUE)
  curve <- curve_points(info$curve)
  limits <- range(points$intensity, curve$intensity, rows$intensity, rows$low, rows$high, na.rm = TRUE)
  ## room on the right for the value beside the last dot
  x_scale <- ggplot2::scale_x_continuous(limits = limits, expand = ggplot2::expansion(mult = c(0.03, 0.09)))
  ## how far the methods aimed at LT1 and at LT2 lie apart, shaded through the curve and the dots
  spans <- do.call(rbind, lapply(1:2, function(k) {
    values <- rows$intensity[as.integer(method_aim(rows$method)) == k]
    if(length(values) > 1) {
      data.frame(name = paste0("LT", k), low = min(values), high = max(values))
    }
  }))
  apart <- if(!is.null(spans)) {
    ggplot2::annotate(
      "rect", xmin = spans$low, xmax = spans$high, ymin = -Inf, ymax = Inf, fill = colours[["accent"]], alpha = 0.12
    )
  }
  y_rows <- ggplot2::scale_y_continuous(
    breaks = c(rows$row, heads),
    labels = c(method_label(rows$method), parse(text = sprintf('bold("%s")', names(heads)))),
    limits = c(0.5, max(heads) + 0.5), expand = c(0, 0.1)
  )
  bands <- ggplot2::annotate(
    "rect", xmin = -Inf, xmax = Inf, ymin = rows$row[rows$row %% 2 == 0] - 0.5,
    ymax = rows$row[rows$row %% 2 == 0] + 0.5, fill = colours[["faint"]], alpha = 0.18
  )

  top_y <- max(points$lactate, curve$lactate) * 1.08
  top <- ggplot2::ggplot() + apart +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9) +
    stage_layers(points, info$data, colours) +
    x_scale +
    ggplot2::scale_y_continuous(limits = c(0, top_y), breaks = lactate_breaks(top_y), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Blood lactate (mmol/L)") +
    theme_lactater()
  if(!is.null(spans)) {
    top <- top + ggplot2::annotate(
      "text", x = (spans$low + spans$high) / 2, y = top_y, label = spans$name, vjust = 1.4, fontface = "bold",
      size = 4.2, colour = colours[["ink"]]
    )
  }

  rows$beside <- if(is.null(rows$high)) rows$intensity else pmax(rows$intensity, rows$high, na.rm = TRUE)
  dots <- ggplot2::ggplot(rows) + bands + apart
  if(!is.null(rows$low)) {
    dots <- dots + ggplot2::geom_segment(
      ggplot2::aes(x = low, xend = high, y = row, yend = row), colour = colours[["muted"]], alpha = 0.55,
      linewidth = 0.9, lineend = "round", na.rm = TRUE
    )
  }
  dots <- dots +
    ggplot2::geom_point(
      ggplot2::aes(intensity, row), shape = 21, size = 2.8, stroke = 0.7,
      fill = ifelse(rows$chosen, colours[["accent"]], colours[["curve"]]), colour = colours[["paper"]]
    ) +
    ## the value beside its dot, past the confidence interval when there is one
    ggplot2::geom_text(
      ggplot2::aes(beside, row, label = format_intensity(intensity, sport)),
      hjust = 0, nudge_x = 0.035 * diff(limits), size = 2.9, colour = colours[["ink"]],
      fontface = ifelse(rows$chosen, "bold", "plain")
    ) +
    x_scale + y_rows +
    ggplot2::labs(x = intensity_label(sport), y = NULL) +
    theme_lactater() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = colours[["ink"]], size = ggplot2::rel(0.95)),
      axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank()
    )

  table <- overview_table(rows, colours, sport, intervals = !is.null(rows$low)) + bands
  ## the table as wide as its columns (lactate, % peak, and heart rate, % HRmax, RPE and CI width when present), so
  ## that two-word headers such as "Heart rate" keep apart
  columns <- 2 + 2 * (!is.null(rows$heart_rate)) + (!is.null(rows$rpe)) + (!is.null(rows$low))
  design <- "AB\nCD"
  ## the corner above the table, empty but in the paper color (a spacer stays white in the night style)
  corner <- ggplot2::ggplot() + theme_lactater() + ggplot2::theme(axis.line = ggplot2::element_blank())
  counted <- sprintf("%d thresholds from %d methods", nrow(rows), length(unique(rows$family)))
  both <- length(chosen) == 2 && all(chosen %in% rows$method)
  patchwork::wrap_plots(
    A = top, B = corner, C = dots, D = table, design = design,
    widths = c(2, 0.37 * columns), heights = c(1.1, max(1.4, max(heads) / 7))
  ) +
    patchwork::plot_annotation(
      ## the answer when both thresholds of the zones are there, otherwise the count
      title = if(both) {
        sprintf(
          "LT1 at %s, LT2 at %s", intensity_text(rows$intensity[match(chosen[1], rows$method)], sport),
          intensity_text(rows$intensity[match(chosen[2], rows$method)], sport)
        )
      } else {
        sprintf(
          "%s, %s to %s", counted, format_intensity(min(rows$intensity), sport),
          intensity_text(max(rows$intensity), sport)
        )
      },
      subtitle = paste0(
        if(both) counted else "Every method", if(!is.null(rows$low)) ", with confidence intervals",
        ", grouped by what it aims at",
        if(!is.null(spans)) ". Shaded: how far the methods for one threshold lie apart",
        if(any(rows$chosen)) ". Green: what marks LT1 and LT2 in the zones"
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
    if(!is.null(rows$hr_max)) list("hr_max", quote(atop("% of", HR[max])), "%.0f", shade = FALSE),
    if(intervals) list("width", bquote(atop("CI width", .(intensity_unit(sport)))), "%.0f", shade = FALSE)
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
  tint <- colours[["curve"]]
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
    ggplot2::scale_y_continuous(limits = c(0.5, max(rows$row) + 1.5), expand = c(0, 0.1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_lactater() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(size = ggplot2::rel(0.75), lineheight = 0.9, vjust = 0)
    )
}
