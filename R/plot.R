## two styles: a blue curve and a green threshold on white (default), and a dark "night" version. Blue and
## green stay apart under color-vision deficiency (deltaE 21.7 deutan, dataviz palette validator)
lactater_palettes <- list(
  light = c(
    paper = "#ffffff", ink = "#11181f", muted = "#66707a", faint = "#b7c3cf", curve = "#1f5fa8", accent = "#0f9d74"
  ),
  night = c(
    paper = "#0f1720", ink = "#e8edf2", muted = "#93a1ae", faint = "#3a4a5a", curve = "#5aa9e6", accent = "#3fcf8e"
  ),
  ## a quieter look for print: ink curve, one amber accent
  journal = c(
    paper = "#ffffff", ink = "#22262d", muted = "#6b7280", faint = "#c9ced6", curve = "#22262d", accent = "#c47f17"
  )
)

#' Colors of the current plot style
#'
#' @param style `"light"`, `"night"` or `"journal"`.
#' @return a named vector of colors.
#' @keywords internal
lactater_colours <- function(style = getOption("lactater.style", "light")) {
  lactater_palettes[[match.arg(style, names(lactater_palettes))]]
}

#' lactater ggplot2 theme
#'
#' The theme used by every lactater plot: axis lines, no grid, and quiet text, so the lactate curve
#' and the threshold carry the color. Use it to make your own plots match.
#'
#' Two options change every lactater plot:
#'
#' * `options(lactater.style = "night")` switches to a dark version, and `"journal"` to a quieter print look (an ink
#'   curve with one amber accent). `"light"`, a blue curve with a green threshold, is the default.
#' * `options(lactater.font = "IBM Plex Sans")` sets the font of the text and the annotations. The font
#'   must be installed (see `systemfonts::require_font()`).
#'
#' The subtitle and the caption are text boxes from ggtext, so they wrap by themselves to the width of the plot.
#' Restyle them with `ggtext::element_textbox_simple()` rather than `ggplot2::element_text()`, which cannot replace
#' them.
#'
#' Plots are `ggplot2` objects, so anything else can be changed by adding layers, scales or a `theme()`.
#'
#' @param base_size Base font size, in points.
#' @param base_family Base font family.
#' @param style `"light"`, `"night"` or `"journal"`.
#'
#' @return a `ggplot2` theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(demo_data, aes(intensity, lactate)) +
#'   geom_point() +
#'   theme_lactater()
theme_lactater <- function(
  base_size = 11,
  base_family = getOption("lactater.font", ""),
  style = getOption("lactater.style", "light")
) {
  colours <- lactater_colours(style)
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    ink = colours[["ink"]],
    paper = colours[["paper"]]
  ) +
    ggplot2::theme(
      geom = ggplot2::element_geom(family = base_family),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = colours[["ink"]], linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = colours[["ink"]], linewidth = 0.4),
      axis.ticks.length = ggplot2::unit(4, "pt"),
      axis.text = ggplot2::element_text(colour = colours[["muted"]], size = ggplot2::rel(0.85)),
      axis.title = ggplot2::element_text(colour = colours[["muted"]], size = ggplot2::rel(0.85)),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.3), margin = ggplot2::margin(b = 4)),
      ## text boxes wrap by themselves to the width of the plot
      plot.subtitle = ggtext::element_textbox_simple(
        colour = colours[["muted"]], size = ggplot2::rel(0.9), lineheight = 1.1, margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggtext::element_textbox_simple(
        colour = colours[["muted"]], size = ggplot2::rel(0.7), margin = ggplot2::margin(t = 10)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(fill = colours[["paper"]], colour = NA),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.margin = ggplot2::margin_auto(12)
    )
}

#' Plot one lactate threshold
#'
#' Draws what the method fitted, the measured stages, and the threshold with a two-line label joined by
#' an arrow: the intensity, then the pace, lactate and heart rate. For internal use only.
#'
#' @param method The method label.
#' @param intensity,lactate,heart_rate,rpe The threshold values.
#' @param plot_data What the method drew on: the fitted `curve` (with a Dmax `line`, a Bsln+ `baseline`,
#' or the IAT `from` point), a `breakpoints` fit, or a `ratio` curve, and the `fitting` label.
#' @param data The data retrieved from `prepare_data()`.
#' @param settings The analysis settings (`sport`).
#' @param interval The confidence interval of the intensity (`low`, `high`), or `NULL`.
#'
#' @return a `ggplot2` object, or `NA` when the threshold could not be estimated.
#' @keywords internal
plot_threshold <- function(method, intensity, lactate, heart_rate, plot_data, data, settings, interval = NULL,
                           rpe = NA) {
  if(is.na(intensity)) return(NA)
  colours <- lactater_colours()
  points <- position_rest(data)
  log_axes <- isTRUE(plot_data$log)
  ratio <- !is.null(plot_data$ratio)

  if(ratio) {
    ## LTratio: the minimum of lactate / intensity
    points <- points[data$intensity > 0, ]
    points$lactate <- points$lactate / points$intensity
    drawn <- list(
      layers = list(ggplot2::geom_line(
        ggplot2::aes(intensity, ratio), plot_data$ratio, colour = colours[["curve"]], linewidth = 0.9
      )),
      y = plot_data$ratio_min,
      top = max(plot_data$ratio$ratio)
    )
  } else if(!is.null(plot_data$breakpoints)) {
    if(log_axes) points <- points[data$intensity > 0, ]
    drawn <- breakpoint_layers(plot_data$breakpoints, method, intensity, log_axes, colours, settings$sport)
  } else if(!is.null(plot_data$tangent)) {
    drawn <- stegmann_layers(plot_data, colours)
  } else {
    drawn <- curve_layers(plot_data, intensity, lactate, colours, settings$sport)
  }

  x_range <- range(points$intensity, drawn$x_range)
  y_range <- c(if(log_axes) min(points$lactate) else 0, max(points$lactate, drawn$top, drawn$y) * 1.06)
  label <- threshold_text(intensity, lactate, heart_rate, settings$sport, interval, rpe)

  p <- ggplot2::ggplot()
  if(!is.null(interval) && !anyNA(interval)) {
    p <- p + ggplot2::annotate(
      "rect", xmin = interval[["low"]], xmax = interval[["high"]], ymin = -Inf, ymax = Inf,
      fill = colours[["accent"]], alpha = 0.12
    )
  }
  p <- p +
    drawn$layers +
    stage_layers(points, data, colours, label_rest = !ratio && !log_axes) +
    marker_layers(intensity, drawn$y, label, x_range, y_range, log_axes, colours, below = isTRUE(drawn$below)) +
    ggplot2::labs(
      title = method_label(method, bold = TRUE),
      subtitle = method_note(method, settings),
      caption = paste("Fit:", plot_data$fitting),
      x = intensity_label(settings$sport),
      y = if(ratio) "Lactate / intensity" else "Blood lactate (mmol/L)"
    ) +
    theme_lactater()

  if(log_axes) {
    return(
      p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10(breaks = 2^(-2:5)) + ggplot2::coord_cartesian(clip = "off")
    )
  }
  p +
    ggplot2::scale_y_continuous(
      limits = c(0, y_range[2]), breaks = if(ratio) ggplot2::waiver() else lactate_breaks(y_range[2]),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::coord_cartesian(clip = "off")
}

#' Layers of a method drawn on the fitted curve
#'
#' The curve, plus the Dmax line and the gap to it, the Bsln+ baseline and rise, or the IAT rise from
#' LTratio. For internal use only.
#'
#' @inheritParams plot_threshold
#' @param colours The plot colours.
#' @param sport The sport, for the unit of the LTratio label.
#' @return a list with the `layers`, the lactate `y` where the threshold sits, and the `top` of the drawing.
#' @keywords internal
curve_layers <- function(plot_data, intensity, lactate, colours, sport) {
  layers <- list()
  top <- max(plot_data$curve$lactate)
  ## notes sit just beside the threshold, clear of its dotted guide
  beside <- intensity + 0.012 * diff(range(plot_data$curve$intensity))
  if(!is.null(plot_data$line)) {
    ## Dmax: the line; the distance to it is perpendicular, and how a perpendicular looks depends on the aspect
    ## ratio of the plot, so it is not drawn
    layers <- c(layers, list(
      ggplot2::geom_line(
        ggplot2::aes(intensity, lactate), plot_data$line, colour = colours[["faint"]], linewidth = 0.5
      )
    ))
    top <- max(top, plot_data$line$lactate)
  }
  from <- if(!is.null(plot_data$baseline)) {
    c(intensity = NA, lactate = plot_data$baseline, label = "baseline")
  } else if(!is.null(plot_data$from)) {
    c(plot_data$from, label = "LTratio")
  }
  if(!is.null(from)) {
    ## Bsln+ and IAT: the rise from the baseline or from LTratio
    base <- as.numeric(from[["lactate"]])
    rise <- paste0("  +", format(round(lactate - base, 2), nsmall = 1))
    layers <- c(layers, list(
      ggplot2::annotate(
        "segment", x = -Inf, xend = intensity, y = base, yend = base, colour = colours[["faint"]],
        linewidth = 0.45, linetype = "22"
      ),
      ggplot2::annotate(
        "segment", x = beside, xend = beside, y = base, yend = lactate, colour = colours[["accent"]],
        linewidth = 0.5, arrow = ggplot2::arrow(length = ggplot2::unit(0.05, "inch"), type = "closed")
      ),
      ggplot2::annotate(
        "text", x = beside, y = (base + lactate) / 2, label = rise, hjust = 0, size = 3, colour = colours[["muted"]]
      )
    ))
    if(from[["label"]] == "LTratio") {
      at <- as.numeric(from[["intensity"]])
      layers <- c(layers, other_point(at, base, sprintf('"LT"[ratio]~~"%s"', intensity_text(at, sport)), colours))
    }
  }
  list(
    layers = c(
      list(ggplot2::geom_line(
        ggplot2::aes(intensity, lactate), plot_data$curve, colour = colours[["curve"]], linewidth = 0.9
      )),
      layers
    ),
    y = lactate,
    top = top,
    ## the Dmax line runs above the curve, so the label goes below it
    below = !is.null(plot_data$line)
  )
}

#' Layers of a breakpoint method
#'
#' The fitted straight lines (solid where they were fitted, dashed beyond), and for LTP the other turn
#' point. For internal use only.
#'
#' @param fit A fit from `fit_breakpoints()` or `fit_two_lines()`.
#' @inheritParams plot_threshold
#' @param log_axes Whether the fit is in log-log space.
#' @param colours The plot colours.
#' @param sport The sport, for the unit of the other turn point.
#' @return a list with the `layers`, the lactate `y` where the threshold sits, and the `top`.
#' @keywords internal
breakpoint_layers <- function(fit, method, intensity, log_axes, colours, sport) {
  back <- if(log_axes) exp else identity
  x <- seq(min(fit$x), max(fit$x), length.out = 300)
  lines <- breakpoint_lines(fit)
  bounds <- c(min(fit$x), fit$psi, max(fit$x))
  segments <- do.call(rbind, lapply(seq_along(lines), function(k) {
    y <- lines[[k]](x)
    fitted <- x >= bounds[k] & x <= bounds[k + 1]
    data.frame(x = back(x), y = back(y), line = k, fitted = fitted)
  }))
  ## the dashed extensions stop near the measured range
  y_range <- range(fit$y)
  lowest <- if(log_axes) back(y_range[1]) / 1.35 else 0
  segments <- segments[segments$fitted | (segments$y > lowest & segments$y < back(y_range[2]) * 1.05), ]
  at <- if(log_axes) log(intensity) else intensity
  layers <- list(
    ggplot2::geom_line(
      ggplot2::aes(x, y, group = interaction(line, fitted)), segments[!segments$fitted, ],
      colour = colours[["faint"]], linewidth = 0.45, linetype = "22"
    ),
    ggplot2::geom_line(
      ggplot2::aes(x, y, group = line), segments[segments$fitted, ], colour = colours[["curve"]], linewidth = 0.9
    )
  )
  if(method %in% c("LTP1", "LTP2") && length(fit$psi) == 2) {
    other <- if(method == "LTP1") 2 else 1
    layers <- c(layers, other_point(
      fit$psi[other], predict_breakpoints(fit, fit$psi[other]),
      sprintf('"LTP%d"~~"%s"', other, intensity_text(fit$psi[other], sport)), colours
    ))
  }
  list(
    layers = layers,
    y = back(predict_breakpoints(fit, at)),
    top = back(y_range[2]),
    x_range = range(back(fit$x))
  )
}

#' Layers of the Stegmann method
#'
#' The fitted curve, the recovery samples placed after the last stage (labeled with their minutes), the point where
#' lactate is back at its end-of-exercise value, and the tangent from there to the curve. For internal use only.
#'
#' @param plot_data The method's plot data, with the `curve` and the `tangent`.
#' @param colours The plot colours.
#' @return a list with the `layers`, the lactate `y` where the threshold sits, the `top` and the `x_range`.
#' @keywords internal
stegmann_layers <- function(plot_data, colours) {
  tangent <- plot_data$tangent
  curve <- plot_data$curve
  recovery <- tangent$recovery
  end <- curve[nrow(curve), ]
  layers <- list(
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9),
    ggplot2::geom_path(
      ggplot2::aes(intensity, lactate), rbind(end, recovery[c("intensity", "lactate")]),
      colour = colours[["faint"]], linewidth = 0.45, linetype = "22"
    ),
    stage_points(recovery, colours, outline = colours[["muted"]]),
    ggplot2::annotate(
      "text", x = recovery$intensity, y = recovery$lactate, label = paste(format(recovery$time), "min"),
      vjust = -1.1, size = 2.8, colour = colours[["muted"]]
    )
  )
  y <- NA_real_
  if(!is.null(tangent$from)) {
    from <- tangent$from
    y <- stats::approx(curve$intensity, curve$lactate, xout = tangent$intensity)$y
    layers <- c(layers, list(
      ggplot2::annotate(
        "segment", x = tangent$intensity, y = y, xend = from[["intensity"]], yend = from[["lactate"]],
        colour = colours[["faint"]], linewidth = 0.5
      ),
      ggplot2::annotate(
        "point", x = from[["intensity"]], y = from[["lactate"]], shape = 21, size = 2.6, stroke = 0.9,
        fill = colours[["paper"]], colour = colours[["accent"]]
      ),
      ggplot2::annotate(
        ## below the point, where the tangent and the recovery samples leave room
        "text", x = from[["intensity"]], y = from[["lactate"]] - 0.2 * max(curve$lactate, recovery$lactate), size = 2.8,
        colour = colours[["muted"]], label = sprintf("back to %.1f mmol/L at %.1f min", from[["lactate"]], tangent$back)
      )
    ))
  }
  list(
    layers = layers,
    y = y,
    top = max(curve$lactate, recovery$lactate),
    x_range = range(curve$intensity, recovery$intensity),
    below = TRUE
  )
}

#' The straight lines of a breakpoint fit
#'
#' @param fit A fit from `fit_breakpoints()` or `fit_two_lines()`.
#' @return a list of functions, one line per segment, each defined over all `x`.
#' @keywords internal
breakpoint_lines <- function(fit) {
  if(!is.null(fit$lines)) {
    return(lapply(1:2, function(k) function(x) fit$lines[k, 1] + fit$lines[k, 2] * x))
  }
  ## a continuous hinge: segment k is the first line plus the slope changes at the first k breakpoints
  slope <- if(fit$flat_first) 0 else fit$coefficients[[2]]
  changes <- fit$coefficients[seq_along(fit$psi) + length(fit$coefficients) - length(fit$psi)]
  lapply(0:length(fit$psi), function(k) {
    j <- seq_len(k)
    function(x) fit$coefficients[[1]] + slope * x + colSums(changes[j] * outer(fit$psi[j], x, function(p, v) v - p))
  })
}

#' A secondary point with a small label
#'
#' @param x,y Where the point sits.
#' @param label The label below it, as plotmath (a quoted string, or e.g. `"LT"[ratio]`).
#' @param colours The plot colours.
#' @keywords internal
other_point <- function(x, y, label, colours) {
  list(
    ggplot2::annotate(
      "point", x = x, y = y, shape = 21, size = 3, stroke = 1, fill = colours[["paper"]], colour = colours[["accent"]]
    ),
    ggplot2::annotate(
      "text", x = x, y = y, label = label, parse = TRUE, hjust = -0.1, vjust = 1.4, size = 3,
      colour = colours[["muted"]]
    )
  )
}

#' Measured stages and the resting sample
#'
#' The stages as open dots; the resting sample, placed one step before the first stage, fainter and
#' labeled. For internal use only.
#'
#' @param points The stages to draw (`intensity`, `lactate`), with the resting sample positioned.
#' @param data The data retrieved from `prepare_data()`, to tell the resting sample apart.
#' @param colours The plot colours.
#' @param label_rest Whether to label the resting sample.
#' @keywords internal
stage_layers <- function(points, data, colours, label_rest = TRUE) {
  rest <- if(nrow(points) == nrow(data)) points[data$intensity == 0, ] else points[0, ]
  stages <- points[!points$intensity %in% rest$intensity, ]
  list(
    if(nrow(rest) > 0) stage_points(rest, colours, outline = colours[["faint"]]),
    if(nrow(rest) > 0 && label_rest) {
      ggplot2::annotate("text", x = rest$intensity, y = rest$lactate, label = "rest", vjust = -1.2, size = 2.9,
                        colour = colours[["muted"]])
    },
    stage_points(stages, colours)
  )
}

#' Measured stages layer
#'
#' The measured stages as open dots.
#'
#' @param points The stages to draw (`intensity`, `lactate`).
#' @param colours The plot colours.
#' @param outline The outline color.
#' @keywords internal
stage_points <- function(points, colours, outline = colours[["curve"]]) {
  ggplot2::geom_point(
    ggplot2::aes(intensity, lactate), points,
    shape = 21, size = 2.4, stroke = 0.8, fill = colours[["paper"]], colour = outline
  )
}

#' The threshold: dot, dotted guides to both axes, and the label joined by a curved arrow
#'
#' The label sits above the threshold, to the left when there is room (above a rising curve the plot is
#' empty) and to the right otherwise; with `below`, below the curve and to the right. For internal use only.
#'
#' @param x,y Where the threshold sits.
#' @param label A list with the `value` and the `detail` lines.
#' @param x_range,y_range The ranges of the plot.
#' @param log_axes Whether both axes are logarithmic.
#' @param colours The plot colours.
#' @param below Whether to put the label below the curve.
#' @keywords internal
marker_layers <- function(x, y, label, x_range, y_range, log_axes, colours, below = FALSE) {
  to <- if(log_axes) log10 else identity
  from <- if(log_axes) function(v) 10^v else identity
  xs <- to(x_range)
  ys <- to(y_range)
  width <- diff(xs)
  height <- diff(ys)
  position <- (to(x) - xs[1]) / width
  left <- !below && position > 0.4
  side <- if(left) -1 else 1
  label_x <- to(x) + side * 0.1 * width
  ## below the curve near the right edge, the label aligns to that edge so it stays inside
  flush_right <- below && position > 0.55
  if(flush_right) label_x <- xs[2]
  hjust <- if(left || flush_right) 1 else 0
  arrow_x <- if(flush_right) to(x) + 0.12 * width else label_x
  label_y <- if(below) {
    max(to(y) - 0.22 * height, ys[1] + 0.16 * height)
  } else {
    min(to(y) + 0.26 * height, ys[2] - 0.08 * height)
  }
  ## the arrow leaves the label from its side nearest the threshold
  arrow_from <- if(below) label_y + 0.11 * height else label_y - 0.09 * height
  guide_from <- if(log_axes) c(x_range[1], y_range[1]) else c(-Inf, 0)
  list(
    ggplot2::annotate(
      "segment", x = x, xend = x, y = guide_from[2], yend = y, colour = colours[["accent"]], linewidth = 0.45,
      linetype = "13"
    ),
    ggplot2::annotate(
      "segment", x = guide_from[1], xend = x, y = y, yend = y, colour = colours[["accent"]], linewidth = 0.45,
      linetype = "13"
    ),
    pointer_layer(
      c(from(arrow_x), from(arrow_from)), c(x, y), bend = if(left || below) 0.15 else -0.15, colours[["muted"]]
    ),
    ggplot2::annotate(
      "point", x = x, y = y, shape = 21, size = 4.2, stroke = 1.2,
      fill = colours[["accent"]], colour = colours[["paper"]]
    ),
    ggplot2::annotate(
      "text", x = from(label_x), y = from(label_y), label = label$value, hjust = hjust, vjust = 0,
      fontface = "bold", size = 6, colour = colours[["ink"]]
    ),
    ggplot2::annotate(
      "text", x = from(label_x), y = from(label_y - 0.015 * height), label = label$detail, hjust = hjust,
      vjust = 1, size = 3.2, colour = colours[["muted"]]
    )
  )
}

#' Threshold label
#'
#' @param intensity,lactate,heart_rate,rpe The threshold values.
#' @param sport The sport, which sets the intensity unit.
#' @param interval The confidence interval of the intensity (`low`, `high`), or `NULL`.
#'
#' @return a list with the `value` (the intensity, e.g. `"140 W"`) and the `detail` line (the interval, pace,
#' lactate and heart rate, joined with middle dots).
#' @keywords internal
threshold_text <- function(intensity, lactate, heart_rate, sport, interval = NULL, rpe = NA) {
  unit <- intensity_unit(sport)
  detail <- c(
    if(!is.null(interval) && !anyNA(interval)) {
      sprintf("CI %s-%s", format_intensity(interval[["low"]], sport), format_intensity(interval[["high"]], sport))
    },
    if(sport != "cycling") format_pace(convert_to_pace(intensity, sport), sport),
    sprintf("%.1f mmol/L", lactate),
    if(!is.null(heart_rate) && !is.na(heart_rate)) paste(heart_rate, "bpm"),
    if(!is.null(rpe) && !is.na(rpe)) paste("RPE", format(rpe))
  )
  list(value = paste(format_intensity(intensity, sport), unit), detail = paste(detail, collapse = " \u00b7 "))
}

#' What each method does, in one line
#'
#' @param method The method label.
#' @param settings The analysis settings.
#' @return the plot subtitle.
#' @keywords internal
method_note <- function(method, settings) {
  dmax <- "The point of the fitted curve furthest below the line from %s to the last stage (%s)"
  note <- switch(
    sub(" .*", "", method),
    "Log-log" = "Where two straight lines through log lactate and log intensity cross (Beaver et al., 1985)",
    "OBLA" = sprintf("Where the fitted curve reaches %s mmol/L", sub("OBLA ", "", method)),
    "Bsln" = sprintf("Where the fitted curve reaches %s mmol/L above the baseline", sub("Bsln \\+ ", "", method)),
    "Dmax" = sprintf(dmax, "the first stage", "Cheng et al., 1992"),
    "ModDmax" = sprintf(
      dmax, sprintf("the stage before the first rise of %s mmol/L", format(settings$moddmax_rise)),
      "Bishop et al., 1998"
    ),
    "Exp-Dmax" = sprintf(dmax, "the first stage", "exponential curve; Hughson et al., 1987"),
    "Log-Poly-ModDmax" = sprintf(dmax, "the Log-log threshold", "Jamnick et al., 2018"),
    "Log-Exp-ModDmax" = sprintf(dmax, "the Log-log threshold", "exponential curve; Jamnick et al., 2018"),
    "LTP1-Poly-ModDmax" = sprintf(dmax, "LTP1", "3rd degree polynomial"),
    "LTP1" = "The first turn point of straight segments fitted to the stages (Hofmann et al., 1997)",
    "LTP2" = "The second turn point of three straight segments fitted to the stages (Hofmann et al., 1997)",
    "LTratio" = "The lowest lactate / intensity ratio on a spline (Dickhuth et al., 1999)",
    "IAT" = sprintf(
      "%s mmol/L above the lactate at LT<sub>ratio</sub> (Dickhuth et al., 1999)", format(settings$iat_rise)
    ),
    "Stegmann" = paste(
      "The tangent to the lactate curve from the point in recovery where lactate is back at its end-of-exercise value",
      "(Stegmann et al., 1981)"
    ),
    ""
  )
  note
}

#' Method names with their subscripts
#'
#' Plotmath, so that `"ModDmax"` is drawn as ModD with a subscript max, `"LTratio"` as LT with a subscript ratio, and
#' `"VO2max"` with a dot over the V.
#' Plotmath keeps the labels plain `element_text()`, which users can restyle. `method_plotmath()` gives the text to
#' parse (for `label_parsed` and `parse = TRUE`), `method_label()` the parsed expressions, and `method_html()` the same
#' names for HTML.
#'
#' @param x Method names.
#' @param bold Whether to draw them in bold, for titles.
#' @return a character vector, or an expression vector.
#' @keywords internal
method_plotmath <- function(x) {
  x <- as.character(x)
  out <- sprintf('"%s"', x)
  dmax <- grepl("Dmax", x, fixed = TRUE)
  out[dmax] <- sprintf('"%sD"[max]*"%s"', sub("Dmax.*", "", x[dmax]), sub(".*Dmax", "", x[dmax]))
  out[x == "LTratio"] <- '"LT"[ratio]'
  out[x == "VO2max"] <- "dot(V)*O[2*max]"
  out
}

#' @rdname method_plotmath
method_html <- function(x) {
  gsub("\\bLTratio\\b", "LT<sub>ratio</sub>", gsub("Dmax", "D<sub>max</sub>", x, fixed = TRUE))
}

#' @rdname method_plotmath
method_label <- function(x, bold = FALSE) {
  text <- method_plotmath(x)
  parse(text = if(bold) sprintf("bold(%s)", text) else text)
}

#' An intensity with its unit
#'
#' @param x An intensity.
#' @param sport The sport.
#' @keywords internal
intensity_text <- function(x, sport) {
  paste(format_intensity(x, sport), intensity_unit(sport))
}

#' A curved arrow that ends on the edge of a dot
#'
#' The arrow is laid out in inches when the plot is drawn, so it stops at the edge of the dot and points at its
#' center whatever the size of the plot. For internal use only.
#'
#' @param from,to The label end and the dot center, in data units (ggplot2 applies the scale transformation).
#' @param bend How far the middle of the arrow bows out, as a share of its length; the sign sets the side.
#' @param colour The arrow color.
#' @param gap Where the arrow stops, in mm from the dot center.
#' @keywords internal
pointer_layer <- function(from, to, bend, colour, gap = 2.6) {
  ## annotation_custom draws in the box between the two points, where 0 and 1 are its edges
  grob <- grid::gTree(
    from = as.numeric(from > to), to = as.numeric(to > from), bend = bend, colour = colour, gap = gap,
    cl = "lactater_pointer"
  )
  ggplot2::annotation_custom(
    grob, xmin = min(from[1], to[1]), xmax = max(from[1], to[1]), ymin = min(from[2], to[2]),
    ymax = max(from[2], to[2])
  )
}

#' @exportS3Method grid::makeContent
makeContent.lactater_pointer <- function(x) {
  inches <- function(at) {
    c(grid::convertX(grid::unit(at[1], "npc"), "in", TRUE), grid::convertY(grid::unit(at[2], "npc"), "in", TRUE))
  }
  start <- inches(x$from)
  center <- inches(x$to)
  chord <- center - start
  span <- sqrt(sum(chord^2))
  ## a quadratic Bezier whose control point is off the middle of the chord; it ends where the line from the control
  ## point to the center meets the edge, so it arrives pointing at the center
  control <- start + chord / 2 + x$bend * c(-chord[2], chord[1])
  towards <- (center - control) / sqrt(sum((center - control)^2))
  end <- center - x$gap / 25.4 * towards
  t <- seq(0, 1, length.out = 40)
  path <- outer((1 - t)^2, start) + outer(2 * (1 - t) * t, control) + outer(t^2, end)
  grid::setChildren(x, grid::gList(if(span > x$gap / 25.4) {
    grid::linesGrob(
      path[, 1], path[, 2], default.units = "in",
      gp = grid::gpar(col = x$colour, fill = x$colour, lwd = 0.4 * ggplot2::.pt),
      arrow = grid::arrow(length = grid::unit(0.065, "inch"), type = "closed")
    )
  }))
}

#' Lactate axis breaks
#'
#' Whole numbers: every 1, 2 or 4 mmol/L depending on the range.
#'
#' @param top The top of the axis.
#' @keywords internal
lactate_breaks <- function(top) {
  seq(0, top, by = if(top > 14) 4 else if(top > 5) 2 else 1)
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
