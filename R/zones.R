#' Training zones from two lactate thresholds
#'
#' Builds the three-zone model from a first (LT1) and a second (LT2) lactate threshold: zone 1 below LT1,
#' zone 2 between LT1 and LT2, and zone 3 above LT2 (Hofmann & Tschakert, 2017). You choose which
#' thresholds mark LT1 and LT2.
#'
#' @details
#' LT1 is usually a "first rise" threshold (e.g. `"LTP1"`, `"Log-log"`, `"Bsln + 1.0"`), and LT2 a threshold
#' near the maximal lactate steady state (e.g. `"LTP2"`, `"ModDmax"`, `"Log-Poly-ModDmax"`, `"OBLA 4.0"`, `"IAT"`).
#' Different choices give different zones, so report which ones you used.
#'
#' @param x The result of [lactate_threshold()].
#' @param lt1,lt2 The thresholds that mark LT1 and LT2: a method in `x$method`, or an intensity. Default to
#' `"LTP1"` and `"LTP2"`, the turn points that define the zones in Hofmann & Tschakert (2017).
#'
#' @return a `lactate_zones` tibble with one row per zone: `zone`, `description`, `intensity_from`,
#' `intensity_to`, the matching `pace` range (running, swimming, rowing), `lactate` range on the fitted curve,
#' and `heart_rate` range (when available). Zone 1 has no lower bound and zone 3 no upper bound. Plot it with
#' [autoplot()].
#'
#' @references
#' Hofmann P, Tschakert G. Intensity- and duration-based options to regulate endurance training. Frontiers in
#' Physiology. 2017;8:337.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' lactate_zones(results)
#' lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
lactate_zones <- function(x, lt1 = "LTP1", lt2 = "LTP2") {
  info <- lactater_info(x)
  boundary <- function(marker, arg) {
    if(is.numeric(marker) && length(marker) == 1 && !is.na(marker)) return(marker)
    i <- match(marker, as.character(x$method))
    if(length(marker) != 1 || is.na(i))
      stop(sprintf("`%s` must be a number or one of: %s.", arg, paste(x$method, collapse = ", ")), call. = FALSE)
    if(is.na(info$intensity[i]))
      stop(sprintf("`%s` (%s) was not found in this test.", arg, marker), call. = FALSE)
    info$intensity[i]
  }
  bounds <- c(boundary(lt1, "lt1"), boundary(lt2, "lt2"))
  if(bounds[1] >= bounds[2]) stop("LT1 must be below LT2.", call. = FALSE)

  sport <- info$settings$sport
  from <- c(NA, bounds)
  to <- c(bounds, NA)
  out <- tibble::tibble(
    zone = c("Zone 1", "Zone 2", "Zone 3"),
    description = c("below LT1", "between LT1 and LT2", "above LT2"),
    intensity_from = round(from, intensity_digits(sport)),
    intensity_to = round(to, intensity_digits(sport))
  )
  if(sport != "cycling") {
    out$pace_from <- convert_to_pace(from, sport)
    out$pace_to <- convert_to_pace(to, sport)
  }
  out$lactate_from <- retrieve_lactate(info$curve, from)
  out$lactate_to <- retrieve_lactate(info$curve, to)
  if("heart_rate" %in% names(info$data)) {
    out$heart_rate_from <- retrieve_heart_rate(info$data, from)
    out$heart_rate_to <- retrieve_heart_rate(info$data, to)
  }

  attr(out, "lactater_zones") <- list(
    bounds = bounds,
    labels = c(if(is.numeric(lt1)) "LT1" else lt1, if(is.numeric(lt2)) "LT2" else lt2),
    info = info
  )
  class(out) <- c("lactate_zones", class(out))
  out
}

#' @rdname lactate_zones
#' @param object The result of [lactate_zones()].
#' @param ... Not used.
#' @export
autoplot.lactate_zones <- function(object, ...) {
  zones <- attr(object, "lactater_zones")
  info <- zones$info
  points <- position_rest(info$data)
  curve <- curve_points(info$curve)
  limits <- range(points$intensity, curve$intensity)
  bands <- data.frame(
    zone = object$zone,
    from = c(limits[1], zones$bounds),
    to = c(zones$bounds, limits[2])
  )
  bands$middle <- (bands$from + bands$to) / 2
  axis <- adjust_x_axis_plot(.data = points, has_rest = any(info$data$intensity == 0))
  unit <- switch(info$settings$sport, cycling = "W", rowing = "W", running = "km/h", swimming = "m/s")
  marks <- data.frame(intensity = zones$bounds)
  subtitle <- paste(
    sprintf("%s: %s (%s %s)", c("LT1", "LT2"), zones$labels, round(zones$bounds, intensity_digits(info$settings$sport)), unit),
    collapse = " \u00b7 "
  )

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = zone), bands,
      alpha = 0.35, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = middle, y = Inf, label = zone), bands,
      vjust = 1.6, size = 3.4, fontface = "bold", colour = lactater_colours[["ink"]]
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = intensity), marks, colour = lactater_colours[["threshold"]], linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = lactater_colours[["curve"]], linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(intensity, lactate), points,
      shape = 21, size = 3, stroke = 0.9,
      fill = lactater_colours[["points"]], colour = lactater_colours[["paper"]]
    ) +
    ggplot2::scale_fill_manual(values = c("Zone 1" = "#cde2fb", "Zone 2" = "#86b6ef", "Zone 3" = "#3987e5")) +
    ggplot2::scale_x_continuous(breaks = axis$breaks, labels = axis$labels, expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(
      title = "Training zones",
      subtitle = subtitle,
      caption = "Three-zone model (Hofmann & Tschakert, 2017)",
      x = intensity_label(info$settings$sport),
      y = quote(Lactate ~ (mmol %.% L^-1))
    ) +
    theme_lactater()
}
