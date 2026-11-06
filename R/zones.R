## zone colors by intensity domain, as usual in the literature: greens (moderate), yellow (heavy), orange and red
## (severe); soft tones that still print
zone_colours <- list(
  "3" = c("#3f9e5a", "#e3b93c", "#d0533f"),
  "5" = c("#9fcfa8", "#3f9e5a", "#e3b93c", "#e8853a", "#d0533f")
)

#' Training zones from two lactate thresholds
#'
#' Builds three or five training zones from a first (LT1) and a second (LT2) lactate threshold, with the zone
#' boundaries placed at the boundaries of the exercise intensity domains: below LT1 is the moderate domain, from LT1
#' to LT2 the heavy domain, and above LT2 the severe domain (Jamnick et al., 2020). You choose which thresholds mark
#' LT1 and LT2.
#'
#' * **Three zones** (`model = 3`): zone 1 below LT1, zone 2 from LT1 to LT2, zone 3 above LT2 (Hofmann & Tschakert,
#'   2017).
#' * **Five zones** (`model = 5`): zones 1 and 2 split the moderate domain, zone 3 is the heavy domain, and zones 4 and
#'   5 split the severe domain, as in the five-zone scale described by Seiler (2010) and Tonnessen et al. (2014),
#'   where zones 1-2 lie below LT1, zone 3 between LT1 and LT2, and zones 4-5 above LT2. The two splits inside a
#'   domain are set by `split`:
#'   * `"relative"` (default): zone 1|2 at 75% of LT1, and zone 4|5 halfway between LT2 and the highest intensity of
#'     the test. In the five-zone scale of Seiler (2010, Table 1), zone 2 starts at 66% of the maximal oxygen uptake
#'     and zone 3 at 81%; with the oxygen cost of cycling and running (Glass & Dwyer, 2007), that puts zone 2 at about
#'     77% of the power at LT1 and 80% of the running speed. Sitko et al. (2025) place zone 2 "immediately below" LT1,
#'     at about 80% to 90% of the heart rate at LT1, which on the cycling tests of [lactate_tests] starts it at a median
#'     of about 65% of the power at LT1 (where that heart rate falls within the stages). 75% lies between the two and
#'     needs no heart rate. Zone 5 starts at 94% of the maximal
#'     oxygen uptake, halfway between the start of zone 4 (88%) and the maximum. Both cuts are approximations: no
#'     physiological marker separates them (Bishop et al., 2025), and the scale's zones are "somewhat arbitrary"
#'     (Seiler, 2010).
#'   * `"seiler"`: where the fitted curve reaches 1.5 mmol/L (zone 1|2) and 6.0 mmol/L (zone 4|5), the blood lactate
#'     limits of the five-zone scale in Seiler (2010). A cut that falls outside its domain, or is not reached, uses
#'     the relative split instead, with a warning.
#'   * Two intensities, `c(zone 1|2, zone 4|5)`, for your own cuts.
#'
#' @details
#' LT1 is usually a "first rise" threshold (e.g. `"Bsln + 0.5"`, `"Log-log"`, `"LTP1"`), and LT2 a threshold meant to
#' estimate the maximal lactate steady state (MLSS; e.g. `"Log-Poly-ModDmax"`, `"Bsln + 1.5"`, `"LTP2"`, `"ModDmax"`,
#' `"OBLA 4.0"`, `"IAT"`). Against a measured MLSS these landed from about 40 W below to 40 W above it on average, by
#' method, protocol and athletes (Hauser et al., 2014; Ploszczyca et al., 2020), and whether the MLSS or the critical
#' power marks the boundary between the heavy and severe domains is debated (Jones et al., 2019). Different choices
#' give different zones, so report which ones you used.
#'
#' The default pair has the best evidence for each boundary and moves least with a single stage. Bsln + 0.5 is among
#' the most defensible markers of the moderate-heavy boundary (Jamnick et al., 2020), and Log-Poly-ModDmax estimated the
#' MLSS with 4-min stages (Jamnick et al., 2018). Left out one stage at a time on the cycling tests of
#' [lactate_tests], Bsln + 0.5 moved by a median 1.7% (jackknife standard error) and Log-Poly-ModDmax by 8.6%, against
#' 23% for LTP1, whose three-segment fit has few stages to spare in a step test. Bsln + 0.5 rests on a single baseline
#' sample, so check that sample (see [lactate_diagnostics()]).
#'
#' @param x The result of [lactate_threshold()].
#' @param lt1,lt2 The thresholds that mark LT1 and LT2: a method in `x$method`, or an intensity. Default to
#' `"Bsln + 0.5"` and `"Log-Poly-ModDmax"`; see `Details`.
#' @param model The number of zones: `3` (default) or `5`.
#' @param split How to split the moderate and severe domains in the five-zone model: `"relative"` (default),
#' `"seiler"`, or two intensities. Ignored with three zones.
#'
#' @return a `lactate_zones` tibble with one row per zone: `zone`, `domain`, `description`, `intensity_from`,
#' `intensity_to`, the matching `pace` range (running, swimming, rowing), the `lactate` range on the fitted curve
#' (`NA` at a boundary outside the stages, where the curve is not extrapolated), and the `heart_rate` and `rpe` ranges
#' (when available; a boundary below the first stage is extrapolated from the first three stages). The first zone has
#' no lower bound and the last no upper bound.
#' Plot it with [autoplot()].
#'
#' @references
#' Hofmann P, Tschakert G. Intensity- and duration-based options to regulate endurance training. Frontiers in
#' Physiology. 2017;8:337.
#'
#' Bishop DJ, Beck B, Biddle SJH, et al. Physical activity and exercise intensity terminology: a joint American
#' College of Sports Medicine (ACSM) expert statement and Exercise and Sport Science Australia (ESSA) consensus
#' statement. Medicine & Science in Sports & Exercise. 2025;57(11):2599-2613. doi:10.1249/MSS.0000000000003795
#'
#' Glass S, Dwyer GB, editors. ACSM's metabolic calculations handbook. Baltimore: Lippincott Williams & Wilkins; 2007.
#'
#' Hauser T, Adam J, Schulz H. Comparison of selected lactate threshold parameters with maximal lactate steady state in
#' cycling. International Journal of Sports Medicine. 2014;35(6):517-521.
#'
#' Jamnick NA, Botella J, Pyne DB, Bishop DJ. Manipulating graded exercise test variables affects the validity of the
#' lactate threshold and VO2peak. PLOS ONE. 2018;13(7):e0199794.
#'
#' Jamnick NA, Pettitt RW, Granata C, Pyne DB, Bishop DJ. An examination and critique of current methods to determine
#' exercise intensity. Sports Medicine. 2020;50(10):1729-1756.
#'
#' Jones AM, Burnley M, Black MI, Poole DC, Vanhatalo A. The maximal metabolic steady state: redefining the 'gold
#' standard'. Physiological Reports. 2019;7(10):e14098.
#'
#' Ploszczyca K, Jazic D, Piotrowicz Z, Chalimoniuk M, Langfort J, Czuba M. Comparison of maximal lactate steady state
#' with anaerobic threshold determined by various methods based on graded exercise test with 3-minute stages in elite
#' cyclists. BMC Sports Science, Medicine and Rehabilitation. 2020;12:70.
#'
#' Seiler S. What is best practice for training intensity and duration distribution in endurance athletes?
#' International Journal of Sports Physiology and Performance. 2010;5(3):276-291.
#'
#' Sitko S, Artetxe X, Bonnevie-Svendsen M, et al. What is "zone 2 training"?: experts' viewpoint on definition,
#' training methods, and expected adaptations. International Journal of Sports Physiology and Performance.
#' 2025;20(11):1614-1617. doi:10.1123/ijspp.2024-0303
#'
#' Tonnessen E, Sylta O, Haugen TA, Hem E, Svendsen IS, Seiler S. The road to gold: training and peaking
#' characteristics in the year prior to a gold medal endurance performance. PLOS ONE. 2014;9(7):e101796.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' lactate_zones(results)
#' lactate_zones(results, model = 5)
#' lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = "seiler")
lactate_zones <- function(x, lt1 = "Bsln + 0.5", lt2 = "Log-Poly-ModDmax", model = 3, split = "relative") {
  info <- lactater_info(x)
  if(!identical(as.numeric(model), 3) && !identical(as.numeric(model), 5))
    stop("`model` must be 3 or 5.", call. = FALSE)
  found <- as.character(x$method)[!is.na(info$intensity[as.character(x$method)])]
  boundary <- function(marker, arg) {
    if(is.numeric(marker) && length(marker) == 1 && !is.na(marker)) return(marker)
    ## the default pair is not in every result (another `method`, or no Log-log crossing): say how to choose
    default <- if(identical(marker, c(lt1 = "Bsln + 0.5", lt2 = "Log-Poly-ModDmax")[[arg]])) ", the default" else ""
    choose <- sprintf("Choose it with `%s =`: a number or one of %s.", arg, paste(found, collapse = ", "))
    i <- match(marker, as.character(x$method))
    if(length(marker) != 1 || is.na(i))
      stop(sprintf("`%s` (%s%s) is not in these results. %s", arg, paste(marker, collapse = ", "), default, choose),
           call. = FALSE)
    if(is.na(info$intensity[[marker]]))
      stop(sprintf("`%s` (%s%s) was not found in this test. %s", arg, marker, default, choose), call. = FALSE)
    info$intensity[[marker]]
  }
  lt <- c(boundary(lt1, "lt1"), boundary(lt2, "lt2"))
  if(lt[1] >= lt[2]) stop("LT1 must be below LT2.", call. = FALSE)

  if(model == 3) {
    bounds <- lt
    domain <- c("moderate", "heavy", "severe")
    description <- c("below LT1", "LT1 to LT2", "above LT2")
  } else {
    bounds <- c(domain_splits(split, lt, info)[1], lt, domain_splits(split, lt, info)[2])
    domain <- c("moderate", "moderate", "heavy", "severe", "severe")
    description <- c("recovery", "endurance", "tempo", "threshold", "VO2max")
  }

  sport <- info$settings$sport
  from <- c(NA, bounds)
  to <- c(bounds, NA)
  out <- tibble::tibble(
    zone = paste("Zone", seq_along(from)),
    domain = domain,
    description = description,
    intensity_from = round(from, intensity_digits(sport)),
    intensity_to = round(to, intensity_digits(sport))
  )
  if(sport != "cycling") {
    out$pace_from <- convert_to_pace(from, sport)
    out$pace_to <- convert_to_pace(to, sport)
  }
  ## a fitted polynomial bends anywhere outside the stages (to -1.5 mmol/L below a runner's first stage), so the
  ## lactate of a boundary there is not known
  stages <- range(info$data$intensity[info$data$intensity > 0])
  on_curve <- function(x) {
    ifelse(!is.na(x) & x >= stages[1] & x <= stages[2], retrieve_lactate(info$curve, x), NA_real_)
  }
  out$lactate_from <- on_curve(from)
  out$lactate_to <- on_curve(to)
  if("heart_rate" %in% names(info$data)) {
    out$heart_rate_from <- zone_value(info$data, "heart_rate", from, digits = 0)
    out$heart_rate_to <- zone_value(info$data, "heart_rate", to, digits = 0)
  }
  if("rpe" %in% names(info$data)) {
    lowest <- if(info$settings$rpe_scale == "6-20") 6 else 0
    out$rpe_from <- zone_value(info$data, "rpe", from, digits = 1, lowest = lowest)
    out$rpe_to <- zone_value(info$data, "rpe", to, digits = 1, lowest = lowest)
  }

  attr(out, "lactater_zones") <- list(
    bounds = bounds,
    lt = lt,
    labels = c(if(is.numeric(lt1)) "LT1" else lt1, if(is.numeric(lt2)) "LT2" else lt2),
    model = as.numeric(model),
    info = info
  )
  class(out) <- c("lactate_zones", class(out))
  out
}

#' The heart rate at a threshold, labelled beside its point
#'
#' Its value over "at LT1", right of the point and under the line, where no stage sits, or left of it and above near
#' the right edge or the bottom of the panel. The offsets are in points, so the label hugs its point at any plot size.
#' For internal use only.
#'
#' @param x,y The point: the threshold and its heart rate.
#' @param lt `"LT1"` or `"LT2"`.
#' @param xlim,ylim The limits of the panel.
#' @param colours The plot colours.
#' @return an `annotation_custom()` layer.
#' @keywords internal
hr_point_label <- function(x, y, lt, xlim, ylim, colours) {
  below <- (xlim[2] - x) / diff(xlim) > 0.2 && (y - ylim[1]) / diff(ylim) > 0.35
  side <- if(below) 1 else -1
  line <- function(label, from_point, size, ...) {
    grid::textGrob(
      label, x = grid::unit(0.5, "npc") + grid::unit(side * 5, "pt"),
      y = grid::unit(0.5, "npc") - grid::unit(side * from_point, "pt"), hjust = if(below) 0 else 1,
      vjust = if(below) 1 else 0, gp = grid::gpar(fontsize = size * ggplot2::.pt,
                                                  fontfamily = getOption("lactater.font", ""), ...)
    )
  }
  ggplot2::annotation_custom(
    grid::grobTree(
      line(sprintf("%.0f bpm", y), if(below) 3 else 13, 3, fontface = "bold", col = colours[["ink"]]),
      line(paste("at", lt), if(below) 13.5 else 3, 2.6, col = colours[["muted"]])
    ),
    xmin = x, xmax = x, ymin = y, ymax = y
  )
}

#' Heart rate or RPE at zone boundaries
#'
#' Interpolated between the stages, like the thresholds; a boundary below the first stage (e.g. the zone 1|2
#' split) is extrapolated from a straight line through the first three stages, where heart rate and RPE rise about
#' linearly with intensity. For internal use only.
#'
#' @param data The data retrieved from `prepare_data()`.
#' @param column `"heart_rate"` or `"rpe"`.
#' @param x The boundary intensities.
#' @param digits Decimals.
#' @param lowest The lowest value the measure can take.
#' @keywords internal
zone_value <- function(data, column, x, digits, lowest = 0) {
  value <- retrieve_at_stages(data, column, x, digits)
  stages <- data[data$intensity > 0 & !is.na(data[[column]]), ]
  if(nrow(stages) < 2) return(value)
  below <- !is.na(x) & is.na(value) & x < min(stages$intensity)
  if(any(below)) {
    first <- stages[seq_len(min(3, nrow(stages))), ]
    line <- stats::.lm.fit(cbind(1, first$intensity), first[[column]])$coefficients
    value[below] <- round(pmax(line[1] + line[2] * x[below], lowest), digits)
  }
  value
}

#' The two splits inside the moderate and the severe domain (five-zone model)
#'
#' @param split `"relative"`, `"seiler"`, or two intensities.
#' @param lt LT1 and LT2.
#' @param info The `lactater` attribute of the results.
#' @return the zone 1|2 and zone 4|5 intensities.
#' @keywords internal
domain_splits <- function(split, lt, info) {
  peak <- max(info$data$intensity)
  ## zone 1|2 at 75% of LT1 and zone 4|5 halfway to the peak: Seiler's (2010) oxygen uptake bands and Sitko's (2025)
  ## heart rates for zone 2, in intensity (see the docs)
  relative <- c(0.75 * lt[1], (lt[2] + peak) / 2)
  if(is.numeric(split)) {
    if(length(split) != 2 || anyNA(split) || split[1] <= 0 || split[1] >= lt[1] || split[2] <= lt[2])
      stop("`split` must be two intensities: one between 0 and LT1, and one above LT2.", call. = FALSE)
    return(split)
  }
  split <- match.arg(split, c("relative", "seiler"))
  if(split == "relative") return(relative)
  seiler <- c(retrieve_intensity(info$curve, 1.5), retrieve_intensity(info$curve, 6))
  inside <- c(!is.na(seiler[1]) && seiler[1] < lt[1], !is.na(seiler[2]) && seiler[2] > lt[2])
  if(!all(inside)) {
    warning(
      sprintf(
        "%s not inside its domain on this curve, so the relative split is used for it.",
        paste(c("1.5 mmol/L (zone 1|2) is", "6.0 mmol/L (zone 4|5) is")[!inside], collapse = " and ")
      ),
      call. = FALSE
    )
  }
  ifelse(inside, seiler, relative)
}

#' @rdname lactate_zones
#' @param object The result of [lactate_zones()].
#' @param heart_rate How the plot shows the heart rate of the stages, when the test has it: `"panel"` (default), in a
#'   panel under the curve on the same intensity axis, read at LT1 and LT2; `"axis"`, on the lactate plot against a
#'   second axis, as many lab reports do; or `"none"`. `options(lactater.heart_rate = "axis")` sets it for every plot
#'   and report.
#' @param ... Not used.
#' @export
autoplot.lactate_zones <- function(object, heart_rate = getOption("lactater.heart_rate", "panel"), ...) {
  heart_rate <- match.arg(heart_rate, c("panel", "axis", "none"))
  zones <- attr(object, "lactater_zones")
  info <- zones$info
  colours <- lactater_colours()
  points <- position_rest(info$data)
  curve <- curve_points(info$curve)
  limits <- range(points$intensity, curve$intensity)
  limits <- limits + c(-0.03, 0.01) * diff(limits)
  sport <- info$settings$sport
  unit <- intensity_unit(sport)
  value <- function(x) format_intensity(x, sport)
  n <- nrow(object)
  bands <- data.frame(
    from = c(limits[1], zones$bounds),
    to = c(zones$bounds, limits[2]),
    zone = object$zone,
    description = object$description,
    fill = zone_colours[[as.character(n)]]
  )
  bands$range <- c(
    paste0("< ", value(zones$bounds[1])),
    paste0(value(zones$bounds[-length(zones$bounds)]), "-", value(zones$bounds[-1])),
    paste0("> ", value(zones$bounds[length(zones$bounds)]))
  )
  ## a zone beyond the test (zone 1 below the first stage of a swimmer) is cut to the plot, or left out when none of
  ## it is in the plot, so it cannot stretch the axis
  bands$from <- pmax(bands$from, limits[1])
  bands$to <- pmin(bands$to, limits[2])
  bands <- bands[bands$to > bands$from, ]
  ## a narrow band drops the unit from its range, and a very narrow one keeps only its number, so neighboring labels
  ## do not run into each other
  width <- (bands$to - bands$from) / diff(limits)
  bands$range <- ifelse(width < 0.14, bands$range, paste(bands$range, unit))
  bands$label <- ifelse(width < 0.09, sub("Zone ", "Z", bands$zone), toupper(bands$zone))
  bands$range[width < 0.09] <- ""
  bands$description[width < 0.09] <- ""
  top <- max(points$lactate, curve$lactate) * 1.25
  marks <- data.frame(intensity = zones$lt, label = sprintf('"%s"~~%s', c("LT1", "LT2"), method_plotmath(zones$labels)))
  ## the heart rate of the exercise stages; the resting value would squeeze them into the top of the scale
  hr <- if("heart_rate" %in% names(points)) {
    points[info$data$intensity > 0 & !is.na(points$heart_rate), ]
  } else {
    points[0, ]
  }
  if(nrow(hr) < 2) heart_rate <- "none"
  y_axis <- ggplot2::waiver()
  hr_layers <- NULL
  if(heart_rate == "axis") {
    ## the heart rate scale maps onto the lower 80% of the lactate axis, under the zone labels
    lo <- max(0, floor(min(hr$heart_rate) / 20) * 20 - 40)
    hi <- ceiling(max(hr$heart_rate) / 20) * 20
    hr$y <- (hr$heart_rate - lo) / (hi - lo) * 0.8 * top
    y_axis <- ggplot2::sec_axis(~ . / (0.8 * top) * (hi - lo) + lo, name = "Heart rate (bpm)",
                                breaks = seq(lo + 20, hi, by = 20))
    hr_layers <- list(
      ggplot2::geom_line(ggplot2::aes(intensity, y), hr, colour = colours[["ink"]], linewidth = 0.5),
      ggplot2::geom_point(ggplot2::aes(intensity, y), hr, shape = 22, size = 1.6, stroke = 0.6,
                          fill = colours[["paper"]], colour = colours[["ink"]]),
      ggplot2::annotate("text", x = hr$intensity[1], y = hr$y[1], label = "Heart rate", hjust = 0.2, vjust = -1.2,
                        size = 2.9, colour = colours[["ink"]])
    )
  }
  title <- sprintf("%d training zones", n)
  subtitle <- "Zones at the intensity domains: moderate below LT1, heavy from LT1 to LT2, severe above LT2"

  ## the zone, its description and its range, set down from the top of the panel in points rather than in units of
  ## the axis, so the three lines keep apart at any height of the plot
  zone_label <- function(k) {
    line <- function(label, from_top, size, ...) {
      grid::textGrob(label, y = grid::unit(1, "npc") - grid::unit(from_top, "pt"), vjust = 1, gp = grid::gpar(
        fontsize = size * ggplot2::.pt, fontfamily = getOption("lactater.font", ""), ...
      ))
    }
    ggplot2::annotation_custom(
      grid::grobTree(
        line(bands$label[k], 5, 3.2, fontface = "bold", col = colours[["ink"]]),
        ## an invisible bar on each side gives every description the same height, so "recovery" (no tall letter)
        ## sits on the same line as "endurance"
        line(parse(text = sprintf('phantom("|")*%s*phantom("|")', method_plotmath(bands$description[k]))), 16, 2.8,
             col = colours[["muted"]]),
        line(bands$range[k], 26.5, 2.8, col = colours[["muted"]])
      ),
      xmin = bands$from[k], xmax = bands$to[k], ymin = -Inf, ymax = Inf
    )
  }

  lactate <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect", xmin = bands$from, xmax = bands$to, ymin = 0, ymax = top, fill = bands$fill, alpha = 0.2
    ) +
    lapply(seq_len(nrow(bands)), zone_label) +
    ggplot2::geom_segment(
      ggplot2::aes(x = intensity, xend = intensity, y = 0, yend = top * 0.8), marks,
      colour = colours[["ink"]], linewidth = 0.4, linetype = "22"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = intensity, y = 0, label = label), marks, parse = TRUE,
      angle = 90, hjust = -0.1, vjust = -0.6, size = 2.9, colour = colours[["muted"]]
    ) +
    hr_layers +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9) +
    stage_layers(points, info$data, colours) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, top), breaks = lactate_breaks(top), expand = c(0, 0), sec.axis = y_axis) +
    ggplot2::labs(title = title, subtitle = subtitle, x = intensity_label(sport), y = "Blood lactate (mmol/L)") +
    theme_lactater()
  if(heart_rate != "panel") return(lactate)

  ## the heart rate under the curve, on the same intensity axis
  at <- data.frame(
    intensity = zones$lt, heart_rate = zone_value(info$data, "heart_rate", zones$lt, 0), lt = c("LT1", "LT2")
  )
  at <- at[!is.na(at$heart_rate), ]
  ## at least 5 bpm of room under the lowest stage, so its dot is not cut by the axis
  hr_limits <- c(floor((min(hr$heart_rate) - 5) / 20) * 20, ceiling(max(hr$heart_rate) / 20) * 20 + 10)
  hr_panel <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect", xmin = bands$from, xmax = bands$to, ymin = hr_limits[1], ymax = hr_limits[2], fill = bands$fill,
      alpha = 0.2
    ) +
    ggplot2::annotate(
      "segment", x = zones$lt, xend = zones$lt, y = hr_limits[1], yend = hr_limits[2], colour = colours[["ink"]],
      linewidth = 0.4, linetype = "22"
    ) +
    ggplot2::geom_line(ggplot2::aes(intensity, heart_rate), hr, colour = colours[["ink"]], linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(intensity, heart_rate), hr, shape = 21, size = 1.8, stroke = 0.6,
                        fill = colours[["paper"]], colour = colours[["ink"]]) +
    ## the heart rate at LT1 and LT2 on a filled point where the line crosses the threshold
    ggplot2::annotate(
      "point", x = at$intensity, y = at$heart_rate, shape = 21, size = 2.4, stroke = 0.9, fill = colours[["ink"]],
      colour = colours[["paper"]]
    ) +
    lapply(seq_len(nrow(at)), function(k) {
      hr_point_label(at$intensity[k], at$heart_rate[k], at$lt[k], limits, hr_limits, colours)
    }) +
    ggplot2::scale_y_continuous(limits = hr_limits, n.breaks = 4, expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = limits, expand = FALSE) +
    ggplot2::labs(x = intensity_label(sport), y = "Heart rate\n(bpm)") +
    theme_lactater()
  lactate <- lactate +
    ggplot2::labs(title = NULL, subtitle = NULL, x = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank())
  patchwork::wrap_plots(lactate, hr_panel, ncol = 1, heights = c(3, 1.6)) +
    patchwork::plot_annotation(
      title = title, subtitle = paste0(subtitle, ". Below: the heart rate of the stages, read at LT1 and LT2"),
      theme = theme_lactater()
    )
}
