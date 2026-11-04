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
#'   * `"equal"` (default): zone 1|2 at half of LT1, and zone 4|5 halfway between LT2 and the highest intensity of
#'     the test.
#'   * `"seiler"`: where the fitted curve reaches 1.5 mmol/L (zone 1|2) and 6.0 mmol/L (zone 4|5), the blood lactate
#'     limits of the five-zone scale in Seiler (2010). A cut that falls outside its domain, or is not reached, uses
#'     the equal split instead, with a warning.
#'   * Two intensities, `c(zone 1|2, zone 4|5)`, for your own cuts.
#'
#' @details
#' LT1 is usually a "first rise" threshold (e.g. `"LTP1"`, `"Log-log"`, `"Bsln + 0.5"`), and LT2 a threshold meant to
#' estimate the maximal lactate steady state (MLSS; e.g. `"LTP2"`, `"ModDmax"`, `"Log-Poly-ModDmax"`, `"OBLA 4.0"`,
#' `"IAT"`). Against a measured MLSS these landed from about 40 W below to 40 W above it on average, by method,
#' protocol and athletes (Hauser et al., 2014; Ploszczyca et al., 2020), and whether the MLSS or the critical power
#' marks the boundary between the heavy and severe domains is debated (Jones et al., 2019). Different choices give
#' different zones, so report which ones you used. No published five-zone scale is built on individual thresholds
#' alone: its lactate and heart-rate limits are averages, so treat the splits inside a domain as a convention and check
#' them against how the athlete responds.
#'
#' @param x The result of [lactate_threshold()].
#' @param lt1,lt2 The thresholds that mark LT1 and LT2: a method in `x$method`, or an intensity. Default to `"LTP1"`
#' and `"LTP2"`, the turn points that define the zones in Hofmann & Tschakert (2017).
#' @param model The number of zones: `3` (default) or `5`.
#' @param split How to split the moderate and severe domains in the five-zone model: `"equal"` (default),
#' `"seiler"`, or two intensities. Ignored with three zones.
#'
#' @return a `lactate_zones` tibble with one row per zone: `zone`, `domain`, `description`, `intensity_from`,
#' `intensity_to`, the matching `pace` range (running, swimming, rowing), the `lactate` range on the fitted curve, and
#' the `heart_rate` and `rpe` ranges (when available; a boundary below the first stage is extrapolated from the first
#' three stages). The first zone has no lower bound and the last no upper bound.
#' Plot it with [autoplot()].
#'
#' @references
#' Hofmann P, Tschakert G. Intensity- and duration-based options to regulate endurance training. Frontiers in
#' Physiology. 2017;8:337.
#'
#' Hauser T, Adam J, Schulz H. Comparison of selected lactate threshold parameters with maximal lactate steady state in
#' cycling. International Journal of Sports Medicine. 2014;35(6):517-521.
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
#' Tonnessen E, Sylta O, Haugen TA, Hem E, Svendsen IS, Seiler S. The road to gold: training and peaking
#' characteristics in the year prior to a gold medal endurance performance. PLOS ONE. 2014;9(7):e101796.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
#' lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5)
#' lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = "seiler")
lactate_zones <- function(x, lt1 = "LTP1", lt2 = "LTP2", model = 3, split = "equal") {
  info <- lactater_info(x)
  if(!identical(as.numeric(model), 3) && !identical(as.numeric(model), 5))
    stop("`model` must be 3 or 5.", call. = FALSE)
  boundary <- function(marker, arg) {
    if(is.numeric(marker) && length(marker) == 1 && !is.na(marker)) return(marker)
    i <- match(marker, as.character(x$method))
    if(length(marker) != 1 || is.na(i))
      stop(sprintf("`%s` must be a number or one of: %s.", arg, paste(x$method, collapse = ", ")), call. = FALSE)
    if(is.na(info$intensity[[marker]]))
      stop(sprintf("`%s` (%s) was not found in this test.", arg, marker), call. = FALSE)
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
  out$lactate_from <- retrieve_lactate(info$curve, from)
  out$lactate_to <- retrieve_lactate(info$curve, to)
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
  below <- !is.na(x) & is.na(value) & nrow(stages) >= 2 & x < min(stages$intensity)
  if(any(below)) {
    first <- stages[seq_len(min(3, nrow(stages))), ]
    line <- stats::.lm.fit(cbind(1, first$intensity), first[[column]])$coefficients
    value[below] <- round(pmax(line[1] + line[2] * x[below], lowest), digits)
  }
  value
}

#' The two splits inside the moderate and the severe domain (five-zone model)
#'
#' @param split `"equal"`, `"seiler"`, or two intensities.
#' @param lt LT1 and LT2.
#' @param info The `lactater` attribute of the results.
#' @return the zone 1|2 and zone 4|5 intensities.
#' @keywords internal
domain_splits <- function(split, lt, info) {
  peak <- max(info$data$intensity)
  equal <- c(lt[1] / 2, (lt[2] + peak) / 2)
  if(is.numeric(split)) {
    if(length(split) != 2 || anyNA(split) || split[1] <= 0 || split[1] >= lt[1] || split[2] <= lt[2])
      stop("`split` must be two intensities: one between 0 and LT1, and one above LT2.", call. = FALSE)
    return(split)
  }
  split <- match.arg(split, c("equal", "seiler"))
  if(split == "equal") return(equal)
  seiler <- c(retrieve_intensity(info$curve, 1.5), retrieve_intensity(info$curve, 6))
  inside <- c(!is.na(seiler[1]) && seiler[1] < lt[1], !is.na(seiler[2]) && seiler[2] > lt[2])
  if(!all(inside)) {
    warning(
      sprintf(
        "%s not inside its domain on this curve, so the equal split is used for it.",
        paste(c("1.5 mmol/L (zone 1|2) is", "6.0 mmol/L (zone 4|5) is")[!inside], collapse = " and ")
      ),
      call. = FALSE
    )
  }
  ifelse(inside, seiler, equal)
}

#' @rdname lactate_zones
#' @param object The result of [lactate_zones()].
#' @param ... Not used.
#' @export
autoplot.lactate_zones <- function(object, ...) {
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
  bands$middle <- (bands$from + bands$to) / 2
  ## a narrow band drops the unit from its range, so neighboring labels do not run into each other
  narrow <- (bands$to - bands$from) / diff(limits) < 0.14
  bands$range <- ifelse(narrow, bands$range, paste(bands$range, unit))
  top <- max(points$lactate, curve$lactate) * 1.25
  marks <- data.frame(intensity = zones$lt, label = sprintf('"%s"~~%s', c("LT1", "LT2"), method_plotmath(zones$labels)))

  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect", xmin = bands$from, xmax = bands$to, ymin = 0, ymax = top, fill = bands$fill, alpha = 0.2
    ) +
    ggplot2::annotate(
      "text", x = bands$middle, y = top * 0.965, label = toupper(bands$zone), vjust = 1, size = 3.2, fontface = "bold",
      colour = colours[["ink"]]
    ) +
    ggplot2::annotate(
      "text", x = bands$middle, y = top * 0.915, label = method_plotmath(bands$description), parse = TRUE, vjust = 1,
      size = 2.8, colour = colours[["muted"]]
    ) +
    ggplot2::annotate(
      "text", x = bands$middle, y = top * 0.865, label = bands$range, vjust = 1, size = 2.8,
      colour = colours[["muted"]]
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = intensity, xend = intensity, y = 0, yend = top * 0.8), marks,
      colour = colours[["ink"]], linewidth = 0.4, linetype = "22"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = intensity, y = 0, label = label), marks, parse = TRUE,
      angle = 90, hjust = -0.1, vjust = -0.6, size = 2.9, colour = colours[["muted"]]
    ) +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve, colour = colours[["curve"]], linewidth = 0.9) +
    stage_layers(points, info$data, colours) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, top), breaks = lactate_breaks(top), expand = c(0, 0)) +
    ggplot2::labs(
      title = sprintf("%d training zones", n),
      subtitle = "Zones at the intensity domains: moderate below LT1, heavy from LT1 to LT2, severe above LT2",
      x = intensity_label(sport),
      y = "Blood lactate (mmol/L)"
    ) +
    theme_lactater()
}
