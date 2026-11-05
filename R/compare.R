## test-retest coefficients of variation (%) of the threshold intensity, where a study reports them:
## Heuberger et al. (2018): 48 well-trained male cyclists, 5-min steps, five tests about two weeks apart;
## Pallares et al. (2016): 14 trained male cyclists, 25 W/min ramp, two tests 2-5 days apart
method_cv <- c(
  "IAT" = 3.6, "OBLA 4.0" = 3.8, "Dmax" = 3.4, "ModDmax" = 4.3,
  "Bsln + 0.5" = 3.7, "Bsln + 1.0" = 3.4, "Bsln + 1.5" = 3.4
)

## the coefficient of variation of each method: the user's `cv` (one number or a named vector) over the defaults
method_cvs <- function(cv, methods) {
  if(!is.null(cv) && (!is.numeric(cv) || anyNA(cv) || any(cv <= 0) || (length(cv) > 1 && is.null(names(cv)))))
    stop("`cv` must be one positive number or a named vector of them.", call. = FALSE)
  if(length(cv) == 1 && is.null(names(cv))) {
    stats::setNames(rep(cv, length(methods)), methods)
  } else {
    replace(method_cv, names(cv), cv)
  }
}

#' Compare two lactate tests
#'
#' Compares two tests of the same athlete (e.g. before and after a training block): the change of each
#' threshold, whether it is larger than the noise of the method, the heart rate at each threshold, and
#' notes on what could make the comparison unfair.
#'
#' A threshold moves between two tests even without any change in fitness, by the day-to-day variation of
#' the athlete and the error of the analyser and the method. Each change is therefore given with an
#' interval, change +/- z * sqrt(2) * typical error, where the typical error is the coefficient of
#' variation (`cv`) times the intensity before (Hopkins, 2000; Swinton et al., 2018). The change is called
#' an `"increase"` or a `"decrease"` when the whole interval lies beyond the smallest worthwhile change
#' (`swc`), and `"unclear"` otherwise.
#'
#' The default `cv` comes from test-retest studies of trained male cyclists: 3.6% for IAT, 3.8% for OBLA 4.0,
#' 3.4% for Dmax and 4.3% for ModDmax (Heuberger et al., 2018), and 3.4-3.7% for Bsln+ (Pallares et al.,
#' 2016, ramp test). Other methods have no published value, so their `verdict` is `NA` unless you give one.
#' The best `cv` is the one of your own athletes, measured with your protocol and analyser.
#'
#' @param before,after The results of [lactate_threshold()] for the two tests, with the same sport.
#' @param cv The test-retest coefficient of variation (%) of each method: one number for all methods, or a
#' named vector (e.g. `c("LTP2" = 3)`) that adds to or replaces the defaults.
#' @param swc The smallest worthwhile change, in % of the intensity before. Default to `0`.
#' @param level The probability of the interval. Default to `0.9`.
#'
#' @return a `lactate_comparison` tibble with one row per method found in both tests: `method`, `before`,
#' `after`, `change`, `change_percent`, the interval of the change (`change_low`, `change_high`), the `verdict`,
#' and the heart rate at each threshold in both tests (when recorded). The `notes` attribute lists what differs
#' between the two tests (stages, step, resting sample, sample type) and a fall in peak lactate. Plot both
#' curves with [autoplot()].
#'
#' @references
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and
#' predictive value of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#'
#' Hopkins WG. Measures of reliability in sports medicine and science. Sports Medicine. 2000;30(1):1-15.
#'
#' Pallares JG, Moran-Navarro R, Ortega JF, Fernandez-Elias VE, Mora-Rodriguez R. Validity and reliability of
#' ventilatory and blood lactate thresholds in well-trained cyclists. PLOS ONE. 2016;11(9):e0163389.
#'
#' Swinton PA, Hemingway BS, Saunders B, Gualano B, Dolan E. A statistical framework to interpret individual
#' response to intervention: paving the way for personalized nutrition and exercise prescription. Frontiers
#' in Nutrition. 2018;5:41.
#'
#' @export
#'
#' @examples
#' series <- subset(lactate_tests, athlete == "cyclist_series")
#' before <- lactate_threshold(subset(series, day == 0), intensity, lactate, heart_rate, plot = FALSE)
#' after <- lactate_threshold(subset(series, day == 58), intensity, lactate, heart_rate, plot = FALSE)
#' compare_tests(before, after)
compare_tests <- function(before, after, cv = NULL, swc = 0, level = 0.9) {
  info_before <- lactater_info(before)
  info_after <- lactater_info(after)
  if(info_before$settings$sport != info_after$settings$sport)
    stop("Both tests must be from the same sport.", call. = FALSE)
  cvs <- method_cvs(cv, intersect(as.character(before$method), as.character(after$method)))

  methods <- intersect(as.character(before$method), as.character(after$method))
  if(length(methods) == 0) stop("The two tests have no method in common.", call. = FALSE)
  ## unrounded intensities, so the change is not driven by the rounding of the table
  value_before <- unname(info_before$intensity[methods])
  value_after <- unname(info_after$intensity[methods])
  digits <- intensity_digits(info_before$settings$sport)

  typical_error <- unname(cvs[methods]) / 100 * value_before
  half_width <- stats::qnorm(1 - (1 - level) / 2) * sqrt(2) * typical_error
  ## not named `change`: tibble() would read the rounded column of that name in the lines below
  difference <- value_after - value_before
  worthwhile <- swc / 100 * value_before
  verdict <- ifelse(difference - half_width > worthwhile, "increase",
                    ifelse(difference + half_width < -worthwhile, "decrease", "unclear"))

  out <- tibble::tibble(
    method = factor(methods, levels = methods),
    before = round(value_before, digits),
    after = round(value_after, digits),
    change = round(difference, digits),
    change_percent = round(100 * difference / value_before, 1),
    change_low = round(difference - half_width, digits),
    change_high = round(difference + half_width, digits),
    verdict = verdict
  )
  if("heart_rate" %in% names(before) && "heart_rate" %in% names(after)) {
    out$heart_rate_before <- before$heart_rate[match(methods, before$method)]
    out$heart_rate_after <- after$heart_rate[match(methods, after$method)]
  }
  if("rpe" %in% names(before) && "rpe" %in% names(after)) {
    out$rpe_before <- before$rpe[match(methods, before$method)]
    out$rpe_after <- after$rpe[match(methods, after$method)]
  }
  attr(out, "lactater_comparison") <- list(before = info_before, after = info_after)
  attr(out, "notes") <- comparison_notes(info_before, info_after)
  class(out) <- c("lactate_comparison", class(out))
  out
}

#' What could make two tests hard to compare
#'
#' @param before,after The `lactater` attributes of the two tests.
#' @return a character vector of notes (empty when nothing stands out).
#' @keywords internal
comparison_notes <- function(before, after) {
  describe <- function(info) {
    stages <- info$data$intensity[info$data$intensity > 0]
    list(
      stages = length(stages), first = min(stages), step = stats::median(diff(stages)),
      rest = any(info$data$intensity == 0), sample = info$settings$sample, peak = max(info$data$lactate)
    )
  }
  b <- describe(before)
  a <- describe(after)
  unit <- intensity_unit(before$settings$sport)
  c(
    if(b$first != a$first || b$step != a$step) {
      sprintf("The protocol differs (first stage %s vs %s %s, step %s vs %s %s): thresholds depend on the protocol.",
              format(b$first), format(a$first), unit, format(b$step), format(a$step), unit)
    },
    if(b$rest != a$rest) "Only one test has a resting sample, which Bsln+ uses as its baseline.",
    if(b$sample != a$sample) sprintf("The sample type differs (%s vs %s).", b$sample, a$sample),
    ## shortcut: a 10% fall is a judgement call, not a published cut-off
    if(a$peak < 0.9 * b$peak) {
      sprintf(
        paste(
          "Peak lactate fell from %.1f to %.1f mmol/L. A lower peak also moves the curve to the right, as in",
          "overreaching or low carbohydrate stores, not only with better fitness."
        ),
        b$peak, a$peak
      )
    }
  )
}

#' Shift of the whole lactate curve between two tests
#'
#' How far the fitted lactate curve moved between two tests: the intensity gained at each lactate value, and,
#' when `intensity` is given, the lactate saved at each intensity. A rightward shift (more intensity for the
#' same lactate) is the classic sign of better endurance; check the peak lactate too (see [compare_tests()]).
#'
#' @inheritParams compare_tests
#' @param lactate Lactate values (mmol/L) at which to compare the intensity. Values outside either curve are
#' `NA`. Default to `seq(1.5, 4, by = 0.5)`.
#' @param intensity Intensities at which to compare the lactate, or `NULL`.
#'
#' @return a tibble with `lactate`, `intensity_before`, `intensity_after` and `shift`; with `intensity`,
#' rows with `intensity`, `lactate_before`, `lactate_after` and `lactate_change` are added.
#' @export
#'
#' @examples
#' series <- subset(lactate_tests, athlete == "cyclist_series")
#' before <- lactate_threshold(subset(series, day == 0), intensity, lactate, plot = FALSE)
#' after <- lactate_threshold(subset(series, day == 58), intensity, lactate, plot = FALSE)
#' curve_shift(before, after)
curve_shift <- function(before, after, lactate = seq(1.5, 4, by = 0.5), intensity = NULL) {
  curve_before <- lactater_info(before)$curve
  curve_after <- lactater_info(after)$curve
  digits <- intensity_digits(lactater_info(before)$settings$sport)
  at_before <- vapply(lactate, retrieve_intensity, numeric(1), curve = curve_before)
  at_after <- vapply(lactate, retrieve_intensity, numeric(1), curve = curve_after)
  out <- tibble::tibble(
    lactate = lactate,
    intensity_before = round(at_before, digits),
    intensity_after = round(at_after, digits),
    shift = round(at_after - at_before, digits)
  )
  if(!is.null(intensity)) {
    inside <- function(curve, x) ifelse(x >= curve$domain[1] & x <= curve$domain[2], curve$predict(x), NA_real_)
    lactate_before <- inside(curve_before, intensity)
    lactate_after <- inside(curve_after, intensity)
    out <- vctrs::vec_rbind(out, tibble::tibble(
      intensity = intensity,
      lactate_before = round(lactate_before, 1),
      lactate_after = round(lactate_after, 1),
      lactate_change = round(lactate_after - lactate_before, 1)
    ))
  }
  out
}

#' Thresholds relative to body mass, peak intensity and peak heart rate
#'
#' Expresses each threshold so that different athletes can be compared: per kg of body mass (power only), as a
#' percentage of the highest intensity of the test, and the heart rate as a percentage of the highest heart rate
#' of the test. A fixed lactate concentration falls at a similar percentage of peak power in trained and
#' untrained people (Maassen & Busse, 1989), so percentages of peak do not rank fitness by themselves.
#'
#' @param x The result of [lactate_threshold()].
#' @param body_mass Body mass (kg), or `NULL`.
#'
#' @return `x` with `intensity_per_kg` (with `body_mass`, cycling and rowing), `percent_peak` and, with heart
#' rate, `percent_peak_heart_rate`.
#'
#' @references
#' Maassen N, Busse MW. The relationship between lactic acid and work load: a measure for endurance capacity
#' or an indicator of carbohydrate deficiency? European Journal of Applied Physiology and Occupational
#' Physiology. 1989;58(7):728-737.
#'
#' @export
#'
#' @examples
#' elite <- subset(lactate_tests, test == "cycling_elite")
#' results <- lactate_threshold(elite, intensity, lactate, heart_rate, plot = FALSE)
#' relative_thresholds(results, body_mass = elite$body_mass[1])
relative_thresholds <- function(x, body_mass = NULL) {
  info <- lactater_info(x)
  if(!is.null(body_mass) && (!is.numeric(body_mass) || length(body_mass) != 1 || is.na(body_mass) || body_mass <= 0))
    stop("`body_mass` must be a single positive number (kg).", call. = FALSE)
  intensity <- unname(info$intensity[as.character(x$method)])
  if(!is.null(body_mass) && intensity_unit(info$settings$sport) == "W") {
    x$intensity_per_kg <- round(intensity / body_mass, 2)
  }
  x$percent_peak <- round(100 * intensity / max(info$data$intensity), 1)
  if("heart_rate" %in% names(x) && !all(is.na(info$data$heart_rate))) {
    x$percent_peak_heart_rate <- round(100 * x$heart_rate / max(info$data$heart_rate, na.rm = TRUE), 1)
  }
  x
}

#' Thresholds over a series of tests
#'
#' Fits a straight line through each threshold over three or more tests of the same athlete, to show the trend
#' with its uncertainty instead of reading noise from one test to the next (Hopkins, 2017).
#'
#' @param ... The results of [lactate_threshold()], in any order. A list of results can be spliced in with `!!!`.
#' @param day The date of each test, in the same order as `...`: dates, or days since the first test.
#'
#' @return a `lactate_trend` tibble with one row per method found in every test: `method`, `tests`, the
#' change per 30 days (`per_30_days`) with its 90% interval (`low`, `high`), and the first and last fitted
#' values. Plot it with [autoplot()]: up to five tests as their lactate curves and training zones, more as each
#' threshold over time.
#'
#' @references
#' Hopkins WG. Measures of reliability in sports medicine and science. Sports Medicine. 2000;30(1):1-15.
#'
#' Hopkins WG. A spreadsheet for monitoring an individual's changes and trend. Sportscience. 2017;21:5-9.
#'
#' @export
#'
#' @examples
#' series <- subset(lactate_tests, athlete == "cyclist_series")
#' days <- c(0, 163, 1107, 1865, 2046)
#' tests <- lapply(days, function(d) {
#'   lactate_threshold(subset(series, day == d), intensity, lactate, plot = FALSE)
#' })
#' ## the test dates are not in the data: an example start date
#' trend <- lactate_trend(!!!tests, day = as.Date("2019-01-14") + days)
#' trend
#' autoplot(trend, lt1 = "Bsln + 0.5", lt2 = "ModDmax")
#' autoplot(trend, view = "thresholds")
lactate_trend <- function(..., day) {
  tests <- rlang::list2(...)
  infos <- lapply(tests, lactater_info)
  if(length(tests) < 3) stop("A trend needs at least three tests.", call. = FALSE)
  if(inherits(day, "POSIXt")) day <- as.Date(day)
  labels <- date_labels(day)
  if(inherits(day, "Date")) day <- as.numeric(day - min(day))
  if(!is.numeric(day) || length(day) != length(tests) || anyNA(day) || anyDuplicated(day))
    stop("`day` must give one distinct day per test.", call. = FALSE)
  if(length(unique(vapply(infos, function(i) i$settings$sport, character(1)))) > 1)
    stop("All tests must be from the same sport.", call. = FALSE)

  methods <- Reduce(intersect, lapply(tests, function(t) as.character(t$method)))
  values <- vapply(infos, function(i) unname(i$intensity[methods]), numeric(length(methods)))
  values <- matrix(values, nrow = length(methods))
  digits <- intensity_digits(infos[[1]]$settings$sport)
  rows <- lapply(seq_along(methods), function(k) {
    found <- !is.na(values[k, ])
    if(sum(found) < 3) return(NULL)
    fit <- stats::lm(intensity ~ day, data.frame(intensity = values[k, found], day = day[found]))
    slope <- stats::coef(fit)[[2]] * 30
    interval <- stats::confint(fit, level = 0.9)[2, ] * 30
    fitted_ends <- stats::predict(fit, data.frame(day = range(day[found])))
    data.frame(
      method = methods[k], tests = sum(found), per_30_days = round(slope, digits + 1),
      low = round(interval[[1]], digits + 1), high = round(interval[[2]], digits + 1),
      first = round(fitted_ends[[1]], digits), last = round(fitted_ends[[2]], digits)
    )
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  out$method <- factor(out$method, levels = methods)
  attr(out, "lactater_trend") <- list(
    tests = tests, labels = labels, values = values, methods = methods, day = day, sport = infos[[1]]$settings$sport,
    intervals = lapply(infos, `[[`, "intervals"), curves = lapply(infos, `[[`, "curve"),
    data = lapply(infos, `[[`, "data")
  )
  class(out) <- c("lactate_trend", class(out))
  out
}

#' @rdname compare_tests
#' @param object The result of [compare_tests()].
#' @param ... Not used.
#' @export
autoplot.lactate_comparison <- function(object, ...) {
  tests <- attr(object, "lactater_comparison")
  labelled <- function(.data, label) transform(.data, test = factor(label, levels = c("Before", "After")))
  curves <- rbind(
    labelled(curve_points(tests$before$curve), "Before"),
    labelled(curve_points(tests$after$curve), "After")
  )
  points <- rbind(
    labelled(position_rest(tests$before$data)[c("intensity", "lactate")], "Before"),
    labelled(position_rest(tests$after$data)[c("intensity", "lactate")], "After")
  )
  colours <- lactater_colours()
  test_colours <- c("Before" = colours[["faint"]], "After" = colours[["curve"]])
  ## the test names sit at the end of each curve instead of a legend: before on its left, after on its right
  ends <- do.call(rbind, lapply(split(curves, curves$test), function(d) d[nrow(d), ]))
  ends$hjust <- ifelse(ends$test == "Before", 1.25, -0.25)
  ## the shift of the curve at 2 and 4 mmol/L, as arrows from the curve before to the curve after
  shift <- curve_shift(list_to_thresholds(tests$before), list_to_thresholds(tests$after), lactate = c(2, 4))
  shift <- shift[!is.na(shift$shift), ]
  unit <- intensity_unit(tests$before$settings$sport)

  ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate, colour = test), curves, linewidth = 0.9) +
    ggplot2::geom_segment(
      ggplot2::aes(x = intensity_before, xend = intensity_after, y = lactate, yend = lactate), shift,
      colour = colours[["accent"]], linewidth = 0.6,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.07, "inch"), type = "closed")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = intensity_after, y = lactate, label = sprintf("%+g %s", shift, unit)), shift,
      hjust = -0.2, vjust = 1.6, size = 3.1, colour = colours[["accent"]], fontface = "bold"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(intensity, lactate, colour = test), points,
      shape = 21, size = 2.4, stroke = 0.8, fill = colours[["paper"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(intensity, lactate, label = test, colour = test, hjust = hjust), ends,
      size = 3.3, fontface = "bold"
    ) +
    ggplot2::scale_colour_manual(values = test_colours, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA), breaks = lactate_breaks(max(points$lactate, curves$lactate) * 1.05),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title = "Lactate curves compared",
      subtitle = "Arrows: the intensity gained at 2 and 4 mmol/L",
      x = intensity_label(tests$before$settings$sport),
      y = "Blood lactate (mmol/L)"
    ) +
    theme_lactater()
}

#' A minimal result object from a stored `lactater` attribute, for helpers that take results
#' @param info A `lactater` attribute.
#' @keywords internal
list_to_thresholds <- function(info) {
  out <- tibble::tibble(method = character(0))
  attr(out, "lactater") <- info
  out
}
#' @rdname lactate_trend
#' @param object The result of [lactate_trend()].
#' @param method The methods to plot in the `"thresholds"` view. Default to ModDmax, IAT, OBLA 4.0 and Dmax, which have
#' a published test-retest variation (see [compare_tests()]), when present.
#' @param view `"curves"`, the default for up to five tests: the lactate curves of all tests, light to dark by date,
#' to see where along the intensity range the curve moved, above the five training zones of each test from `lt1` and
#' `lt2`. `"thresholds"`, the default for more tests: one panel per method, each test as a dot (with its confidence
#' interval when computed) and a gray band for the noise of the method around the first test, so that dots outside
#' the band are changes larger than the noise (as in monitoring charts; Hopkins, 2017).
#' @param lt1,lt2 The thresholds that mark LT1 and LT2 in the `"curves"` view, as in [lactate_zones()].
#' @param cv The test-retest coefficient of variation (%) for the noise band, as in [compare_tests()].
#' @export
autoplot.lactate_trend <- function(object, method = NULL, view = NULL, cv = NULL, lt1 = "LTP1", lt2 = "LTP2", ...) {
  trend <- attr(object, "lactater_trend")
  if(is.null(view)) view <- if(length(trend$day) <= 5) "curves" else "thresholds"
  view <- match.arg(view, c("curves", "thresholds"))
  if(view == "curves") {
    order <- order(trend$day)
    return(plot_series(trend$tests[order], trend$labels[order], lt1, lt2))
  }
  colours <- lactater_colours()
  cvs <- method_cvs(cv, trend$methods)
  if(is.null(method)) {
    with_cv <- intersect(c("ModDmax", "IAT", "OBLA 4.0", "Dmax", names(cvs)), levels(object$method))
    method <- utils_head(if(length(with_cv) > 0) with_cv else levels(object$method), 4)
  }
  keep <- match(method, trend$methods)
  points <- do.call(rbind, lapply(seq_along(trend$day), function(k) {
    interval <- trend$intervals[[k]]
    data.frame(
      method = factor(method, levels = method), day = trend$day[k], intensity = trend$values[keep, k],
      low = if(!is.null(interval)) unname(interval[method, "low"]) else NA_real_,
      high = if(!is.null(interval)) unname(interval[method, "high"]) else NA_real_
    )
  }))
  points <- points[!is.na(points$intensity), ]
  ## noise band: the first test +/- 1.645 * sqrt(2) * typical error, where 90% of test-to-test changes fall when
  ## nothing changed (Hopkins, 2000)
  first <- points[points$day == min(trend$day), ]
  first$half <- 1.645 * sqrt(2) * unname(cvs[as.character(first$method)]) / 100 * first$intensity
  points$outside <- abs(points$intensity - first$intensity[match(points$method, first$method)]) >
    first$half[match(points$method, first$method)]
  points$outside[is.na(points$outside)] <- FALSE
  first <- first[!is.na(first$half), ]

  ggplot2::ggplot(points, ggplot2::aes(day, intensity)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = intensity - half, ymax = intensity + half), first,
      inherit.aes = FALSE, fill = colours[["faint"]], alpha = 0.35
    ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = intensity), first, colour = colours[["muted"]], linetype = "22",
                        linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = low, ymax = high), colour = colours[["muted"]], alpha = 0.6,
                            linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(colour = colours[["faint"]], linewidth = 0.5) +
    ggplot2::geom_point(
      ggplot2::aes(fill = outside), shape = 21, size = 2.8, stroke = 0.7, colour = colours[["paper"]]
    ) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = colours[["faint"]], `TRUE` = colours[["curve"]]), guide = "none") +
    ## few day labels, so they stay apart in narrow panels
    ggplot2::scale_x_continuous(n.breaks = 3) +
    ggplot2::facet_wrap(
      ggplot2::vars(method), nrow = 1, scales = "free_y",
      labeller = ggplot2::as_labeller(function(x) sprintf("bold(%s)", method_plotmath(x)), ggplot2::label_parsed)
    ) +
    ggplot2::labs(
      title = "Did the thresholds change?",
      subtitle = paste(
        "Each test as a dot; gray band: the noise of the method around the first test, where 90% of changes fall",
        "when nothing changed. Dark dots lie outside it"
      ),
      x = "Day", y = intensity_label(trend$sport)
    ) +
    theme_lactater()
}

#' The lactate curves and training zones of a few tests
#'
#' `plot_series()` draws every lactate curve, light to dark by date, then the heart rate of the stages in the same
#' colors (when the tests have it), above the five zones of each test as one bar, on one intensity axis.
#' `series_zones()` gives the five zones of each test, and names the test when a threshold is missing.
#' `date_labels()` names tests by date ("3 Nov 2026") or by day ("Day 58"). For internal use only.
#'
#' @param tests Results of [lactate_threshold()], oldest first.
#' @param labels The name of each test, from `date_labels()`.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, as in [lactate_zones()].
#' @param day Dates, or days since the first test.
#' @keywords internal
plot_series <- function(tests, labels, lt1, lt2) {
  n <- length(tests)
  if(n > 5) stop("The curves of more than five tests do not fit on one plot: choose five.", call. = FALSE)
  colours <- lactater_colours()
  infos <- lapply(tests, lactater_info)
  sport <- infos[[1]]$settings$sport
  zones <- series_zones(tests, labels, lt1, lt2)
  named <- function(k, d) cbind(d, test = factor(labels[k], levels = labels))
  curves <- do.call(rbind, lapply(seq_len(n), function(k) named(k, curve_points(infos[[k]]$curve))))
  stages <- do.call(rbind, lapply(seq_len(n), function(k) {
    named(k, infos[[k]]$data[infos[[k]]$data$intensity > 0, c("intensity", "lactate")])
  }))
  span <- diff(range(stages$intensity, curves$intensity))
  xlim <- c(min(stages$intensity, curves$intensity) - 0.02 * span, max(curves$intensity) + 0.2 * span)

  ## the name of each test at the end of its curve, pushed up until it clears the names already placed near it
  ends <- do.call(rbind, lapply(split(curves, curves$test), function(d) d[nrow(d), ]))
  gap <- 0.07 * max(curves$lactate)
  placed <- integer()
  for(i in order(ends$lactate)) {
    repeat {
      near <- placed[abs(ends$intensity[placed] - ends$intensity[i]) < 0.18 * span &
                       abs(ends$lactate[placed] - ends$lactate[i]) < gap]
      if(!length(near)) break
      ## a little more than the gap, so that rounding cannot find the same name near again
      ends$lactate[i] <- max(ends$lactate[near]) + gap * 1.001
    }
    placed <- c(placed, i)
  }
  top <- max(stages$lactate, curves$lactate, ends$lactate) * 1.05

  shades <- grDevices::colorRampPalette(c(colours[["faint"]], colours[["curve"]]), space = "Lab")(n)
  lactate <- ggplot2::ggplot(curves, ggplot2::aes(intensity, lactate, colour = test)) +
    ggplot2::geom_hline(yintercept = c(2, 4), colour = colours[["faint"]], linewidth = 0.3, linetype = "22") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(data = stages, shape = 21, fill = colours[["paper"]], size = 1.7, stroke = 0.7) +
    ggplot2::geom_label(
      ggplot2::aes(label = test), ends, hjust = -0.12, size = 3.1, fontface = "bold", colour = colours[["ink"]],
      fill = colours[["paper"]], border.colour = NA, label.padding = ggplot2::unit(0.08, "lines")
    ) +
    ggplot2::scale_colour_manual(values = stats::setNames(shades, labels), guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, top), breaks = lactate_breaks(top), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = xlim, expand = FALSE) +
    ggplot2::labs(x = NULL, y = "Blood lactate (mmol/L)") +
    theme_lactater()

  ## one bar per test, the first at the top, in the colors of the zones, with LT1 and LT2 at their boundaries
  bars <- do.call(rbind, lapply(seq_len(n), function(k) {
    z <- zones[[k]]
    data.frame(
      row = n + 1 - k, from = ifelse(is.na(z$intensity_from), xlim[1], z$intensity_from),
      to = ifelse(is.na(z$intensity_to), xlim[2], z$intensity_to), fill = zone_colours[["5"]]
    )
  }))
  marks <- do.call(rbind, lapply(seq_len(n), function(k) {
    data.frame(row = n + 1 - k, intensity = zones[[k]]$intensity_from[3:4])
  }))
  zone_bars <- ggplot2::ggplot(bars) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = from, xmax = to, ymin = row - 0.36, ymax = row + 0.36), fill = bars$fill, alpha = 0.55,
      colour = colours[["paper"]], linewidth = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(intensity, row, label = format_intensity(intensity, sport)), marks, hjust = 1.15, size = 2.7,
      fontface = "bold", colour = colours[["ink"]]
    ) +
    ggplot2::scale_y_continuous(breaks = seq_len(n), labels = rev(labels), expand = c(0, 0.1)) +
    ggplot2::coord_cartesian(xlim = xlim, expand = FALSE) +
    ggplot2::labs(x = intensity_label(sport), y = NULL) +
    theme_lactater() +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  ## the heart rate of the exercise stages of every test that has it, in the colors of its curve
  hr <- do.call(rbind, lapply(seq_len(n), function(k) {
    d <- infos[[k]]$data
    if(!"heart_rate" %in% names(d)) return(NULL)
    d <- d[d$intensity > 0 & !is.na(d$heart_rate), c("intensity", "heart_rate")]
    if(nrow(d) > 0) named(k, d)
  }))
  plots <- list(lactate, zone_bars)
  heights <- c(3, 0.35 * n + 0.4)
  if(!is.null(hr) && nrow(hr) >= 2) {
    hr_panel <- ggplot2::ggplot(hr, ggplot2::aes(intensity, heart_rate, colour = test)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(shape = 21, fill = colours[["paper"]], size = 1.6, stroke = 0.7) +
      ggplot2::scale_colour_manual(values = stats::setNames(shades, labels), guide = "none") +
      ggplot2::scale_y_continuous(n.breaks = 4) +
      ggplot2::coord_cartesian(
        xlim = xlim, ylim = c(floor((min(hr$heart_rate) - 5) / 20) * 20, max(hr$heart_rate) + 6), expand = FALSE
      ) +
      ggplot2::labs(x = NULL, y = "Heart rate (bpm)") +
      theme_lactater()
    plots <- list(lactate, hr_panel, zone_bars)
    heights <- c(3, 1.6, heights[2])
  }

  with_hr <- length(plots) == 3
  patchwork::wrap_plots(plots, ncol = 1, heights = heights) +
    patchwork::plot_annotation(
      title = if(with_hr) {
        "Where the curve, the heart rate and the zones moved"
      } else {
        "Where the curve and the zones moved"
      },
      subtitle = paste0(
        n, " tests, light to dark by date; dashed: 2 and 4 mmol/L.",
        if(with_hr) " Below them, the heart rate of the stages.",
        " Bars: the five zones of each test, with LT1 and LT2"
      ),
      theme = theme_lactater()
    )
}

#' @rdname plot_series
series_zones <- function(tests, labels, lt1, lt2) {
  lapply(seq_along(tests), function(k) {
    tryCatch(lactate_zones(tests[[k]], lt1 = lt1, lt2 = lt2, model = 5), error = function(e) {
      stop(sprintf("The test of %s: %s", labels[k], conditionMessage(e)), call. = FALSE)
    })
  })
}

#' @rdname plot_series
date_labels <- function(day) {
  if(inherits(day, "Date")) trimws(format(day, "%e %b %Y")) else paste("Day", day)
}

utils_head <- function(x, n) x[seq_len(min(n, length(x)))]
