## test-retest coefficients of variation (%) of the threshold intensity, where a study reports them:
## Heuberger et al. (2018): 48 well-trained male cyclists, 5-min steps, five tests about two weeks apart;
## Pallares et al. (2016): 14 trained male cyclists, 25 W/min ramp, two tests 2-5 days apart
method_cv <- c(
  "IAT" = 3.6, "OBLA 4.0" = 3.8, "Dmax" = 3.4, "ModDmax" = 4.3,
  "Bsln + 0.5" = 3.7, "Bsln + 1.0" = 3.4, "Bsln + 1.5" = 3.4
)

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
  if(!is.null(cv) && (!is.numeric(cv) || anyNA(cv) || any(cv <= 0) || (length(cv) > 1 && is.null(names(cv)))))
    stop("`cv` must be one positive number or a named vector of them.", call. = FALSE)

  methods <- intersect(as.character(before$method), as.character(after$method))
  if(length(methods) == 0) stop("The two tests have no method in common.", call. = FALSE)
  ## unrounded intensities, so the change is not driven by the rounding of the table
  value_before <- unname(info_before$intensity[methods])
  value_after <- unname(info_after$intensity[methods])
  digits <- intensity_digits(info_before$settings$sport)

  cvs <- if(length(cv) == 1 && is.null(names(cv))) {
    stats::setNames(rep(cv, length(methods)), methods)
  } else {
    replace(method_cv, names(cv), cv)
  }
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
#' @param ... The results of [lactate_threshold()], in any order.
#' @param day The day of each test (e.g. days since the first test), in the same order as `...`.
#'
#' @return a `lactate_trend` tibble with one row per method found in every test: `method`, `tests`, the
#' change per 30 days (`per_30_days`) with its 90% interval (`low`, `high`), and the first and last fitted
#' values. Plot it with [autoplot()].
#'
#' @references
#' Hopkins WG. A spreadsheet for monitoring an individual's changes and trend. Sportscience. 2017;21:5-9.
#'
#' @export
#'
#' @examples
#' series <- subset(lactate_tests, athlete == "cyclist_series")
#' days <- unique(series$day)
#' tests <- lapply(days, function(d) lactate_threshold(subset(series, day == d), intensity, lactate, plot = FALSE))
#' do.call(lactate_trend, c(tests, list(day = days)))
lactate_trend <- function(..., day) {
  tests <- list(...)
  infos <- lapply(tests, lactater_info)
  if(length(tests) < 3) stop("A trend needs at least three tests.", call. = FALSE)
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
    values = values, methods = methods, day = day, sport = infos[[1]]$settings$sport
  )
  class(out) <- c("lactate_trend", class(out))
  out
}

#' @rdname lactate_trend
#' @param object The result of [lactate_trend()].
#' @param method The methods to plot. Default to the first four.
#' @export
autoplot.lactate_trend <- function(object, method = utils_head(levels(object$method), 4), ...) {
  trend <- attr(object, "lactater_trend")
  colours <- lactater_colours()
  keep <- trend$methods %in% method
  points <- data.frame(
    method = factor(rep(trend$methods[keep], times = length(trend$day)), levels = method),
    day = rep(trend$day, each = sum(keep)),
    intensity = as.vector(trend$values[keep, , drop = FALSE])
  )
  points <- points[!is.na(points$intensity), ]
  lines <- object[object$method %in% method, ]
  lines <- data.frame(
    method = factor(rep(as.character(lines$method), 2), levels = method),
    day = rep(range(trend$day), each = nrow(lines)),
    intensity = c(lines$first, lines$last)
  )
  labels <- object[object$method %in% method, ]
  labels$text <- sprintf(
    "%+.1f %s per 30 days (90%% CI %+.1f to %+.1f)", labels$per_30_days, intensity_unit(trend$sport),
    labels$low, labels$high
  )
  labels$method <- factor(as.character(labels$method), levels = method)

  ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(day, intensity), lines, colour = colours[["accent"]], linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(day, intensity), points, shape = 21, size = 2.4, stroke = 0.8,
      fill = colours[["paper"]], colour = colours[["curve"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = -Inf, y = Inf, label = text), labels, hjust = -0.05, vjust = 1.4, size = 3,
      colour = colours[["muted"]]
    ) +
    ggplot2::facet_wrap(ggplot2::vars(method), scales = "free_y") +
    ggplot2::labs(
      title = "Thresholds over time",
      subtitle = "Each test as a dot, and the straight-line trend through them",
      x = "Day", y = intensity_label(trend$sport)
    ) +
    theme_lactater()
}

utils_head <- function(x, n) x[seq_len(min(n, length(x)))]

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
