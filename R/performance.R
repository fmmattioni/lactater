## Every equation: mean race intensity (in the unit of the sport) = intercept + slope * threshold, with its residual
## SD (`sigma`), sample size, the mean and SD of the thresholds it was fitted on (for the prediction interval; `NA`
## when not reported), the range of those thresholds, the steps of the source protocol, and the race `distance` (m)
## when the race is a time over a distance.

## Running, converted to km/h from the published m/s.
## * IAT: Roecker (Röcker) et al. (1998), Table 2 (equations, mean race time +/- SD) and Table 3 (r). The SD of race
##   speed comes from the SD of race time as distance * SD / mean time^2. IAT range read off their Fig. 2.
## * OBLA 4.0: Sjödin & Jacobs (1981), Table 5 (equation) and Table 1 (marathon speed SD, v4 range).
running_equations <- local({
  r <- c(0.91, 0.92, 0.93, 0.93, 0.957)
  n <- c(117, 225, 92, 166, 18)
  speed_sd <- c(5000 * 113.4 / 983.7^2, 10000 * 343.4 / 2280.3^2, 21097.5 * 812.8 / 4998.8^2,
                42195 * 1816.5 / 11172.8^2, 0.6)
  data.frame(
    event = c("5 km", "10 km", "Half marathon", "Marathon", "Marathon"),
    distance = c(5000, 10000, 21097.5, 42195, 42195),
    method = c("IAT", "IAT", "IAT", "IAT", "OBLA 4.0"),
    intercept = 3.6 * c(0.64, 0.20, -0.75, -0.61, -1.5265),
    slope = c(1.03, 1.05, 1.23, 1.10, 1.239),
    ## standard error of the estimate: SD x sqrt(1 - r^2), with n - 2 degrees of freedom
    sigma = 3.6 * speed_sd * sqrt(1 - r^2) * sqrt((n - 1) / (n - 2)),
    n = n, mean = NA_real_, sd = NA_real_,
    low = c(10, 10, 10, 10, 12.6), high = c(20, 20, 20, 20, 18.4),
    step_low = c(2, 2, 2, 2, 1), step_high = 2,
    stage_minutes = c(3, 3, 3, 3, 4), site = c(rep("earlobe", 4), "fingertip"), grade = c(2, 2, 2, 2, NA),
    source = c(rep("Roecker et al. 1998", 4), "Sjodin & Jacobs 1981"),
    protocol = c(rep(paste(
      "427 competitive runners (88 women); treadmill at 2% grade, 3-min stages, +2 km/h, earlobe hemolyzed",
      "whole blood; best race within 2 months of the test"
    ), 4), paste(
      "18 male marathon runners; treadmill, 4-min stages, +1 to 2 km/h, fingertip blood; marathon 2 to 3 weeks",
      "after the test"
    ))
  )
})

## Cycling: 45-min time-trial power (W), fitted by data-raw/performance_cycling.R on the open data of Heuberger et
## al. (2018) with lactater's own thresholds.
cycling_equations <- data.frame(
  event = "45-min time trial",
  distance = NA_real_,
  method = c("LTP2", "IAT", "ModDmax", "OBLA 4.0", "Dmax", "Log-Poly-ModDmax"),
  intercept = c(58.797, 89.0946, 36.1515, 106.023, 63.3548, 64.1916),
  slope = c(0.72264, 0.641256, 0.817857, 0.548225, 0.795138, 0.720807),
  sigma = c(11.7833, 12.2457, 12.3267, 12.339, 15.3764, 13.0157),
  n = c(39, 39, 35, 33, 39, 38),
  mean = c(298.486, 289.12, 290.202, 303.038, 265.539, 291.976),
  sd = c(30.2778, 33.7327, 26.5078, 34.0113, 24.6354, 29.9073),
  low = c(258.333, 243.191, 256.83, 249.057, 209.046, 218.621),
  high = c(375, 394.286, 366.708, 377.555, 323.332, 376.066),
  step_low = 25, step_high = 25, stage_minutes = 5, site = "venous", grade = NA_real_,
  source = "Heuberger et al. 2018 (refitted)",
  protocol = paste(
    "48 well-trained male cyclists; 5-min stages, +25 W from 175 W, venous blood (Lactate Pro 2);",
    "45-min time trial a week after the test"
  )
)

#' Predict performance from a step test
#'
#' Estimates race times in running and time-trial power in cycling from the thresholds of a step test, with a 95%
#' prediction range. Each estimate comes from an equation fitted on one threshold, in one population, with one test
#' protocol, so it only applies to tests run the same way:
#'
#' * **Running**: 5 km, 10 km, half marathon and marathon from IAT (Roecker et al., 1998; 427 competitive runners,
#'   treadmill at 2% grade, 3-min stages, +2 km/h, earlobe hemolyzed whole blood), and the marathon from OBLA 4.0
#'   (Sjödin & Jacobs, 1981; 18 male marathon runners, 4-min stages, fingertip blood). lactater's `IAT` follows the
#'   definition of Roecker et al. (the lowest lactate/speed ratio + 1.5 mmol/L).
#' * **Cycling**: 45-min time-trial power from LTP2, IAT, ModDmax, OBLA 4.0, Dmax or Log-Poly-ModDmax, fitted with
#'   lactater's own thresholds on the open data of Heuberger et al. (2018; 48 well-trained men, 5-min stages,
#'   +25 W, venous blood).
#' * **Your own equation**, from [calibrate_performance()]: any threshold, sport and protocol, fitted on your own
#'   athletes' tests and races. This is the best option when you have the data.
#'
#' Other thresholds are not offered with the published equations: an equation fitted on one threshold gives wrong
#' numbers for another.
#'
#' @details
#' The 95% range is the prediction interval of the regression: the predicted intensity +/- t x the standard error of
#' the estimate x `sqrt(1 + 1/n + (threshold - mean)^2 / ((n - 1) * sd^2))`. Roecker et al. (1998) reported neither
#' that error nor the mean of the thresholds per distance, so the error is derived from their tables as
#' `SD x sqrt(1 - r^2)`, with the SD of race speed computed from the SD of race time, and the last term is left out.
#'
#' Expect an individual error of about +/- 10% of race speed in running and +/- 25 W in cycling with the published
#' equations. A warning is given when the steps of the test differ from those of the source study (a different step
#' changes the threshold: Coen et al., 2001; Kuipers et al., 2003), and when a threshold lies outside the range of
#' the source sample. Stage duration, sampling site and treadmill grade are checked too when the test was analyzed with
#' `stage_duration`, `site` and `grade` (see [lactate_threshold()]); otherwise compare them with the protocol in the
#' printed header. A recent race is a better predictor of the next one than any step test
#' (Vickers & Vertosick, 2016).
#'
#' @param x The result of [lactate_threshold()].
#' @param method The thresholds to predict from. Default to every threshold of `x` that has an equation.
#' @param equation Your own equation(s) from [calibrate_performance()], one or a list of them, used in place of the
#' published ones.
#'
#' @return a `lactate_performance` tibble with one row per estimate: `event`, `method`, `threshold`, the predicted
#' mean race `intensity` (speed or power, in the unit of the sport) with its 95% range (`intensity_low`,
#' `intensity_high`), for races over a distance the `time` with its range (`time_low`, `time_high`), and the `source`.
#' Plot it with [autoplot()].
#'
#' @references
#' Coen B, Urhausen A, Kindermann W. Individual anaerobic threshold: methodological aspects of its assessment in
#' running. International Journal of Sports Medicine. 2001;22(1):8-16.
#'
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and predictive
#' value of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#'
#' Kuipers H, Rietjens G, Verstappen F, Schoenmakers H, Hofman G. Effects of stage duration in incremental running
#' tests on physiological variables. International Journal of Sports Medicine. 2003;24(7):486-491.
#'
#' Roecker K, Schotte O, Niess AM, Horstmann T, Dickhuth HH. Predicting competition performance in long-distance
#' running by means of a treadmill test. Medicine & Science in Sports & Exercise. 1998;30(10):1552-1557.
#'
#' Sjödin B, Jacobs I. Onset of blood lactate accumulation and marathon running performance. International Journal
#' of Sports Medicine. 1981;2(1):23-26.
#'
#' Vickers AJ, Vertosick EA. An empirical study of race times in recreational endurance runners. BMC Sports Science,
#' Medicine and Rehabilitation. 2016;8:26.
#'
#' @export
#'
#' @examples
#' runner <- data.frame(
#'   intensity = c(0, 8, 10, 12, 14, 16, 18),
#'   lactate = c(1.0, 1.1, 1.2, 1.6, 2.6, 4.6, 8.0)
#' )
#' results <- lactate_threshold(runner, intensity, lactate, sport = "running", plot = FALSE)
#' prediction <- predict_performance(results)
#' prediction
#' autoplot(prediction)
predict_performance <- function(x, method = NULL, equation = NULL) {
  info <- lactater_info(x)
  sport <- info$settings$sport
  equations <- if(!is.null(equation)) {
    if(inherits(equation, "lactate_calibration")) equation <- list(equation)
    if(!all(vapply(equation, inherits, logical(1), "lactate_calibration")))
      stop("`equation` must come from `calibrate_performance()`.", call. = FALSE)
    if(any(vapply(equation, function(e) e$sport != sport, logical(1))))
      stop(sprintf("`equation` was calibrated for another sport than %s.", sport), call. = FALSE)
    do.call(rbind, lapply(equation, function(e) as.data.frame(e)[names(running_equations)]))
  } else {
    switch(
      sport,
      running = running_equations,
      cycling = cycling_equations,
      stop(sprintf("There is no published equation for %s; calibrate your own with `calibrate_performance()`.",
                   sport), call. = FALSE)
    )
  }
  offered <- unique(equations$method)
  found <- intersect(offered, names(info$intensity)[!is.na(info$intensity)])
  if(is.null(method)) method <- found
  unknown <- setdiff(method, offered)
  if(length(unknown) > 0)
    stop(sprintf("There is no %s equation for %s. Equations exist for: %s.", sport, paste(unknown, collapse = ", "),
                 paste(offered, collapse = ", ")), call. = FALSE)
  missing <- setdiff(method, found)
  if(length(missing) > 0) stop(sprintf("Not found in this test: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  if(length(method) == 0) stop("No threshold with an equation was found in this test.", call. = FALSE)
  equations <- equations[equations$method %in% method, ]
  threshold <- unname(info$intensity[equations$method])
  unit <- intensity_unit(sport)

  ## the steps of the test, to compare with the source protocol
  step <- stats::median(diff(sort(info$data$intensity[info$data$intensity > 0])))
  off <- !is.na(equations$step_low) & !is.na(step) &
    (step < equations$step_low * 0.95 | step > equations$step_high * 1.05)
  for(source in unique(equations$source[off])) {
    expected <- unique(unlist(equations[equations$source == source, c("step_low", "step_high")]))
    warning(sprintf(
      "The steps of this test (+%s %s) differ from those of %s (+%s %s): the thresholds, and so %s",
      format(step), unit, source, paste(expected, collapse = " to "), unit, "the prediction, may shift."
    ), call. = FALSE)
  }
  for(k in which(!duplicated(equations$source))) {
    differs <- protocol_differences(info$settings, equations$stage_minutes[k], equations$site[k], equations$grade[k])
    if(length(differs) > 0) warning(sprintf(
      "This test differs from the protocol of %s (%s): the thresholds, and so the prediction, may shift.",
      equations$source[k], paste(differs, collapse = "; ")
    ), call. = FALSE)
  }
  outside <- threshold < equations$low | threshold > equations$high
  for(k in which(outside & !duplicated(paste(equations$method, equations$source)))) {
    warning(sprintf(
      "%s (%s %s) is outside the range of %s (%s to %s %s): the prediction is an extrapolation.",
      equations$method[k], format_intensity(threshold[k], sport), unit, equations$source[k],
      format_intensity(equations$low[k], sport), format_intensity(equations$high[k], sport), unit
    ), call. = FALSE)
  }

  intensity <- equations$intercept + equations$slope * threshold
  leverage <- ifelse(is.na(equations$sd), 0, (threshold - equations$mean)^2 / ((equations$n - 1) * equations$sd^2))
  half <- stats::qt(0.975, equations$n - 2) * equations$sigma * sqrt(1 + 1 / equations$n + leverage)
  digits <- intensity_digits(sport)
  out <- tibble::tibble(
    event = equations$event, method = equations$method, threshold = round(threshold, digits),
    intensity = round(intensity, digits), intensity_low = round(intensity - half, digits),
    intensity_high = round(intensity + half, digits)
  )
  if(any(!is.na(equations$distance))) {
    ## the times first, after the threshold: they are what a runner reads
    times <- tibble::tibble(
      time = format_time(race_time(intensity, equations$distance, sport)),
      time_low = format_time(race_time(intensity + half, equations$distance, sport)),
      time_high = format_time(race_time(intensity - half, equations$distance, sport))
    )
    out <- tibble::as_tibble(c(out[1:3], times, out[-(1:3)]))
  }
  out$source <- equations$source
  protocols <- stats::setNames(equations$protocol, equations$source)
  attr(out, "lactater_performance") <- list(
    sport = sport, protocols = protocols[!duplicated(names(protocols))],
    plot = data.frame(
      event = out$event, method = out$method, threshold = threshold, value = intensity, low = intensity - half,
      high = intensity + half,
      label = if("time" %in% names(out)) out$time else paste(format_intensity(intensity, sport), unit)
    )
  )
  class(out) <- c("lactate_performance", class(out))
  out
}

#' Calibrate your own performance equation
#'
#' Fits the relationship between a threshold and race performance on your own athletes, for any threshold, sport and
#' test protocol, so that [predict_performance()] can use it for new tests. This is the most valid way to predict
#' performance from a step test: the published equations only apply to tests run like those of their studies, while
#' yours applies to your protocol, your analyzer and your athletes.
#'
#' Give one row per athlete: the threshold from [lactate_threshold()] and the result of a race run soon after the
#' test (e.g. within 2 months, as in Roecker et al., 1998). With `distance`, `performance` is the race time in seconds
#' and is converted to the mean race intensity of the sport (speed in km/h for running, m/s for swimming, and power
#' in W for rowing, with the Concept2 relation `watts = 2.80 / pace^3`, pace in s/m); without it, `performance` is
#' already a mean intensity (e.g. time-trial power in W). The equation is a straight line, `intensity = intercept +
#' slope * threshold`, as in the published equations. Use the same protocol for the new tests as for the athletes it
#' was fitted on, and more athletes make a narrower, more stable range.
#'
#' @param data A data frame with one row per athlete.
#' @param threshold The column of `data` with the threshold, in the intensity unit of the sport.
#' @param performance The column of `data` with the race time (s) when `distance` is given, or the mean race
#' intensity otherwise.
#' @param method The lactater method the thresholds come from (e.g. `"LTP2"`), read from new tests in
#' [predict_performance()].
#' @param sport `"cycling"`, `"running"`, `"rowing"` or `"swimming"`, as in [lactate_threshold()].
#' @param distance The race distance in meters, when `performance` is a time. Not for cycling.
#' @param event A name for the race. Default to the distance (e.g. `"10 km"`), or `"Performance"`.
#' @param protocol A short description of your test protocol and population, printed with every prediction.
#'
#' @return a `lactate_calibration` tibble with one row: the equation (`intercept`, `slope`), its residual SD
#' (`sigma`), `n`, the correlation `r`, the leave-one-out prediction error (`loo_error`), and what
#' [predict_performance()] needs. Plot it with [autoplot()].
#' @export
#'
#' @examples
#' ## thresholds and 10 km times of 12 runners from your lab (simulated here)
#' set.seed(1)
#' lab <- data.frame(ltp2 = round(rnorm(12, 15, 1.5), 1))
#' lab$time_10km <- round(10000 / ((0.5 + 1.02 * lab$ltp2 + rnorm(12, 0, 0.4)) / 3.6))
#' own <- calibrate_performance(
#'   lab, "ltp2", "time_10km", method = "LTP2", sport = "running", distance = 10000,
#'   protocol = "3-min stages, +1 km/h, 1% grade, earlobe"
#' )
#' own
#' autoplot(own)
calibrate_performance <- function(data, threshold, performance, method, sport = "running", distance = NULL,
                                  event = NULL, protocol = NULL) {
  sport <- match.arg(sport, c("cycling", "running", "rowing", "swimming"))
  if(!all(c(threshold, performance) %in% names(data)))
    stop("`threshold` and `performance` must name columns of `data`.", call. = FALSE)
  if(!is.character(method) || length(method) != 1) stop("`method` must be the name of one method.", call. = FALSE)
  if(!is.null(distance) && sport == "cycling")
    stop("For cycling, give the mean power as `performance`, without `distance`.", call. = FALSE)
  if(!is.null(distance) && (!is.numeric(distance) || length(distance) != 1 || distance <= 0))
    stop("`distance` must be one positive number of meters.", call. = FALSE)
  x <- data[[threshold]]
  y <- data[[performance]]
  if(!is.numeric(x) || !is.numeric(y)) stop("`threshold` and `performance` must be numeric.", call. = FALSE)
  if(any(y <= 0, na.rm = TRUE)) stop("`performance` must be positive.", call. = FALSE)
  if(!is.null(distance)) y <- race_intensity(y, distance, sport)
  keep <- !is.na(x) & !is.na(y)
  if(sum(keep) < 4) stop("At least four athletes with both a threshold and a performance are needed.", call. = FALSE)
  x <- x[keep]
  y <- y[keep]
  fit <- stats::lm(y ~ x)
  press <- stats::residuals(fit) / (1 - stats::hatvalues(fit))

  out <- tibble::tibble(
    event = if(!is.null(event)) event else if(is.null(distance)) "Performance" else if(distance %% 1000 == 0) {
      paste(distance / 1000, "km")
    } else {
      paste(distance, "m")
    },
    sport = sport, distance = if(is.null(distance)) NA_real_ else distance, method = method,
    intercept = unname(stats::coef(fit)[1]), slope = unname(stats::coef(fit)[2]), sigma = summary(fit)$sigma,
    n = length(x), mean = mean(x), sd = stats::sd(x), low = min(x), high = max(x),
    r = stats::cor(x, y), loo_error = sqrt(mean(press^2)), step_low = NA_real_, step_high = NA_real_,
    stage_minutes = NA_real_, site = NA_character_, grade = NA_real_,
    source = "Your calibration", protocol = if(is.null(protocol)) "your own tests and races" else protocol
  )
  attr(out, "lactater_calibration") <- data.frame(threshold = x, intensity = y)
  class(out) <- c("lactate_calibration", class(out))
  out
}

## how the protocol of a test differs from a reference protocol, where both are known: stage length (min), sampling
## site and treadmill grade (%); `settings` are those of lactate_threshold(), `NA` marks a reference value not known
protocol_differences <- function(settings, stage_minutes = NA, site = NA, grade = NA) {
  c(
    if(!is.null(settings$stage_duration) && !is.na(stage_minutes) && settings$stage_duration != stage_minutes) {
      sprintf("%s-min stages, not %s", format(settings$stage_duration), format(stage_minutes))
    },
    if(!is.null(settings$site) && !is.na(site) && settings$site != site) {
      sprintf("%s sample, not %s", settings$site, site)
    },
    if(!is.null(settings$grade) && !is.na(grade) && settings$grade != grade) {
      sprintf("%s%% grade, not %s%%", format(settings$grade), format(grade))
    }
  )
}

## mean race intensity from a race time over a distance, and back
race_intensity <- function(time, distance, sport) {
  switch(sport, running = 3.6 * distance / time, swimming = distance / time, rowing = 2.80 / (time / distance)^3)
}
race_time <- function(intensity, distance, sport) {
  switch(
    sport,
    running = distance / (intensity / 3.6), swimming = distance / intensity,
    rowing = distance * (2.80 / intensity)^(1 / 3), cycling = rep(NA_real_, length(intensity))
  )
}

## h:mm:ss, or m:ss under an hour
format_time <- function(seconds) {
  seconds <- round(seconds)
  ifelse(
    is.na(seconds), NA_character_,
    ifelse(
      seconds >= 3600,
      sprintf("%d:%02d:%02d", seconds %/% 3600, seconds %% 3600 %/% 60, seconds %% 60),
      sprintf("%d:%02d", seconds %/% 60, seconds %% 60)
    )
  )
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_performance <- function(x, ...) {
  performance <- attr(x, "lactater_performance")
  if(is.null(performance)) return(NextMethod())
  c("Predicted performance" = paste0(performance$sport, ", with 95% prediction ranges"), performance$protocols)
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_calibration <- function(x, ...) {
  unit <- intensity_unit(x$sport)
  c(
    "Performance equation" = sprintf(
      "%s (%s): intensity = %.3g + %.3g x %s, in %s", x$event, x$sport, x$intercept, x$slope, x$method, unit
    ),
    "Fit" = sprintf("n = %d, r = %.2f, leave-one-out prediction error %.3g %s", x$n, x$r, x$loo_error, unit),
    "Protocol" = x$protocol
  )
}

#' @rdname predict_performance
#' @param object The result of [predict_performance()].
#' @param ... Not used.
#' @export
autoplot.lactate_performance <- function(object, ...) {
  colours <- lactater_colours()
  performance <- attr(object, "lactater_performance")
  sport <- performance$sport
  rows <- performance$plot
  ## one event (cycling): the rows only need the method
  names <- if(length(unique(rows$event)) == 1) rows$method else paste0(rows$event, " (", rows$method, ")")
  rows$row <- factor(names, levels = rev(unique(names)))
  rows$label_at <- pmax(rows$high, rows$threshold)

  ggplot2::ggplot(rows, ggplot2::aes(y = row)) +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, yend = row), colour = colours[["muted"]], alpha = 0.55,
                          linewidth = 0.9, lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(x = threshold), shape = 21, size = 2.6, stroke = 0.8, fill = colours[["paper"]],
                        colour = colours[["accent"]]) +
    ggplot2::geom_point(ggplot2::aes(x = value), shape = 21, size = 3, stroke = 0.7, fill = colours[["curve"]],
                        colour = colours[["paper"]]) +
    ggplot2::geom_text(ggplot2::aes(x = label_at, label = label), hjust = -0.2, size = 3.2, colour = colours[["ink"]]) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.2))) +
    ggplot2::scale_y_discrete(labels = method_label) +
    ggplot2::labs(
      title = if(length(unique(rows$event)) == 1) paste("Predicted", rows$event[1]) else "Predicted race times",
      subtitle = "Blue: the predicted race intensity with its 95% range; green: the threshold it comes from",
      caption = paste("From", paste(names(performance$protocols), collapse = "; ")),
      x = intensity_label(sport), y = NULL
    ) +
    theme_lactater() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank())
}

#' @rdname calibrate_performance
#' @param object The result of [calibrate_performance()].
#' @param ... Not used.
#' @export
autoplot.lactate_calibration <- function(object, ...) {
  colours <- lactater_colours()
  points <- attr(object, "lactater_calibration")
  unit <- intensity_unit(object$sport)
  line <- data.frame(threshold = seq(object$low, object$high, length.out = 100))
  line$value <- object$intercept + object$slope * line$threshold
  line$half <- stats::qt(0.975, object$n - 2) * object$sigma *
    sqrt(1 + 1 / object$n + (line$threshold - object$mean)^2 / ((object$n - 1) * object$sd^2))

  ggplot2::ggplot(points, ggplot2::aes(threshold, intensity)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = threshold, ymin = value - half, ymax = value + half), line,
                         inherit.aes = FALSE, fill = colours[["faint"]], alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(threshold, value), line, colour = colours[["curve"]], linewidth = 0.9) +
    ggplot2::geom_point(shape = 21, size = 2.6, stroke = 0.7, fill = colours[["paper"]], colour = colours[["curve"]]) +
    ggplot2::labs(
      title = sprintf("Your %s equation", object$event),
      subtitle = sprintf(
        "Each dot an athlete; gray: the 95%% range for a new athlete. n = %d, r = %.2f, leave-one-out error %.3g %s",
        object$n, object$r, object$loo_error, unit
      ),
      x = sprintf("%s (%s)", object$method, unit),
      y = paste("Race", sub("^(.)", "\\L\\1", intensity_label(object$sport), perl = TRUE))
    ) +
    theme_lactater()
}
