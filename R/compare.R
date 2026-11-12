## test-retest coefficients of variation (%) of the threshold intensity, as lactater computes each method:
## lactate_reliability() on the five step tests of every rider in the open data of Heuberger et al. (2018): 204 tests
## of 47 well-trained male cyclists, 5-min steps, venous blood, about two weeks apart (data-raw/reliability_cycling.R).
## Half of the riders received erythropoietin and the data do not say who, so these can only be too high; pairs of
## consecutive tests gave the same size (mean 5.8-6.3% against 6.0% over the five tests), so a drift between the
## groups is not what sets them. The tests have no resting sample: Bsln+ rises from the first stage.
## Published values, which the help page lists beside these: Heuberger et al. (2018), placebo riders and their own
## implementation, IAT 3.6, OBLA 4.0 3.8, Dmax 3.4, ModDmax 4.3; Pallares et al. (2016), 14 cyclists, 25 W/min ramp,
## Bsln + 0.5 3.7, + 1.0 3.4, + 1.5 3.4.
method_cv <- c(
  "Log-log" = 7.8, "OBLA 2.0" = 7.7, "OBLA 2.5" = 5.2, "OBLA 3.0" = 4.6, "OBLA 3.5" = 4.8, "OBLA 4.0" = 4.4,
  "Bsln + 0.5" = 8.2, "Bsln + 1.0" = 5.0, "Bsln + 1.5" = 4.5, "Dmax" = 5.9, "ModDmax" = 5.1, "Exp-Dmax" = 4.3,
  "Log-Poly-ModDmax" = 5.5, "Log-Exp-ModDmax" = 4.9, "LTP1" = 10.6, "LTP2" = 6.0, "LTratio" = 9.4, "IAT" = 4.4
)

## the coefficient of variation of each method: the user's `cv` (one number, a named vector, or the result of
## lactate_reliability()) over the defaults
method_cvs <- function(cv, methods) {
  if(is.data.frame(cv) && all(c("method", "cv") %in% names(cv))) {
    cv <- stats::setNames(cv$cv, cv$method)[!is.na(cv$cv)]
  }
  if(!is.null(cv) && (!is.numeric(cv) || anyNA(cv) || any(cv <= 0) || (length(cv) > 1 && is.null(names(cv)))))
    stop("`cv` must be one positive number or a named vector of them.", call. = FALSE)
  if(length(cv) == 1 && is.null(names(cv))) {
    stats::setNames(rep(cv, length(methods)), methods)
  } else {
    replace(method_cv, names(cv), cv)
  }
}

## the quantile that turns the noise of each method into an interval at `level`: normal for the defaults (133 to 153
## degrees of freedom) and for a `cv` given as numbers, t for a `cv` from lactate_reliability(), whose few athletes
## leave the typical error itself uncertain (Swinton et al., 2018, Table 2)
noise_quantile <- function(cv, methods, level) {
  df <- if(is.data.frame(cv) && "df" %in% names(cv)) cv$df[match(methods, cv$method)] else NA_real_
  stats::qt(1 - (1 - level) / 2, ifelse(is.na(df), Inf, df))
}

#' Test-retest reliability of each threshold, from your own repeated tests
#'
#' The typical error of each threshold, from athletes who did the same test two or more times while their fitness did
#' not change (e.g. a few days apart): the residual standard deviation of a two-way analysis of athletes and tests,
#' which leaves out the mean change from one test to the next (Hopkins, 2000). It is given in intensity units and as a
#' coefficient of variation, `100 * (exp(s) - 1)`, with `s` the typical error of the log intensities. Pass the result
#' to `compare_tests(cv = )` so that its verdicts use the noise of your own protocol, analyzer and athletes rather than
#' the default values, from trained male cyclists.
#'
#' A typical error from few athletes is itself uncertain: with 10 athletes and 2 tests (9 degrees of freedom) its 90%
#' confidence interval spans 0.73 to 1.65 times its value (from the chi-square distribution of a variance). The
#' result therefore gives the degrees of freedom (`df`) and that interval for the `cv` (`cv_low`, `cv_high`), as
#' reliability statistics should always come with one (Sainani, 2017), and [compare_tests()] widens its intervals for
#' a small `df`. Atkinson & Nevill (1998) recommend more than 40 athletes for a reliability study; with fewer, read
#' the interval.
#'
#' The `bias` is the mean change from each test to the next, which the typical error leaves out. Report it beside the
#' random error (Atkinson & Nevill, 1998): a `bias` that is not near zero is a learning or order effect, or a change
#' in fitness between the tests. The `cv` assumes that the error grows with the intensity, as it usually does for
#' such measures; with athletes of a similar level the two cannot be told apart, and `typical_error` is the same
#' noise in intensity units.
#'
#' @param x The result of [lactate_threshold()] with `.by`, one group per athlete and test (e.g.
#'   `.by = c("athlete", "test")`).
#' @param athlete,test The columns of `x` that name the athlete and the test (e.g. the test number or date). Tests are
#'   matched by their order within each athlete: first, second, and so on.
#'
#' @return a tibble with one row per method: `method`, `n_athletes` (with two or more tests), `n_tests`, `bias`
#'   (the mean change from one test to the next, intensity units), `typical_error` (intensity units), `cv` (%) with
#'   its 90% confidence interval (`cv_low`, `cv_high`), and the degrees of freedom of the typical error (`df`).
#'   Methods with fewer than two such athletes get `NA`.
#'
#' @references
#' Atkinson G, Nevill AM. Statistical methods for assessing measurement error (reliability) in variables relevant to
#' sports medicine. Sports Medicine. 1998;26(4):217-238.
#'
#' Hopkins WG. Measures of reliability in sports medicine and science. Sports Medicine. 2000;30(1):1-15.
#'
#' Sainani KL. Reliability statistics. PM&R. 2017;9(6):622-628. doi:10.1016/j.pmrj.2017.05.001
#'
#' @export
#'
#' @examples
#' ## the same athlete tested twice, a few days apart (simulated here as small changes in lactate)
#' set.seed(1)
#' repeats <- do.call(rbind, lapply(1:4, function(a) do.call(rbind, lapply(1:2, function(t) {
#'   transform(demo_data, athlete = a, test = t, intensity = intensity * (0.9 + 0.05 * a),
#'             lactate = lactate * exp(rnorm(nrow(demo_data), 0, 0.05)))
#' }))))
#' results <- lactate_threshold(repeats, intensity, lactate, method = c("OBLA", "Dmax"), plot = FALSE,
#'                              .by = c("athlete", "test"))
#' reliability <- lactate_reliability(results, "athlete", "test")
#' reliability
lactate_reliability <- function(x, athlete, test) {
  if(!is.data.frame(x) || !all(c(athlete, test, "method", "intensity") %in% names(x)) ||
       length(athlete) != 1 || length(test) != 1)
    stop("`x` must be the result of `lactate_threshold(.by = )`, and `athlete` and `test` two of its columns.",
         call. = FALSE)
  rows <- lapply(unique(as.character(x$method)), function(m) {
    d <- x[as.character(x$method) == m & !is.na(x$intensity) & x$intensity > 0, ]
    who <- as.character(d[[athlete]])
    keep <- who %in% who[duplicated(who)]
    d <- d[keep, ]
    who <- who[keep]
    sorted <- order(who, d[[test]])
    d <- d[sorted, ]
    who <- who[sorted]
    ## the first, second, ... test of each athlete, so dates work as well as test numbers
    when <- factor(stats::ave(seq_along(who), who, FUN = seq_along))
    who <- factor(who)
    ## a two-way fit of athletes and tests (one term each), leaving the residual: the typical error
    fit <- function(y) if(nlevels(when) > 1) stats::lm(y ~ who + when) else stats::lm(y ~ who)
    error <- function(y) if(fit(y)$df.residual > 0) summary(fit(y))$sigma else NA_real_
    enough <- nlevels(who) >= 2
    df <- if(enough && fit(d$intensity)$df.residual > 0) fit(d$intensity)$df.residual else NA_integer_
    log_error <- if(enough) error(log(d$intensity)) else NA_real_
    ## the 90% interval of a standard deviation, from the chi-square distribution of its variance
    cv_at <- function(p) 100 * (exp(log_error * sqrt(df / stats::qchisq(p, df))) - 1)
    data.frame(
      method = m, n_athletes = nlevels(who), n_tests = nrow(d),
      bias = if(enough) mean(unlist(tapply(d$intensity, who, diff))) else NA_real_,
      typical_error = if(enough) error(d$intensity) else NA_real_,
      cv = 100 * (exp(log_error) - 1), cv_low = cv_at(0.95), cv_high = cv_at(0.05), df = df
    )
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  for(column in c("bias", "typical_error", "cv", "cv_low", "cv_high")) out[[column]] <- round(out[[column]], 2)
  out
}

#' Compare two lactate tests
#'
#' Compares two tests of the same athlete (e.g. before and after a training block): the change of each
#' threshold, whether it is larger than the noise of the method, the heart rate at each threshold, and
#' notes on what could make the comparison unfair.
#'
#' A threshold moves between two tests even without any change in fitness, by the day-to-day variation of
#' the athlete and the error of the analyzer and the method. Each change is therefore given with an
#' interval, change +/- z * sqrt(2) * typical error, where the typical error is the coefficient of
#' variation (`cv`) times the intensity before (Hopkins, 2000; Swinton et al., 2018). This is the model behind the
#' repeatability of Bland & Altman (1996), for whom two measurements of the same person differ by less than
#' `1.96 * sqrt(2)` within-subject standard deviations in 95% of pairs, a size also known as the minimal detectable
#' change (Sainani, 2017). With a `cv` from [lactate_reliability()], `z` is the t quantile for its degrees of freedom,
#' because a typical error from few athletes is itself uncertain (Swinton et al., 2018): with 10 athletes tested
#' twice, the normal quantile would call 13% of unchanged pairs "beyond the noise" at the 90% level instead of 10%.
#'
#' Three columns say how the change stands against that noise, without a probability attached to them:
#' * `noise`: `sqrt(2)` x the typical error, the typical difference between two tests when nothing changed, in the
#'   unit of the intensity.
#' * `ratio`: the change divided by the noise. At 1, the change is as large as two tests usually differ by anyway.
#' * `verdict`: `"beyond the noise"` when the whole interval lies beyond the smallest worthwhile change (`swc`; beyond
#'   zero by default), which at the default 90% is a ratio above 1.64, and `"within the noise"` otherwise. A change
#'   within the noise is not a sign that nothing changed: one pair of tests cannot tell. The words describe this one
#'   comparison against the `cv` in the table; graded terms such as "likely" are not used, because they read a
#'   frequentist interval as a probability of the true change (Lohse et al., 2020).
#'
#' Three things decide how often a verdict is wrong, so settle them before looking at the result:
#' * **The level.** At the default 90%, an unchanged threshold is called "beyond the noise" in 10% of comparisons: 5%
#'   as a gain and 5% as a loss, which is a one-sided test for a gain at 5%, the test Sainani (2018) recommends in
#'   place of graded terms. No level is agreed on: 95% is the classic choice, and narrower intervals are defended for
#'   decisions in practice (Swinton et al., 2018).
#' * **The smallest worthwhile change.** With `swc` above zero, the verdict asks for a change that is certainly
#'   larger than `swc`, not merely larger than zero. No value is agreed on for lactate thresholds; one convention is
#'   0.2 times the standard deviation between athletes (Swinton et al., 2018). Choosing `swc` or `level` after seeing
#'   the verdict lets one pick the verdict one likes (Sainani, 2018).
#' * **The threshold.** Each method gets its own verdict, and with every method in the table some of them will lie
#'   beyond the noise by chance. In 157 pairs of consecutive tests of the cyclists of Heuberger et al. (2018), each
#'   method was beyond the noise in 6% to 15% of pairs, but at least one of the 18 was in 50%, and the default LT1 or
#'   LT2 in 19%. The LT1 and LT2 chosen in [lactate_threshold()] are the ones to read, as the header does; a method
#'   picked because its verdict stands out proves nothing.
#'
#' The default `cv` of each method is lactater's own calculation: [lactate_reliability()] on the five step tests of
#' each of 47 well-trained male cyclists in the open data of Heuberger et al. (2018; 204 tests, 5-min stages, venous
#' blood, about two weeks apart), so that it is the noise of each method as lactater computes it, on a step test. The
#' output gives it in the `cv` column.
#'
#' | Method | Default `cv` (%) | Published (%) |
#' |:--|--:|:--|
#' | OBLA 2.0, 2.5, 3.0, 3.5 | 7.7, 5.2, 4.6, 4.8 | |
#' | OBLA 4.0 | 4.4 | 3.8 (Heuberger et al., 2018) |
#' | Bsln + 0.5, 1.0, 1.5 | 8.2, 5.0, 4.5 | 3.7, 3.4, 3.4 (Pallares et al., 2016; ramp test) |
#' | Dmax | 5.9 | 3.4 (Heuberger et al., 2018) |
#' | ModDmax | 5.1 | 4.3 (Heuberger et al., 2018) |
#' | Exp-Dmax, Log-Poly-ModDmax, Log-Exp-ModDmax | 4.3, 5.5, 4.9 | |
#' | IAT | 4.4 | 3.6 (Heuberger et al., 2018) |
#' | LTP1, LTP2 | 10.6, 6.0 | |
#' | Log-log, LTratio | 7.8, 9.4 | |
#'
#' The defaults are on the careful side. Heuberger et al. computed their values on the riders who received placebo,
#' with their own implementation of each method; the open data do not say which riders received erythropoietin, so the
#' defaults use every rider and are, if anything, too high (pairs of consecutive tests gave values of the same size,
#' so a drift between the two groups does not explain them). Those tests have no resting sample, so Bsln+ rose from
#' the first stage. To use the published values instead, give them: `cv = c("IAT" = 3.6, "Dmax" = 3.4)`.
#' `Stegmann`, `Keul`, `Simon` and your own `fixed` or `baseline_plus` values have no default: their `verdict` is `NA`
#' unless you give one.
#'
#' The defaults come from cycling and are used for every sport, as a percentage of the intensity; the `notes` say so
#' for other sports. No open test-retest data of running step tests were found to compute running values the same way.
#' Published running values are of a similar size in trained runners: typical errors of about 0.3 to 0.8 km/h (2% to
#' 6%) for 22 trained men on a treadmill with 1-min stages, from the SDs of their test-retest differences (the CVs
#' they report, 1.2% to 3.5%, are smaller; Cerezuela-Espejo et al., 2018), and 0.4 to 1.4 km/h for 19 healthy,
#' untrained people with 4-min and 8-min stages (Gavin et al., 2014).
#' The best `cv` is the one of your own athletes, measured with your protocol and analyzer. A `cv` assumes that the
#' noise grows with the intensity, which is common for such measures but should be checked where the data allow
#' (Atkinson & Nevill, 1998).
#'
#' @param before,after The results of [lactate_threshold()] for the two tests, with the same sport.
#' @param cv The test-retest coefficient of variation (%) of each method: one number for all methods, or a
#' named vector (e.g. `c("LTP2" = 3)`) that adds to or replaces the defaults, or the result of [lactate_reliability()]
#' for your own tests.
#' @param swc The smallest worthwhile change, in % of the intensity before. Defaults to `0`.
#' @param level The confidence level of the interval. Defaults to `0.9`.
#'
#' @return a `lactate_comparison` tibble with one row per method found in both tests: `method`, `before`,
#' `after`, `change`, `change_percent`, the interval of the change (`change_low`, `change_high`), the `cv` (%) it
#' was computed with, the `noise`, the `ratio` of the change to it, the `verdict`,
#' and the heart rate at each threshold in both tests (when recorded). The `notes` attribute lists what differs
#' between the two tests (stages, step, resting sample, sample type) and a fall in peak lactate. Plot both
#' curves with [autoplot()].
#'
#' @references
#' Atkinson G, Nevill AM. Statistical methods for assessing measurement error (reliability) in variables relevant to
#' sports medicine. Sports Medicine. 1998;26(4):217-238.
#'
#' Bland JM, Altman DG. Measurement error. BMJ. 1996;312(7047):1654. doi:10.1136/bmj.312.7047.1654
#'
#' Cerezuela-Espejo V, Courel-Ibáñez J, Morán-Navarro R, Martínez-Cava A, Pallarés JG. The relationship between
#' lactate and ventilatory thresholds in runners: validity and reliability of exercise test performance parameters.
#' Frontiers in Physiology. 2018;9:1320. doi:10.3389/fphys.2018.01320
#'
#' Gavin JP, Willems MET, Myers SD. Reproducibility of lactate markers during 4 and 8 min stage incremental running:
#' a pilot study. Journal of Science and Medicine in Sport. 2014;17(6):635-639. doi:10.1016/j.jsams.2013.08.006
#'
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and
#' predictive value of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#'
#' Hopkins WG. Measures of reliability in sports medicine and science. Sports Medicine. 2000;30(1):1-15.
#'
#' Lohse KR, Sainani KL, Taylor JA, Butson ML, Knight EJ, Vickers AJ. Systematic review of the use of
#' "magnitude-based inference" in sports science and medicine. PLOS ONE. 2020;15(6):e0235318.
#'
#' Pallares JG, Moran-Navarro R, Ortega JF, Fernandez-Elias VE, Mora-Rodriguez R. Validity and reliability of
#' ventilatory and blood lactate thresholds in well-trained cyclists. PLOS ONE. 2016;11(9):e0163389.
#'
#' Sainani KL. Reliability statistics. PM&R. 2017;9(6):622-628. doi:10.1016/j.pmrj.2017.05.001
#'
#' Sainani KL. The problem with "magnitude-based inference". Medicine & Science in Sports & Exercise.
#' 2018;50(10):2166-2176. doi:10.1249/MSS.0000000000001645
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
  half_width <- noise_quantile(cv, methods, level) * sqrt(2) * typical_error
  ## not named `change`: tibble() would read the rounded column of that name in the lines below
  difference <- value_after - value_before
  worthwhile <- swc / 100 * value_before
  beyond <- difference - half_width > worthwhile | difference + half_width < -worthwhile
  verdict <- ifelse(beyond, "beyond the noise", "within the noise")
  ## what two tests differ by when nothing changed: the standard deviation of a difference of two measurements
  ## (not named `noise`: tibble() would read the rounded column of that name for the ratio)
  noise_sd <- sqrt(2) * typical_error

  out <- tibble::tibble(
    method = factor(methods, levels = methods),
    before = round(value_before, digits),
    after = round(value_after, digits),
    change = round(difference, digits),
    change_percent = round(100 * difference / value_before, 1),
    change_low = round(difference - half_width, digits),
    change_high = round(difference + half_width, digits),
    cv = unname(cvs[methods]),
    noise = round(noise_sd, digits),
    ratio = round(difference / noise_sd, 1),
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
  attr(out, "lactater_comparison") <- list(before = info_before, after = info_after, level = level)
  attr(out, "notes") <- c(
    comparison_notes(info_before, info_after),
    if(is.null(cv) && info_before$settings$sport != "cycling") {
      "The default `cv` comes from step tests of cyclists (Heuberger et al., 2018): give your own for this sport."
    }
  )
  class(out) <- c("lactate_comparison", class(out))
  out
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_comparison <- function(x, ...) {
  tests <- attr(x, "lactater_comparison")
  if(is.null(tests)) return(NextMethod())
  notes <- attr(x, "notes")
  c(
    "Tests compared" = sprintf(
      "%s, %d methods; noise: what two tests differ by when nothing changed; verdict at %g%%",
      tests$before$settings$sport, nrow(x), 100 * tests$level
    ),
    pair_change(x, tests$before),
    if(length(notes)) stats::setNames(notes, rep("Note", length(notes)))
  )
}

#' The change of LT1 and LT2 in one line each
#'
#' @param x The comparison.
#' @param info The `lactater` attribute of the first test, whose LT1 and LT2 are followed.
#' @return a character vector named `LT1` and `LT2` (only those chosen by method and found in both tests).
#' @keywords internal
pair_change <- function(x, info) {
  ## after select(), the columns of the line may be gone
  if(!all(c("method", "before", "after", "change", "noise", "ratio", "verdict") %in% names(x))) return(NULL)
  pair <- unlist(lapply(chosen_pair(info), as.character))
  names(pair) <- c("LT1", "LT2")
  ## a threshold missing in one test has no change to report
  pair <- pair[pair %in% as.character(x$method)[!is.na(x$change)]]
  rows <- x[match(pair, as.character(x$method)), ]
  sport <- info$settings$sport
  stats::setNames(sprintf(
    "%s to %s (%s%s, %s), by %s", format_intensity(rows$before, sport), intensity_text(rows$after, sport),
    ifelse(rows$change > 0, "+", ""), format_intensity(rows$change, sport),
    ifelse(
      is.na(rows$verdict), "no test-retest noise known for it: give `cv`",
      sprintf("%s x the noise of %s: %s", format(abs(rows$ratio), nsmall = 1), intensity_text(rows$noise, sport),
              rows$verdict)
    ), pair
  ), names(pair))
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
  known <- function(x) if(is.null(x)) NA else x
  ## the second test against the first, where both were given a stage length, site or grade
  differs <- protocol_differences(
    after$settings, known(before$settings$stage_duration), known(before$settings$site), known(before$settings$grade)
  )
  c(
    if(b$first != a$first || b$step != a$step) {
      sprintf("The protocol differs (first stage %s vs %s %s, step %s vs %s %s): thresholds depend on the protocol.",
              format(b$first), format(a$first), unit, format(b$step), format(a$step), unit)
    },
    if(length(differs) > 0) {
      sprintf("The second test had %s: thresholds depend on the protocol.", paste(differs, collapse = "; "))
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
#' `NA`. Defaults to `seq(1.5, 4, by = 0.5)`.
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
#' with its uncertainty instead of reading noise from one test to the next.
#'
#' @param ... The results of [lactate_threshold()], in any order. A list of results can be spliced in with `!!!`.
#' @param day The date of each test, in the same order as `...`: dates, or days since the first test.
#' @param level The confidence level of the interval of the trend, and of the noise band in the `"thresholds"` view.
#'   Defaults to `0.9`, as in [compare_tests()].
#'
#' @return a `lactate_trend` tibble with one row per method found in every test: `method`, `tests`, the
#' change per 30 days (`per_30_days`) with its confidence interval at `level` (`low`, `high`), and the first and last
#' fitted values. Plot it with [autoplot()]: up to five tests as their lactate curves and training zones, more as each
#' threshold over time.
#'
#' @references
#' Hopkins WG. Measures of reliability in sports medicine and science. Sports Medicine. 2000;30(1):1-15.
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
lactate_trend <- function(..., day, level = 0.9) {
  tests <- rlang::list2(...)
  infos <- lapply(tests, lactater_info)
  if(length(tests) < 3) stop("A trend needs at least three tests.", call. = FALSE)
  if(inherits(day, "POSIXt")) day <- as.Date(day)
  labels <- date_labels(day)
  dates <- if(inherits(day, "Date")) day
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
    interval <- stats::confint(fit, level = level)[2, ] * 30
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
    tests = tests, labels = labels, values = values, methods = methods, day = day, dates = dates,
    sport = infos[[1]]$settings$sport, level = level,
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
  labeled <- function(.data, label) transform(.data, test = factor(label, levels = c("Before", "After")))
  curves <- rbind(
    labeled(curve_points(tests$before$curve), "Before"),
    labeled(curve_points(tests$after$curve), "After")
  )
  stages <- function(info) info$data[info$data$intensity > 0, c("intensity", "lactate")]
  points <- rbind(labeled(stages(tests$before), "Before"), labeled(stages(tests$after), "After"))
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
#' @param method The methods to plot in the `"thresholds"` view. Defaults to ModDmax, IAT, OBLA 4.0 and Dmax, which have
#' a published test-retest variation (see [compare_tests()]), when present.
#' @param view `"curves"`, the default for up to five tests: the lactate curves of all tests (the latest in blue, the
#' first in dark gray, the tests between in light gray), to see where along the intensity range the curve moved, then
#' their heart rate with LT1 and LT2 as filled points (the
#' latest test labeled), above the five training zones of each test from `lt1` and `lt2`. `"thresholds"`, the default
#' for more tests: one panel per method, each test as a dot (with its own bootstrap interval when computed, at the
#' `ci_level` of [lactate_threshold()]) and a gray band for the noise of the method around the first test, at the
#' `level` of [lactate_trend()], so that dots outside the band are changes larger than the noise. Every test is one
#' more look: with nothing changed, at least one dot falls outside a 90% band in 24% of series with three later
#' tests, 33% with five and 43% with eight, so one dark dot in a long series is weak evidence, and the trend line of
#' [lactate_trend()] is the answer for the series.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2 in the `"curves"` view. Default to those each test chose in
#'   [lactate_threshold()].
#' @param cv The test-retest coefficient of variation (%) for the noise band, as in [compare_tests()].
#' @export
autoplot.lactate_trend <- function(object, method = NULL, view = NULL, cv = NULL, lt1 = NULL, lt2 = NULL, ...) {
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
  ## noise band: the first test +/- quantile * sqrt(2) * typical error, where a test-to-test change falls with the
  ## probability `level` when nothing changed (Hopkins, 2000)
  first <- points[points$day == min(trend$day), ]
  first$half <- noise_quantile(cv, as.character(first$method), trend$level) * sqrt(2) *
    unname(cvs[as.character(first$method)]) / 100 * first$intensity
  points$outside <- abs(points$intensity - first$intensity[match(points$method, first$method)]) >
    first$half[match(points$method, first$method)]
  points$outside[is.na(points$outside)] <- FALSE
  first <- first[!is.na(first$half), ]
  ## the tests on their dates when the trend has them, with few labels, so they stay apart in narrow panels
  points$at <- if(is.null(trend$dates)) points$day else trend$dates[match(points$day, trend$day)]
  ## the band spans the panel; a date scale wants its infinities as dates
  span <- if(is.null(trend$dates)) c(-Inf, Inf) else structure(c(-Inf, Inf), class = "Date")
  x_scale <- if(is.null(trend$dates)) {
    ggplot2::scale_x_continuous(n.breaks = 3)
  } else {
    ggplot2::scale_x_date(breaks = pretty(trend$dates, n = 3), labels = function(d) format(d, "%b\n%Y"))
  }

  ggplot2::ggplot(points, ggplot2::aes(at, intensity)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = span[1], xmax = span[2], ymin = intensity - half, ymax = intensity + half), first,
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
    x_scale +
    ggplot2::facet_wrap(
      ggplot2::vars(method), nrow = 1, scales = "free_y",
      labeller = ggplot2::as_labeller(function(x) sprintf("bold(%s)", method_plotmath(x)), ggplot2::label_parsed)
    ) +
    ggplot2::labs(
      title = "Did the thresholds change?",
      subtitle = paste(
        sprintf("Each test as a dot; gray band: the noise of the method around the first test, where %g%% of changes",
                100 * trend$level),
        "fall when nothing changed. Dark dots lie outside it; in a long series, one may do so by chance"
      ),
      x = if(is.null(trend$dates)) "Day" else NULL, y = intensity_label(trend$sport)
    ) +
    theme_lactater()
}

#' The lactate curves and training zones of a few tests
#'
#' `plot_series()` draws every lactate curve (the latest test in blue, the first in dark gray, the tests between in
#' light gray), then the heart rate of the stages in the same colors with LT1 and LT2 as filled points (when the tests
#' have it), above the five zones of each test as one bar that ends at its highest stage, on one intensity axis.
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
  ends$end <- ends$lactate
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

  ## the latest test in blue, the first in dark gray, the tests between in light gray and drawn first, under the two
  shades <- utils::tail(c(colours[["muted"]], rep(colours[["faint"]], max(n - 2, 0)), colours[["curve"]]), n)
  ends_of_series <- labels[unique(c(1, n))]
  curves$draw <- factor(curves$test, levels = c(setdiff(labels, ends_of_series), ends_of_series))
  thick <- ggplot2::scale_linewidth_manual(values = c(`FALSE` = 0.6, `TRUE` = 0.95), guide = "none")
  lactate <- ggplot2::ggplot(curves, ggplot2::aes(intensity, lactate, colour = test)) +
    ggplot2::geom_hline(yintercept = c(2, 4), colour = colours[["faint"]], linewidth = 0.3, linetype = "22") +
    ggplot2::geom_line(ggplot2::aes(group = draw, linewidth = test %in% ends_of_series)) +
    thick +
    ggplot2::geom_point(data = stages, shape = 21, fill = colours[["paper"]], size = 1.7, stroke = 0.7) +
    ggplot2::geom_segment(
      ggplot2::aes(x = intensity, xend = intensity + 0.015 * span, y = end, yend = lactate),
      ends[ends$lactate != ends$end, ], inherit.aes = FALSE, colour = colours[["muted"]], linewidth = 0.3
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = test), ends, hjust = -0.12, size = 3.1,
      fontface = ifelse(ends$test == labels[n], "bold", "plain"),
      colour = ifelse(ends$test %in% ends_of_series, colours[["ink"]], colours[["muted"]]), fill = colours[["paper"]],
      border.colour = NA, label.padding = ggplot2::unit(0.08, "lines")
    ) +
    ggplot2::scale_colour_manual(values = stats::setNames(shades, labels), guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, top), breaks = lactate_breaks(top), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = xlim, expand = FALSE) +
    ggplot2::labs(x = NULL, y = "Blood lactate (mmol/L)") +
    theme_lactater()

  ## one bar per test, the first at the top, in the colors of the zones, with LT1 and LT2 at their boundaries; the
  ## bar ends at the highest stage of its test, where zone 5 was last seen
  peaks <- vapply(infos, function(i) max(i$data$intensity), 0)
  bars <- do.call(rbind, lapply(seq_len(n), function(k) {
    z <- zones[[k]]
    data.frame(
      row = n + 1 - k, from = ifelse(is.na(z$intensity_from), xlim[1], z$intensity_from),
      to = ifelse(is.na(z$intensity_to), peaks[k], z$intensity_to), fill = zone_colours[["5"]]
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
    ## what the two numbers are, over the first bar, and where each test ended
    ggplot2::annotate(
      "text", x = marks$intensity[marks$row == n], y = n + 0.36, label = c("LT1", "LT2"), hjust = 1.2, vjust = -0.5,
      size = 2.5, colour = colours[["muted"]]
    ) +
    ggplot2::annotate(
      "text", x = peaks, y = rev(seq_len(n)), label = paste("peak", format_intensity(peaks, sport)), hjust = -0.15,
      size = 2.5, colour = colours[["muted"]]
    ) +
    ggplot2::scale_y_continuous(breaks = seq_len(n), labels = rev(labels)) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0.54, n + 0.9), expand = FALSE) +
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
    ## the heart rate at LT1 and LT2 of each test on a filled point of its color, labeled for the latest test
    at <- do.call(rbind, lapply(seq_len(n), function(k) {
      z <- zones[[k]]
      if("heart_rate_from" %in% names(z)) {
        named(k, data.frame(
          intensity = z$intensity_from[3:4], heart_rate = z$heart_rate_from[3:4], lt = c("LT1", "LT2")
        ))
      }
    }))
    at <- at[!is.na(at$heart_rate), ]
    latest <- at[at$test == labels[n], ]
    ylim <- c(floor((min(hr$heart_rate) - 5) / 20) * 20, max(hr$heart_rate) + 6)
    ## lines without the stages, which the curves above show, so the points at LT1 and LT2 stand out
    hr$draw <- factor(hr$test, levels = levels(curves$draw))
    hr_panel <- ggplot2::ggplot(hr, ggplot2::aes(intensity, heart_rate, colour = test)) +
      ggplot2::geom_line(ggplot2::aes(group = draw, linewidth = test %in% ends_of_series)) +
      thick +
      ggplot2::geom_point(ggplot2::aes(fill = test), at, shape = 21, size = 2.6, stroke = 0.9,
                          colour = colours[["paper"]]) +
      lapply(seq_len(nrow(latest)), function(k) {
        hr_point_label(latest$intensity[k], latest$heart_rate[k], latest$lt[k], xlim, ylim, colours)
      }) +
      ggplot2::scale_colour_manual(values = stats::setNames(shades, labels), guide = "none") +
      ggplot2::scale_fill_manual(values = stats::setNames(shades, labels), guide = "none") +
      ggplot2::scale_y_continuous(n.breaks = 4) +
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(x = NULL, y = "Heart rate (bpm)") +
      theme_lactater()
    plots <- list(lactate, hr_panel, zone_bars)
    heights <- c(3, 1.6, heights[2])
  }

  with_hr <- length(plots) == 3
  moved <- function(change) paste(sprintf("%+.*f", intensity_digits(sport), change), intensity_unit(sport))
  patchwork::wrap_plots(plots, ncol = 1, heights = heights) +
    patchwork::plot_annotation(
      title = sprintf(
        "Since the first test: LT1 %s, LT2 %s", moved(zones[[n]]$intensity_from[3] - zones[[1]]$intensity_from[3]),
        moved(zones[[n]]$intensity_from[4] - zones[[1]]$intensity_from[4])
      ),
      subtitle = paste0(
        "Blue: the latest test; dark gray: the first", if(n > 2) "; light gray: the tests between", ". Below: ",
        if(with_hr) "the heart rate, with LT1 and LT2 as filled points, and ",
        "the zones of each test up to its peak"
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
