#' Lactate thresholds from a step test
#'
#' Estimates the lactate thresholds of a step incremental test with every method at once, or the methods you choose,
#' and returns them with the lactate, heart rate and RPE at each one. How each method is defined, how lactater computes
#' it and what the evidence says is in the article
#' [The methods and the evidence](https://fmmattioni.github.io/lactater/articles/methods.html); the essentials are
#' under `Details`.
#'
#' @param .data One row per stage: the intensity, the lactate at the end of the stage and, if recorded, the heart rate
#'   and the rating of perceived exertion (RPE). The resting sample, if any, is the row with intensity 0. Rows with a
#'   missing intensity or lactate are dropped with a warning.
#' @param intensity_column The name of the intensity column: bare, as a string, or a variable that holds the name.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if recorded. The heart rate at each threshold is
#'   interpolated linearly between the measured stages, and is `NA` outside them: a threshold there is doubtful
#'   anyway (see [lactate_checks()]). A zone boundary below the first stage, which is common for zone 1|2, is instead
#'   extrapolated (see [lactate_zones()]).
#' @param rpe_column The name of the RPE column, if recorded. The RPE at each threshold is interpolated like the heart
#'   rate. A heart rate or RPE column with no values, as when none was written on the recording form, is left out.
#' @param rpe_scale The RPE scale: `"6-20"` (Borg's RPE scale, default) or `"CR10"` (Borg's category-ratio scale,
#'   0 to 10, with ratings above 10 allowed for an absolute maximum).
#' @param method The method families to estimate, one or more of `"Log-log"`, `"OBLA"`, `"Bsln+"`, `"Dmax"`,
#'   `"LTP"`, `"LTratio"`, `"IAT"`, `"Keul"`, `"Simon"` and `"Stegmann"`. Defaults to all of them (`Keul` and `Simon`
#'   only for running, `Stegmann` only when `recovery` is given).
#' @param fit The curve fitted to the lactate values: `"3rd degree polynomial"` (default), `"4th degree polynomial"`
#'   or `"B-spline"` (a natural cubic regression spline with 4 degrees of freedom; with 6 stages it nearly passes
#'   through every point). OBLA, Bsln+ and Stegmann read their thresholds off this curve; the other methods fit their
#'   own, as their papers did (see `Details`). [lactate_fit_quality()] compares the curves on your data.
#' @param include_baseline Whether the resting sample is part of the curve fitted with `fit` (used by OBLA, Bsln+,
#'   LTratio, IAT and Stegmann). In the fit it is placed one median step before the first stage. Log-log, LTP and the
#'   Dmax family fit the exercise stages only. Defaults to `FALSE`.
#' @param sport One of `"cycling"` (intensity in W), `"running"` (km/h), `"swimming"` (m/s) or `"rowing"` (W). For
#'   running, swimming and rowing a `pace` column is added: s/km, s/100 m, and the Concept2 split in s/500 m
#'   (`watts = 2.80 / pace^3`, pace in s/m).
#' @param loglog_restrainer Deprecated. Log-log uses all stages, as Beaver et al. (1985) did. A value below `1` still
#'   keeps only that share of the first stages, with a warning; it will be removed in a future version.
#' @param plot Whether to draw the plot of each method into a `plot` column. Defaults to `TRUE`.
#' @param dmax_anchor Where the ends of the Dmax lines sit: `"original"` (default) as in each variant's paper,
#'   `"fitted"` on the fitted curve, `"observed"` at the measured lactate of the start and end stages. See `Details`.
#' @param moddmax_rise The rise in lactate (mmol/L) between two consecutive stages that marks the first rise in
#'   ModDmax. Defaults to `0.4` (Bishop et al., 1998, who measured fingertip plasma lactate); change it for other
#'   sample types such as sweat.
#' @param ltp_fix_first_slope Whether the first LTP segment (before LTP1) is flat instead of estimated. Defaults to
#'   `FALSE`.
#' @param dmax_methods Which Dmax variants to estimate when `method` includes `"Dmax"`: one or more of `"Dmax"`,
#'   `"ModDmax"`, `"Exp-Dmax"`, `"LTP1-Poly-ModDmax"`, `"Log-Poly-ModDmax"` and `"Log-Exp-ModDmax"`. Defaults to all
#'   but `"LTP1-Poly-ModDmax"` (a line from LTP1 instead of Log-log; not part of Jamnick et al., 2018).
#' @param fixed The fixed lactate concentrations (mmol/L) for OBLA. Defaults to `c(2, 2.5, 3, 3.5, 4)`.
#' @param baseline_plus The rises above baseline (mmol/L) for Bsln+. Defaults to `c(0.5, 1, 1.5)`.
#' @param iat_rise The rise (mmol/L) above the lactate at LTratio that defines IAT. Defaults to `1.5` (Dickhuth et
#'   al., 1999).
#' @param ltp_breakpoints The number of lactate turn points to fit: `2` (default) for a test to exhaustion, or `1`
#'   for a submaximal test that stopped before the second turn point (LTP1 only; LTP2 is `NA`).
#' @param recovery The lactate samples taken after the end of the test, for Stegmann: a data frame with the columns
#'   `time` (minutes after the end of exercise) and `lactate`. Sample until lactate has fallen back below its
#'   end-of-exercise value; Stegmann et al. (1981) sampled at 1, 2, 5 and 10 min.
#' @param stage_duration The duration of each stage, in minutes, or `NULL`. Stegmann needs it to put the recovery
#'   minutes on the intensity axis, and [predict_performance()], [reference_percentiles()] and [compare_tests()] check
#'   it against their protocols (a different stage length moves the thresholds).
#' @param last_stage_duration The minutes completed in the last stage when the test ended before the stage did, or
#'   `NULL` (the last stage was completed). The intensity of the last stage is then time-weighted: the stage before it
#'   plus the completed share of the increment (Kuipers et al., 2003), so 2 of 3 min at 200 W after 175 W count as
#'   191.7 W. Needs `stage_duration`. The lactate of a short last stage had less time to rise; `max_intensity` leaves
#'   the stage out instead.
#' @param site Where the blood was sampled: `"earlobe"`, `"fingertip"`, `"venous"`, `"other"`, or `NULL` (not
#'   recorded). It does not change the calculations; [predict_performance()], [reference_percentiles()] and
#'   [compare_tests()] check it against their protocols.
#' @param grade The treadmill grade (%) of a running test, or `NULL`. Checked like `site`.
#' @param max_intensity Stages above this intensity are left out of every method (an incomplete last stage, or one
#'   where lactate plateaus). Defaults to `NULL` (keep all stages).
#' @param sample The sample the lactate was measured in: `"blood"` (default), `"plasma"`, `"sweat"` or
#'   `"interstitial fluid"`. It does not change the calculations; [lactate_checks()] warns when a method relies on
#'   fixed blood lactate concentrations that do not transfer to the sample.
#' @param ci Whether to add bootstrap confidence intervals for the intensities (`intensity_low`, `intensity_high`).
#'   See `Details`. Defaults to `FALSE`.
#' @param n_boot Number of bootstrap replicates when `ci = TRUE`. Defaults to `500`.
#' @param ci_level Confidence level when `ci = TRUE`. Defaults to `0.95`.
#' @param seed A seed for the bootstrap replicates, so that the intervals are reproducible, or `NULL` to draw them
#'   from the current random stream (then call `set.seed()` first for reproducibility).
#' @param lt1,lt2 The thresholds that mark LT1 and LT2 for this athlete: a method (e.g. `"Log-log"`, `"LTP2"`) or an
#'   intensity. Chosen once here, they are what [lactate_zones()], [test_report()], the overview plot and the header
#'   of [compare_tests()] and [predict_performance()] use. Default to `"Bsln + 0.5"` and `"Log-Poly-ModDmax"`; see
#'   [lactate_zones()] for why.
#' @param .by Names of columns that identify separate tests (`"athlete"` or `athlete`, or `c("athlete", "date")`).
#'   Each test is analyzed on its own and the results are bound together, with the `.by` columns first.
#'
#' @return a `lactate_thresholds` tibble (plot it with [autoplot()] or [plot()]) with one row per threshold:
#' \item{method_category}{the method family.}
#' \item{method}{the method.}
#' \item{fitting}{the curve the method used.}
#' \item{intensity}{the intensity at the threshold, in whole watts, km/h to 0.1 or m/s to 0.01; `NA` when the method
#'   found no threshold in the tested range.}
#' \item{intensity_low, intensity_high}{with `ci = TRUE`, the confidence interval of the intensity.}
#' \item{pace}{for running, swimming and rowing, the pace at the threshold.}
#' \item{lactate}{the lactate at the threshold.}
#' \item{heart_rate}{the heart rate at the threshold, when `heart_rate_column` is given.}
#' \item{rpe}{the RPE at the threshold, when `rpe_column` is given.}
#' \item{plot}{the plot of the method, when `plot = TRUE`.}
#' With `.by`, the columns that identify each test come first.
#'
#' @details
#'
#' ## Log-log
#'
#' Two straight lines through log(lactate) against log(intensity), split at a measured stage that belongs to both
#' lines, at the split with "the lowest residuals sum of squares" (Beaver et al., 1985; Jamnick et al., 2018), with
#' at least three stages per line, so at least five stages. The threshold is where the lines cross, and the lactate
#' is read off the lines; it is `NA` when they cross outside the measured stages. Beaver et al. plotted lactate
#' against oxygen uptake; lactater uses the intensity.
#'
#' ## OBLA
#'
#' The intensity at which the fitted curve reaches each concentration in `fixed`, 2.0 to 4.0 mmol/L by default
#' (Heck et al., 1985; Kindermann et al., 1979; Skinner & McLellan, 1980), solved exactly. When the curve crosses a
#' target more than once (an early lactate dip), the last crossing where lactate rises through it is used: from there
#' on, lactate stays above the target. `NA` when the curve never reaches it.
#'
#' ## Bsln+
#'
#' The intensity at which the fitted curve is each rise in `baseline_plus` above the baseline (Berg et al., 1990;
#' Zoladz et al., 1995), solved like OBLA. The baseline is one sample: the resting sample when there is one, otherwise
#' the first stage. Studies differ here: Heuberger et al. (2018) used the mean of the samples before lactate rose, and
#' Jamnick et al. (2018) wrote only "baseline value(s)". One sample carries the analyzer error, about 0.1 mmol/L at
#' low concentrations (Bonaventura et al., 2015): on `demo_data`, a baseline 0.1 mmol/L lower or higher moves
#' Bsln + 0.5 from 83 W to 77 or 88 W. [lactate_diagnostics()] shows how much each threshold rests on the baseline
#' sample, and [lactate_checks()] says when the LT1 methods disagree.
#'
#' ## The Dmax family
#'
#' Every variant takes the point of its curve that lies furthest below a straight line ending at the last stage (the
#' perpendicular distance peaks at the same point as the vertical one). The variants differ in the curve, which `fit`
#' does not change, and in where the line starts:
#'
#' * Dmax: a 3rd degree polynomial, from the first stage (Cheng et al., 1992).
#' * ModDmax: the polynomial, from the stage preceding the first rise of at least `moddmax_rise` (Bishop et al., 1998).
#'   Bishop et al. and Jamnick et al. (2018) wrote a rise of "> 0.4"; lactater counts a rise of exactly 0.4 too, as
#'   Heuberger et al. (2018) did.
#' * Exp-Dmax: an exponential plus constant, from the first stage (Hughson et al., 1987).
#' * Log-Poly-ModDmax and Log-Exp-ModDmax: the polynomial or the exponential, from the Log-log threshold, computed
#'   without the resting sample (Jamnick et al., 2018).
#' * LTP1-Poly-ModDmax: the polynomial, from LTP1 (only when requested in `dmax_methods`).
#'
#' With `dmax_anchor = "original"` (default) the ends of the line sit where each paper put them: on the fitted curve
#' for Dmax and Exp-Dmax ("the two end points of the curve", Cheng et al., 1992), and at the measured lactate for
#' ModDmax ("the LT and the final lactate point", Bishop et al., 1998) and for the end of the Log-log variants ("the
#' final lactate point", Jamnick et al., 2018). `"fitted"` puts every end on the curve and `"observed"` every end at
#' the measured values (Heuberger et al., 2018); a line starting at the Log-log threshold or LTP1 always starts on the
#' curve. `NA` when the curve never dips below the line. The resting sample is never used.
#'
#' ## LTP
#'
#' Three connected straight segments through the exercise stages, with the two breakpoints found by exhaustive search
#' for the lowest residual sum of squares, at least two stages per segment (so at least six stages; four with
#' `ltp_breakpoints = 1`, which fits LTP1 only). `ltp_fix_first_slope = TRUE` keeps the first segment flat. LTP1 is
#' the first rise of lactate above resting levels and LTP2 the second, accelerated rise (Hofmann et al., 1997; Pokan
#' et al., 1997; Hofmann & Tschakert, 2017). The original papers fitted each turn point in its own window, on tests
#' with 1-min or 90-s stages; with the 6 to 10 stages of a step test the windows hold too few stages, so lactater fits
#' the three segments together. The fit has 6 parameters, so with 6 or 7 stages a single stage can move the turn
#' points a lot: [lactate_checks()] says so, and [lactate_diagnostics()] shows by how much.
#'
#' ## LTratio and IAT
#'
#' LTratio is the intensity where lactate / intensity is lowest on a B-spline through the stages (Dickhuth et al.,
#' 1999). IAT, the individual anaerobic threshold of Dickhuth et al. (1999), is the intensity `iat_rise` (1.5 mmol/L)
#' above the lactate at LTratio, on the same spline (as described by Heuberger et al., 2018). Like OBLA and Bsln+, IAT
#' uses an absolute lactate step, so it assumes blood lactate.
#'
#' ## Keul and Simon
#'
#' The individual anaerobic thresholds of Keul et al. (1979) and Simon et al. (1981), for running only: the speed at
#' which a 3rd degree polynomial through the exercise stages rises by 1.26 mmol/L per km/h (Keul; a tangent at
#' 51°34') or by 1 mmol/L per km/h (Simon; 45°). Keul et al. took the mean slope at 4 mmol/L of 60 tests of 20
#' cross-country skiers; Simon et al. took 45° as the point of the largest change in slope. Both tested on a treadmill
#' at a 5% grade with 3-min stages of 2 km/h, sampling earlobe blood, and set the slope on that protocol. A slope in
#' watts or m/s has no counterpart in these papers, so the methods run only with `sport = "running"`. When the slope
#' reaches the value more than once, the last crossing where it rises through it is used; `NA` when it never does
#' within the stages.
#'
#' ## Stegmann
#'
#' The individual anaerobic threshold of Stegmann et al. (1981). After the test, blood lactate first rises and then
#' falls. From the point in recovery where it is back at its end-of-exercise value, a tangent is drawn to the lactate
#' curve of the test; the point of contact is the threshold, and the slope of the tangent the maximal rate of lactate
#' elimination. Stegmann et al. plotted lactate against time, so lactater puts the recovery minutes on the intensity
#' axis at the rate the test rose (the median increment between stages divided by `stage_duration`), interpolates
#' the return to the end-of-exercise value between the recovery samples after the post-exercise peak, and draws the
#' tangent to the curve fitted with `fit`. When lactate never falls back to its end value, or never rises after the
#' test, the threshold is `NA`.
#'
#' ## Bootstrap intervals
#'
#' With `ci = TRUE`, each threshold gets an approximate percentile bootstrap interval for the noise of the samples.
#' Each replicate keeps the curve fitted to the data and gives every stage its own residual with a random sign (a
#' wild bootstrap; Wu, 1986; Davidson & Flachaire, 2008), because the error of a lactate sample grows with its
#' concentration; the residuals are scaled for their leverage and widened for the few degrees of freedom a step test
#' leaves. Every method is then run again on each replicate, and the bounds are rounded outward. Replicates on which
#' a method fails are left out, with a warning. The interval does not include the bias of the fitted curve, errors
#' that are the same in every sample (analyzer bias, sampling site), or the day-to-day variation of the athlete,
#' which is larger (about 3.5-4% of the intensity; Heuberger et al., 2018; see [compare_tests()]). In simulated tests,
#' 95% intervals held the true threshold in 84% to 97% of tests, depending on the method and the analyzer error; how
#' they were calibrated is in [Checking the analysis](https://fmmattioni.github.io/lactater/articles/checking.html).
#'
#' @references
#'
#' Beaver WL, Wasserman K, Whipp BJ. Improved detection of lactate threshold during exercise using a log-log
#' transformation.
#' Journal of Applied Physiology. 1985;59(6):1936–40.
#'
#' Heck H, Mader A, Hess G, Mücke S, Müller R, Hollmann W. Justification of the 4-mmol/l Lactate Threshold.
#' International Journal of Sports Medicine. 1985;06(03):117–30.
#'
#' Kindermann W, Simon G, Keul J. The significance of the aerobic-anaerobic transition for the determination of work
#' load
#' intensities during endurance training. European Journal of Applied Physiology and Occupational Physiology.
#' 1979;42(1):25–34.
#'
#' Skinner JS, Mclellan TH. The Transition from Aerobic to Anaerobic Metabolism. Research Quarterly for Exercise and
#' Sport. 1980;51(1):234–48.
#'
#' Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ. Reliability and accuracy of six hand-held blood
#' lactate analysers. Journal of Sports Science and Medicine. 2015;14:203-214.
#'
#' Berg A, Jakob E, Lehmann M, Dickhuth HH, Huber G, Keul J. Current aspects of modern ergometry. Pneumologie.
#' 1990;44(1):2–13.
#'
#' Zoladz JA, Rademaker AC, Sargeant AJ. Non-linear relationship between O2 uptake and power output at high intensities
#' of
#' exercise in humans. The Journal of Physiology. 1995;488(1):211–7.
#'
#' Cheng B, Kuipers H, Snyder A, Keizer H, Jeukendrup A, Hesselink M. A New Approach for the Determination of
#' Ventilatory and
#' Lactate Thresholds. International Journal of Sports Medicine. 1992;13(07):518–22.
#'
#' Bishop D, Jenkins DG, Mackinnon LT. The relationship between plasma lactate parameters, Wpeak and 1-h cycling
#' performance
#' in women. Med Sci Sports Exerc. 1998;30(8):1270–5.
#'
#' Hughson RL, Weisiger KH, Swanson GD. Blood lactate concentration increases as a continuous function in progressive
#' exercise.
#' Journal of Applied Physiology. 1987;62(5):1975–81.
#'
#' Jamnick NA, Botella J, Pyne DB, Bishop DJ. Manipulating graded exercise test variables affects the validity of the
#' lactate
#' threshold and VO2peak. PLOS ONE. 2018;13(7):e0199794.
#'
#' Davidson R, Flachaire E. The wild bootstrap, tamed at last. Journal of Econometrics. 2008;146(1):162-169.
#' doi:10.1016/j.jeconom.2008.08.003
#'
#' Wu CFJ. Jackknife, bootstrap and other resampling methods in regression analysis. The Annals of Statistics.
#' 1986;14(4):1261-1295. doi:10.1214/aos/1176350142
#'
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and predictive
#' value
#' of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#'
#' Hofmann P, Tschakert G. Intensity- and Duration-Based Options to Regulate Endurance Training. Front Physiol.
#' 2017;8:337.
#'
#' Hofmann P, Pokan R, von Duvillard SP, Seibert FJ, Zweiker R, Schmid P. Heart rate performance curve during
#' incremental
#' cycle ergometer exercise in healthy young male subjects. Med Sci Sports Exerc. 1997;29(6):762–8.
#'
#' Pokan R, Hofmann P, Von Duvillard SP, et al. Left ventricular function in response to the transition from aerobic to
#' anaerobic metabolism. Med Sci Sports Exerc. 1997;29(8):1040–7.
#'
#' Dickhuth H-H, Yin L, Niess A, et al. Ventilatory, Lactate-Derived and Catecholamine Thresholds During Incremental
#' Treadmill Running: Relationship and Reproducibility. International Journal of Sports Medicine. 1999;20(02):122–7.
#'
#' Kuipers H, Rietjens G, Verstappen F, Schoenmakers H, Hofman G. Effects of stage duration in incremental running
#' tests on physiological variables. International Journal of Sports Medicine. 2003;24(7):486–91.
#' doi:10.1055/s-2003-42020
#'
#' Stegmann H, Kindermann W, Schnabel A. Lactate kinetics and individual anaerobic threshold. International Journal
#' of Sports Medicine. 1981;2(3):160–5. \doi{10.1055/s-2008-1034604}
#'
#' Keul J, Simon G, Berg A, Dickhuth HH, Goerttler I, Kübel R. Bestimmung der individuellen anaeroben Schwelle zur
#' Leistungsbewertung und Trainingsgestaltung. Deutsche Zeitschrift für Sportmedizin. 1979;30(7):212–8.
#'
#' Simon G, Berg A, Dickhuth HH, Simon-Alt A, Keul J. Bestimmung der anaeroben Schwelle in Abhängigkeit vom Alter und
#' von der Leistungsfähigkeit. Deutsche Zeitschrift für Sportmedizin. 1981;32(1):7–14.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' results
#' lactate_checks(results)
#' autoplot(results, method = "Dmax")
lactate_threshold <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column = NULL,
  rpe_column = NULL,
  rpe_scale = c("6-20", "CR10"),
  method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio", "IAT", "Keul", "Simon", "Stegmann"),
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming", "rowing"),
  loglog_restrainer = 1,
  plot = TRUE,
  dmax_anchor = c("original", "fitted", "observed"),
  moddmax_rise = 0.4,
  ltp_fix_first_slope = FALSE,
  dmax_methods = c("Dmax", "ModDmax", "Exp-Dmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax"),
  fixed = c(2, 2.5, 3, 3.5, 4),
  baseline_plus = c(0.5, 1, 1.5),
  iat_rise = 1.5,
  ltp_breakpoints = 2,
  recovery = NULL,
  stage_duration = NULL,
  last_stage_duration = NULL,
  site = NULL,
  grade = NULL,
  max_intensity = NULL,
  sample = c("blood", "plasma", "sweat", "interstitial fluid"),
  ci = FALSE,
  n_boot = 500,
  ci_level = 0.95,
  seed = NULL,
  lt1 = "Bsln + 0.5",
  lt2 = "Log-Poly-ModDmax",
  .by = NULL
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)
  method_given <- !missing(method)

  ## before the argument is touched, or missing() is always FALSE
  restrainer_given <- !missing(loglog_restrainer)
  if(restrainer_given)
    warning(
      "`loglog_restrainer` is deprecated and will be removed: Log-log uses all stages, as Beaver et al. (1985) did.",
      call. = FALSE
    )
  ## make sure 'loglog_restrainer' is numeric
  loglog_restrainer <- as.numeric(loglog_restrainer)
  if(length(loglog_restrainer) != 1 || is.na(loglog_restrainer))
    stop("`loglog_restrainer` must be a single numeric value between 0 and 1.", call. = FALSE)

  if(loglog_restrainer < 0 || loglog_restrainer > 1)
    stop("You can only choose a value between 0 and 1.", call. = FALSE)
  positive <- function(x) is.numeric(x) && length(x) >= 1 && !anyNA(x) && all(x > 0)
  if(!positive(fixed)) stop("`fixed` must be one or more positive lactate values (mmol/L).", call. = FALSE)
  if(!positive(baseline_plus)) stop("`baseline_plus` must be one or more positive rises (mmol/L).", call. = FALSE)
  if(!positive(iat_rise) || length(iat_rise) != 1)
    stop("`iat_rise` must be a single positive number (mmol/L).", call. = FALSE)
  if(!identical(as.numeric(ltp_breakpoints), 1) && !identical(as.numeric(ltp_breakpoints), 2))
    stop("`ltp_breakpoints` must be 1 or 2.", call. = FALSE)

  method <- match.arg(arg = method, several.ok = TRUE)
  if("Stegmann" %in% method && is.null(recovery)) {
    if(method_given)
      stop("`Stegmann` needs `recovery`: the lactate samples after the end of the test.", call. = FALSE)
    method <- setdiff(method, "Stegmann")
  }
  if(!is.null(recovery)) recovery <- prepare_recovery(recovery, stage_duration)
  fit <- match.arg(arg = fit, several.ok = FALSE)
  sport <- match.arg(arg = sport, several.ok = FALSE)
  slopes <- intersect(c("Keul", "Simon"), method)
  if(length(slopes) > 0 && sport != "running") {
    if(method_given)
      stop(sprintf("`%s` %s defined on running speed in km/h: use `sport = \"running\"`.",
                   paste(slopes, collapse = "` and `"), if(length(slopes) > 1) "are" else "is"), call. = FALSE)
    method <- setdiff(method, slopes)
  }
  dmax_anchor <- match.arg(arg = dmax_anchor)
  if(!is.numeric(moddmax_rise) || length(moddmax_rise) != 1 || is.na(moddmax_rise) || moddmax_rise <= 0)
    stop("`moddmax_rise` must be a single positive number (mmol/L).", call. = FALSE)
  dmax_variants <- c("Dmax", "ModDmax", "Exp-Dmax", "LTP1-Poly-ModDmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax")
  if(length(dmax_methods) == 0 || !all(dmax_methods %in% dmax_variants))
    stop(sprintf("`dmax_methods` must be one or more of: %s.", paste(dmax_variants, collapse = ", ")), call. = FALSE)
  if(!is.null(max_intensity) && (!is.numeric(max_intensity) || length(max_intensity) != 1 || is.na(max_intensity)))
    stop("`max_intensity` must be `NULL` or a single number.", call. = FALSE)
  sample <- match.arg(arg = sample)
  rpe_scale <- match.arg(arg = rpe_scale)
  if(!is.null(stage_duration) &&
       (!is.numeric(stage_duration) || length(stage_duration) != 1 || is.na(stage_duration) || stage_duration <= 0))
    stop("`stage_duration` must be `NULL` or a single positive number of minutes.", call. = FALSE)
  if(!is.null(last_stage_duration)) {
    if(is.null(stage_duration)) stop("`last_stage_duration` needs `stage_duration`.", call. = FALSE)
    if(!is.numeric(last_stage_duration) || length(last_stage_duration) != 1 || is.na(last_stage_duration) ||
         last_stage_duration <= 0 || last_stage_duration > stage_duration)
      stop("`last_stage_duration` must be `NULL` or the minutes completed in the last stage, up to `stage_duration`.",
           call. = FALSE)
  }
  sites <- c("earlobe", "fingertip", "venous", "other")
  if(!is.null(site) && (!is.character(site) || length(site) != 1 || !site %in% sites))
    stop(sprintf("`site` must be `NULL` or one of: %s.", paste(sites, collapse = ", ")), call. = FALSE)
  if(!is.null(grade) && (!is.numeric(grade) || length(grade) != 1 || is.na(grade)))
    stop("`grade` must be `NULL` or a single number (%).", call. = FALSE)
  if(!is.logical(ci) || length(ci) != 1 || is.na(ci))
    stop("`ci` must be `TRUE` or `FALSE`.", call. = FALSE)
  bad_n_boot <- !is.numeric(n_boot) || length(n_boot) != 1 || n_boot < 20
  bad_level <- !is.numeric(ci_level) || length(ci_level) != 1 || ci_level <= 0 || ci_level >= 1
  if(ci && (bad_n_boot || bad_level))
    stop("`n_boot` must be a number >= 20 and `ci_level` a number between 0 and 1.", call. = FALSE)
  if(!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)))
    stop("`seed` must be `NULL` or a single number.", call. = FALSE)

  one_marker <- function(x) length(x) == 1 && !is.na(x) && (is.character(x) || is.numeric(x))
  if(!one_marker(lt1) || !one_marker(lt2))
    stop("`lt1` and `lt2` must each be one method or one intensity.", call. = FALSE)

  ## column names work quoted, unquoted, or from a variable that holds the name
  caller <- parent.frame()
  columns <- list(
    intensity_column = column_name(rlang::enexpr(intensity_column), .data, caller),
    lactate_column = column_name(rlang::enexpr(lactate_column), .data, caller),
    heart_rate_column = column_name(rlang::enexpr(heart_rate_column), .data, caller),
    rpe_column = column_name(rlang::enexpr(rpe_column), .data, caller)
  )

  ## a bare column name works too
  by_expression <- rlang::enexpr(.by)
  if(is.symbol(by_expression) && rlang::as_name(by_expression) %in% names(.data)) .by <- rlang::as_name(by_expression)
  if(!is.null(.by)) {
    if(!is.null(recovery)) stop("`recovery` works with one test at a time, not with `.by`.", call. = FALSE)
    if(!is.null(last_stage_duration))
      stop("`last_stage_duration` works with one test at a time, not with `.by`.", call. = FALSE)
    arguments <- c(columns, list(
      method = method, fit = fit, include_baseline = include_baseline, sport = sport,
      plot = plot, dmax_anchor = dmax_anchor,
      moddmax_rise = moddmax_rise, ltp_fix_first_slope = ltp_fix_first_slope, dmax_methods = dmax_methods,
      fixed = fixed, baseline_plus = baseline_plus, iat_rise = iat_rise, ltp_breakpoints = ltp_breakpoints,
      max_intensity = max_intensity, sample = sample, ci = ci, n_boot = n_boot, ci_level = ci_level, seed = seed,
      rpe_scale = rpe_scale, stage_duration = stage_duration, site = site, grade = grade, lt1 = lt1, lt2 = lt2
    ))
    if(restrainer_given) arguments$loglog_restrainer <- loglog_restrainer
    return(lactate_threshold_by(.data, .by, arguments))
  }

  data <- do.call(prepare_data, c(list(.data = .data, rpe_scale = rpe_scale), columns))
  if(!is.null(last_stage_duration)) {
    steps <- utils::tail(diff(data$intensity), 2)
    if(length(steps) == 2 && steps[2] < steps[1] && last_stage_duration < stage_duration)
      warning("The last step is already smaller than the one before: is its intensity time-weighted already?",
              call. = FALSE)
    data$intensity <- weight_last_stage(data$intensity, last_stage_duration / stage_duration)
  }
  if(!is.null(max_intensity)) {
    ## the recovery times count from the real end of the test, so its last stage must stay
    if(!is.null(recovery) && any(data$intensity > max_intensity))
      stop("`recovery` is timed from the end of the test, so `max_intensity` cannot leave out its last stages.",
           call. = FALSE)
    data <- data[data$intensity <= max_intensity, ]
    if(sum(data$intensity > 0) < 3)
      stop("Fewer than 3 stages are left at or below `max_intensity`.", call. = FALSE)
  }

  settings <- list(
    fit = fit,
    include_baseline = include_baseline,
    sport = sport,
    loglog_restrainer = loglog_restrainer,
    dmax_anchor = dmax_anchor,
    moddmax_rise = moddmax_rise,
    ltp_fix_first_slope = ltp_fix_first_slope,
    dmax_methods = dmax_variants[dmax_variants %in% dmax_methods],
    fixed = sort(unique(fixed)),
    baseline_plus = sort(unique(baseline_plus)),
    iat_rise = iat_rise,
    ltp_breakpoints = as.integer(ltp_breakpoints),
    sample = sample,
    rpe_scale = rpe_scale,
    ci_level = ci_level,
    recovery = recovery,
    stage_duration = stage_duration,
    last_stage_duration = last_stage_duration,
    site = site,
    grade = grade,
    lt1 = lt1,
    lt2 = lt2
  )
  computed <- compute_thresholds(data, settings, method)
  curve <- computed$curve
  rows <- computed$rows
  plot_data <- computed$plot_data
  categories <- method_categories

  out <- tibble::tibble(
    method_category = factor(rows$method_category, levels = categories),
    method = factor(rows$method, levels = unique(rows$method)),
    fitting = rows$fitting,
    intensity = round(rows$intensity, digits = intensity_digits(sport))
  )
  intervals <- NULL
  if(ci) {
    intervals <- bootstrap_intervals(data, settings, method, curve, nrow(rows), n_boot, ci_level, seed)
    if(!is.null(intervals)) {
      ## a threshold that was not found has no interval, even if some replicates found one
      intervals[is.na(rows$intensity), ] <- NA_real_
      ## rounded outward, so the rounded interval still holds the unrounded one
      scale <- 10^intensity_digits(sport)
      out$intensity_low <- floor(intervals[, "low"] * scale + 1e-9) / scale
      out$intensity_high <- ceiling(intervals[, "high"] * scale - 1e-9) / scale
    }
  }
  if(sport != "cycling") out$pace <- convert_to_pace(rows$intensity, sport)
  out$lactate <- rows$lactate
  if("heart_rate" %in% names(data)) out$heart_rate <- retrieve_heart_rate(data, rows$intensity)
  if("rpe" %in% names(data)) out$rpe <- retrieve_rpe(data, rows$intensity)

  if(plot) {
    heart_rate <- if("heart_rate" %in% names(out)) out$heart_rate else rep(NA_real_, nrow(out))
    rpe <- if("rpe" %in% names(out)) out$rpe else rep(NA_real_, nrow(out))
    out$plot <- lapply(seq_len(nrow(out)), function(i) {
      plot_threshold(
        method = as.character(out$method[i]),
        intensity = rows$intensity[i],
        lactate = out$lactate[i],
        heart_rate = heart_rate[i],
        rpe = rpe[i],
        plot_data = plot_data[[i]],
        data = data,
        settings = settings,
        interval = if(!is.null(intervals)) intervals[i, ]
      )
    })
  }

  new_lactate_thresholds(
    out,
    data = data, curve = curve, settings = settings, plot_data = plot_data,
    intensity = rows$intensity, intervals = intervals
  )
}

#' Lactate thresholds by group
#'
#' Runs [lactate_threshold()] once per group and binds the results, with the group columns first.
#' For internal use only.
#'
#' @param .data The raw data.
#' @param .by The grouping column names.
#' @param arguments The other [lactate_threshold()] arguments.
#'
#' @keywords internal
lactate_threshold_by <- function(.data, .by, arguments) {
  if(!is.character(.by) || !all(.by %in% names(.data)))
    stop("`.by` must name one or more columns of the data.", call. = FALSE)
  groups <- split(.data, .data[.by], drop = TRUE, sep = "\r")
  results <- lapply(groups, function(group) {
    result <- do.call(lactate_threshold, c(list(.data = group), arguments))
    keys <- group[rep(1, nrow(result)), .by, drop = FALSE]
    vctrs::vec_cbind(tibble::as_tibble(keys), tibble::as_tibble(unclass_thresholds(result)))
  })
  vctrs::vec_rbind(!!!unname(results))
}

#' Column name from a bare name, a string, or a variable that holds a string
#'
#' @param expr The captured argument.
#' @param .data The data.
#' @param env Where to look up a variable.
#' @return the column name, or `NULL`.
#' @keywords internal
column_name <- function(expr, .data, env) {
  if(is.null(expr) || is.character(expr)) return(expr)
  if(is.symbol(expr) && rlang::as_name(expr) %in% names(.data)) return(rlang::as_name(expr))
  value <- tryCatch(eval(expr, env), error = function(e) if(is.symbol(expr)) rlang::as_name(expr) else stop(e))
  if(!is.null(value) && (!is.character(value) || length(value) != 1))
    stop("Column arguments must be a column name or a single string.", call. = FALSE)
  value
}

unclass_thresholds <- function(x) {
  attr(x, "lactater") <- NULL
  class(x) <- setdiff(class(x), "lactate_thresholds")
  x
}
