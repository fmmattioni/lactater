#' Lactate threshold
#'
#' This is a general function that applies several lactate threshold methods at the same time.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if applicable. The heart rate at each threshold is
#' interpolated linearly between the measured stages.
#' @param rpe_column The name of the rating of perceived exertion (RPE) column, if recorded. The RPE at each threshold
#' is interpolated between the measured stages, like the heart rate. A heart rate or RPE column with no values, as
#' when none was written on the recording form, is left out.
#' @param rpe_scale The RPE scale: `"6-20"` (Borg's RPE scale, default) or `"CR10"` (Borg's category-ratio scale,
#' 0 to 10, with ratings above 10 allowed for an absolute maximum).
#' @param method The lactate threshold method to calculate. It can be one or many of the following:
#' `Log-log`, `OBLA`, `Bsln+`, `Dmax`, `LTP`, `LTratio`, `IAT`, `Stegmann`. See `Details` for more information.
#' Default to all of them (`Stegmann` only when `recovery` is given).
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate
#' thresholds.
#' Please, note that a few lactate thresholds have default methods for this and cannot be changed.
#' Options are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline` (a natural cubic regression spline with 4
#' degrees of freedom, built on a B-spline basis; with 6 stages it nearly passes through every point). See `Details`.
#' @param include_baseline A boolean to indicate whether to include the resting (baseline) sample in the fitted
#' curve (used by OBLA, Bsln+, LTratio, IAT and Stegmann). The resting sample is the row with intensity 0; in the fit
#' it is placed one median step before the first stage. Log-log, LTP and the Dmax family fit the exercise stages only,
#' so they never use it. Data without a resting sample is fine. Rows with a missing intensity or lactate are dropped
#' with a warning.
#' @param sport The sport at which the incremental test was performed. One of `cycling` (intensity in W), `running`
#' (km/h), `swimming` (m/s), or `rowing` (W). For running, swimming and rowing a `pace` column is added: s/km,
#' s/100 m, and the Concept2 split in s/500 m (`watts = 2.80 / pace^3`, pace in s/m).
#' @param loglog_restrainer Deprecated. Log-log uses all stages, as Beaver et al. (1985) did. A
#' value below `1` still keeps only that share of the first stages, with a warning; it will be removed in a future
#' version.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods. Default to `TRUE`.
#' @param dmax_anchor Where the ends of the `Dmax` lines sit: `"original"` (default) as in each
#' variant's paper, `"fitted"` on the fitted curve, `"observed"` at the measured lactate of the start
#' and end stages. See `Details`.
#' @param ltp_fix_first_slope A boolean to indicate whether the first `LTP` segment (before LTP1) is flat
#' (slope fixed at zero) instead of estimated. Default to `FALSE`.
#' @param dmax_methods Which `Dmax` variants to calculate when `method` includes `Dmax`: one or more of
#' `"Dmax"`, `"ModDmax"`, `"Exp-Dmax"`, `"LTP1-Poly-ModDmax"`, `"Log-Poly-ModDmax"`, and `"Log-Exp-ModDmax"`.
#' Default to all but `"LTP1-Poly-ModDmax"` (a line from LTP1 instead of Log-log; not part of Jamnick et al., 2018).
#' @param max_intensity Stages above this intensity are left out of every method (e.g. an incomplete last
#' stage, or a last stage where lactate plateaus). Default to `NULL` (keep all stages).
#' @param sample The sample the lactate was measured in: `"blood"` (default), `"plasma"`, `"sweat"`, or
#' `"interstitial fluid"`. It does not change the calculations; [lactate_checks()] warns when a method relies on
#' fixed blood lactate concentrations that do not transfer to the sample.
#' @param ci A boolean to indicate whether to add bootstrap confidence intervals for the intensities
#' (`intensity_low`, `intensity_high`). See `Details`. Default to `FALSE`.
#' @param n_boot Number of bootstrap replicates when `ci = TRUE`. Default to `500`.
#' @param ci_level Confidence level when `ci = TRUE`. Default to `0.95`.
#' @param .by Names of columns that identify separate tests (e.g. `"athlete"` or `athlete`, or
#' `c("athlete", "date")`).
#' Each test is analyzed on its own and the results are bound together, with the `.by` columns first.
#' @param moddmax_rise The rise in lactate (mmol/L) between two consecutive stages that marks the
#' first rise in `ModDmax`. Default to `0.4` (Bishop et al., 1998, who measured fingertip plasma lactate);
#' change it for other sample types such as sweat.
#' @param fixed The fixed lactate concentrations (mmol/L) for `OBLA`. Default to `c(2, 2.5, 3, 3.5, 4)`.
#' @param baseline_plus The rises above baseline (mmol/L) for `Bsln+`. Default to `c(0.5, 1, 1.5)`.
#' @param iat_rise The rise (mmol/L) above the lactate at LTratio that defines `IAT`. Default to `1.5`
#' (Dickhuth et al., 1999).
#' @param ltp_breakpoints The number of lactate turn points to fit: `2` (default) for a test to exhaustion,
#' or `1` for a submaximal test that stopped before the second turn point (LTP1 only; LTP2 is `NA`).
#' @param recovery The lactate samples taken after the end of the test, for `Stegmann`: a data frame with the
#' columns `time` (minutes after the end of exercise) and `lactate`. Sample until lactate has fallen back below
#' its end-of-exercise value; Stegmann et al. (1981) sampled at 1, 2, 5 and 10 min.
#' @param stage_duration The duration of each stage, in minutes, or `NULL`. `Stegmann` needs it to put the recovery
#' minutes on the intensity axis, and [predict_performance()], [reference_percentiles()] and [compare_tests()] check it
#' against their protocols (a different stage length moves the thresholds).
#' @param site Where the blood was sampled: `"earlobe"`, `"fingertip"`, `"venous"`, `"other"`, or `NULL` (not
#' recorded). It does not change the calculations; [predict_performance()], [reference_percentiles()] and
#' [compare_tests()] check it against their protocols.
#' @param grade The treadmill grade (%) of a running test, or `NULL`. Checked like `site`.
#'
#' @return a `lactate_thresholds` tibble (plot it with [autoplot()] or [plot()]) with the following columns:
#' \item{method_category}{the category of the lactate threshold method.}
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{heart_rate}{the heart rate associated with the estimated lactate threshold}
#' \item{rpe}{the rating of perceived exertion at the threshold, when `rpe_column` is given}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @details
#'
#' ## Log-log
#'
#' The log of lactate vs the log of intensity is divided at a measured stage into two segments that both
#' include that stage, and each segment is fitted by its own straight line (Beaver et al., 1985). Beaver et al.
#' chose the division by eye; lactater takes the division with "the lowest residuals sum of squares"
#' (Jamnick et al., 2018), trying every stage, with at least three stages per line (so at least five stages).
#' The exercise intensity where the two lines cross is considered as Log-log; it is `NA` when they cross
#' outside the measured stages. Beaver et al. plotted log lactate against log oxygen uptake; lactater uses the log of
#' the exercise intensity, so the threshold is an intensity, not an oxygen uptake.
#' Caution: this method might require a double-check via a visual inspection, depending in some cases.
#'
#' ## OBLA
#'
#' The Onset of Blood Lactate Accumulation (OBLA) is the exercise intensity at fixed lactate of 2.0, 2.5,
#' 3.0, 3.5, and 4.0 mmol/L (Heck et al., 1985, Kindermann et al., 1979; Skinner & Mclellan, 1980). The lactate curve is
#' usually fitted using a 3rd order polynomial regression curve,
#' but the user can define another method (4th degree polynomial or B-spline).
#'
#' ## Bsln+
#'
#' In the baseline plus method (Bsln+), the exercise intensity at which lactate increases to 0.5, 1.0,
#' and 1.5 mmol/L above baseline (resting) values is considered (Berg et al., 1990; Zoladz et al., 1995). The lactate
#' curve is usually fitted using a
#' 3rd order polynomial regression curve,
#' but the user can define another method (4th degree polynomial or B-spline).
#'
#' The baseline is a single sample: the resting sample when there is one, otherwise the first stage. Studies differ
#' here: Heuberger et al. (2018) used the mean of the measurements before lactate rose, and Jamnick et al. (2018) wrote
#' only "baseline value(s)". One sample carries the analyzer error, about 0.1 mmol/L at low concentrations
#' (Bonaventura et al., 2015): on `demo_data`, a baseline 0.1 mmol/L lower or higher moves Bsln + 0.5 from 83 W to 77
#' or 88 W. [lactate_diagnostics()] shows how much each threshold rests on the baseline sample.
#'
#' ## Dmax
#'
#' All `Dmax` variants look for the point on the fitted curve furthest below a straight line that
#' ends at the last stage. With `dmax_anchor = "original"` (default) the ends sit where each paper put them:
#' on the fitted curve for Dmax and Exp-Dmax ("the two end points of the curve", Cheng et al., 1992), and at
#' the measured lactate for ModDmax ("the LT and the final lactate point", Bishop et al., 1998) and for the
#' end of the Log-log variants ("the final lactate point", Jamnick et al., 2018). `dmax_anchor = "fitted"`
#' puts every end on the curve and `"observed"` every end at the measured values (Heuberger et al., 2018).
#' A line starting at the Log-log threshold or LTP1 always starts on the curve. When the curve never dips
#' below the line, the result is `NA`. The resting sample is never used.
#'
#' ### Dmax
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the first and the last stage (Cheng et al., 1992). The lactate curve is fitted using a 3rd order polynomial
#' regression curve,
#' and it can't be changed.
#'
#' ### Modified Dmax (ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the stage preceding the first rise in lactate of at least `moddmax_rise` (0.4 mmol/L by default)
#' and the last stage (Bishop et al., 1998). Bishop et al. and Jamnick et al. (2018) wrote a rise of "> 0.4";
#' lactater counts a rise of exactly 0.4 too, as Heuberger et al. (2018) did. The lactate curve is fitted using a
#' 3rd order polynomial regression curve, and it can't be changed.
#'
#' ### Exponential Dmax (Exp-Dmax)
#'
#' The exercise intensity on the exponential plus-constant regression lactate curve that
#' yields the maximum perpendicular distance to the straight line between the first and the
#' last stage (Hughson et al., 1987). The lactate curve is fitted using an exponential curve,
#' and it can't be changed.
#'
#' ### Log-log modified Dmax (Log-Poly-ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the Log-log threshold and the last stage in the 3rd order polynomial regression curve (Jamnick et al., 2018).
#' The Log-log threshold used here is always computed without the resting sample.
#' The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
#'
#' ### Log-log exponential Dmax (Log-Exp-ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the Log-log threshold and the last stage in the exponential curve (Jamnick et al., 2018).
#' The lactate curve is fitted using an exponential curve,
#' and it can't be changed.
#'
#' ## LTP
#'
#' ### Lactate Turning Point 1 (LTP1) and Lactate Turning Point 2 (LTP2)
#'
#' The lactate response is divided into three continuous straight segments, fitted to the measured stages
#' by exhaustive search for the two breakpoints with the lowest residual sum of squares (at least two stages
#' per segment, so at least six stages are needed). With `ltp_fix_first_slope = TRUE` the first segment is
#' flat. The original papers fitted each turn point with its own two-line regression inside a window (LTP1
#' from the first stage up to 4 mmol/L, or up to 75% of the maximal intensity; LTP2 from LTP1 to the maximal
#' intensity), on tests with 1-min or 90-s stages. With the 6-8 stages of a typical step test those windows
#' hold too few stages, so lactater fits the three segments together. The first breaking point, representing
#' the first rise in lactate above resting levels, is considered as LTP1. The second breaking point,
#' representing an accelerated lactate accumulation, is then considered as LTP2
#' (Hofmann & Tschakert, 2017; Hofmann et al., 1997; Pokan et al., 1997).
#' Caution: this method might require a double-check via a visual inspection, depending in some cases.
#'
#' ## LTratio
#'
#' The lactate response (i.e., ratio of lactate / exercise intensity vs exercise intensity) is interpolated
#' using a B-spline regression curve. LTratio is then defined as the lowest value of the lactate / exercise intensity
#' ratio, which attempts to describe the onset of the lactate increase (Dickhuth et al., 1999).
#'
#' ## Bootstrap intervals
#'
#' With `ci = TRUE`, each threshold gets a percentile bootstrap interval for the noise of the samples. Each replicate
#' keeps the curve fitted to the data and gives every stage its own residual with a random sign (a wild bootstrap; Wu,
#' 1986; Davidson & Flachaire, 2008), because the error of a lactate sample grows with its concentration; the
#' residuals are scaled for their leverage and widened as a t quantile is over a normal one, for the few degrees of
#' freedom a step test leaves. Every method is then run again, and the bounds are rounded outward.
#'
#' In a simulation of 150 tests of 8 stages from a known curve, with an analyzer error of 0.1 mmol/L plus 4%, the 95%
#' intervals held the true threshold in 93% of tests for Bsln + 1.0, 94% for Exp-Dmax and 84% for OBLA 4.0; with a
#' constant error of 0.25 mmol/L, in 93%, 97% and 91% (`data-raw/bootstrap_coverage.R`). OBLA falls short because a
#' 3rd degree polynomial cannot follow the curve exactly (here 1.5 W too low on average), an error the interval does
#' not include. Nor does it include errors that are the same in every sample (analyzer bias, sampling site), or the
#' day-to-day variation of the athlete, which is larger (about 3.5-4% of the intensity; Heuberger et al., 2018; see
#' [compare_tests()]). Call `set.seed()` first to make the intervals reproducible.
#'
#' ## IAT
#'
#' The individual anaerobic threshold (IAT) is the intensity at which lactate is 1.5 mmol/L above the
#' lactate at LTratio, on the same B-spline curve (Dickhuth et al., 1999; as described by Heuberger et al., 2018).
#' Like OBLA and Bsln+, it uses an absolute lactate step, so it assumes blood lactate.
#'
#' ## Stegmann
#'
#' The individual anaerobic threshold of Stegmann et al. (1981). After the test, blood lactate first rises and then
#' falls. From the point in recovery where it is back at its end-of-exercise value, a tangent is drawn to the
#' lactate curve of the test; the point of contact is the threshold, and the slope of the tangent the maximal rate of
#' lactate elimination. Stegmann et al. plotted lactate against time, so lactater puts the recovery minutes on the
#' intensity axis at the rate the test rose: the median increment between stages divided by `stage_duration`. The
#' time lactate is back at its end-of-exercise value is interpolated linearly between the recovery samples after
#' the post-exercise peak; when lactate never falls back to it, or never rises after the test, the threshold is `NA`.
#' The tangent touches the curve fitted with `fit`.
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
#' Stegmann H, Kindermann W, Schnabel A. Lactate kinetics and individual anaerobic threshold. International Journal
#' of Sports Medicine. 1981;2(3):160–5. \doi{10.1055/s-2008-1034604}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lactate_threshold(
#'   .data = demo_data,
#'   intensity_column = "intensity",
#'   lactate_column = "lactate",
#'   heart_rate_column = "heart_rate",
#'   fit = "3rd degree polynomial",
#'   include_baseline = TRUE,
#'   sport = "cycling",
#'   plot = TRUE
#' )
#' }
lactate_threshold <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column = NULL,
  rpe_column = NULL,
  rpe_scale = c("6-20", "CR10"),
  method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio", "IAT", "Stegmann"),
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
  site = NULL,
  grade = NULL,
  max_intensity = NULL,
  sample = c("blood", "plasma", "sweat", "interstitial fluid"),
  ci = FALSE,
  n_boot = 500,
  ci_level = 0.95,
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
    arguments <- c(columns, list(
      method = method, fit = fit, include_baseline = include_baseline, sport = sport,
      plot = plot, dmax_anchor = dmax_anchor,
      moddmax_rise = moddmax_rise, ltp_fix_first_slope = ltp_fix_first_slope, dmax_methods = dmax_methods,
      fixed = fixed, baseline_plus = baseline_plus, iat_rise = iat_rise, ltp_breakpoints = ltp_breakpoints,
      max_intensity = max_intensity, sample = sample, ci = ci, n_boot = n_boot, ci_level = ci_level,
      rpe_scale = rpe_scale, stage_duration = stage_duration, site = site, grade = grade
    ))
    if(restrainer_given) arguments$loglog_restrainer <- loglog_restrainer
    return(lactate_threshold_by(.data, .by, arguments))
  }

  data <- do.call(prepare_data, c(list(.data = .data, rpe_scale = rpe_scale), columns))
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
    site = site,
    grade = grade
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
    intervals <- bootstrap_intervals(data, settings, method, curve, nrow(rows), n_boot, ci_level)
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
