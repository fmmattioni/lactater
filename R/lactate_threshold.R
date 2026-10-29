#' Lactate threshold
#'
#' This is a general function that applies several lactate threshold methods at the same time.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if applicable. The heart rate at each threshold is
#' interpolated linearly between the measured stages.
#' @param method The lactate threshold method to calculate. It can be one or many of the following:
#' `Log-log`, `OBLA`, `Bsln+`, `Dmax`, `LTP`, `LTratio`, `IAT`. See `Details` for more information.
#' Default to all of them.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' Please, note that a few lactate thresholds have default methods for this and cannot be changed.
#' Options are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline`. See `Details`.
#' @param include_baseline A boolean to indicate whether to include the resting (baseline) sample in the fit. The resting sample is
#' the row with intensity 0; it is placed one median step before the first stage. Data without a resting sample is fine.
#' Rows with a missing intensity or lactate are dropped with a warning.
#' @param sport The sport at which the incremental test was performed. One of `cycling` (intensity in W), `running`
#' (km/h), `swimming` (m/s), or `rowing` (W). For running, swimming and rowing a `pace` column is added: s/km,
#' s/100 m, and the Concept2 split in s/500 m (`watts = 2.80 / pace^3`, pace in s/m).
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods. Default to `TRUE`.
#' @param dmax_anchor Where the ends of the `Dmax` lines sit: `"fitted"` (default) puts them on the
#' fitted curve, `"observed"` at the measured lactate of the start and end stages. See `Details`.
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
#' @param .by Names of columns that identify separate tests (e.g. `"athlete"`, or `c("athlete", "date")`).
#' Each test is analysed on its own and the results are bound together, with the `.by` columns first.
#' @param moddmax_rise The rise in lactate (mmol/L) between two consecutive stages that marks the
#' first rise in `ModDmax`. Default to `0.4` (Bishop et al., 1998), a blood lactate value; change it
#' for other sample types such as sweat.
#'
#' @return a `lactate_thresholds` tibble (plot it with [autoplot()] or [plot()]) with the following columns:
#' \item{method_category}{the category of the lactate threshold method.}
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{heart_rate}{the heart rate associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @details
#'
#' ## Log-log
#'
#' The log of lactate vs the log of intensity is divided into two straight segments (Beaver et al., 1985).
#' The breakpoint is "the intersection point of the two lines with the lowest residuals sum of squares"
#' (Jamnick et al., 2018), found by exhaustive search over the measured stages, with at least two stages
#' in each segment. The exercise intensity at the breakpoint is considered as Log-log.
#' Caution: this method might require a double-check via a visual inspection, depending in some cases.
#'
#' ## OBLA
#'
#' The Onset of Blood Lactate Accumulation (OBLA) is the exercise intensity at fixed lactate of 2.0, 2.5,
#' 3.0, 3.5, and 4.0 mmol/L (Heck et al., 1985, Kindermann et al., 1979; Skinner & Mclellan, 1980). The lactate curve is usually fitted using a 3rd order polynomial regression curve,
#' but the user can define another method (4th degree polynomial or B-spline).
#'
#' ## Bsln+
#'
#' In the baseline plus method (Bsln+), the exercise intensity at which lactate increases to 0.5, 1.0,
#' and 1.5 mmol/L above baseline (resting) values is considered (Berg et al., 1990; Zoladz et al., 1995). The lactate curve is usually fitted using a
#' 3rd order polynomial regression curve,
#' but the user can define another method (4th degree polynomial or B-spline).
#'
#' ## Dmax
#'
#' All `Dmax` variants look for the point on the fitted curve furthest below a straight line that
#' ends at the last stage. With `dmax_anchor = "fitted"` (default) both ends of the line sit on the
#' fitted curve ("the two end points of the curve", Jamnick et al., 2018); with
#' `dmax_anchor = "observed"` they sit at the measured lactate values (Heuberger et al., 2018).
#' A line starting at the Log-log threshold always starts on the curve. When the curve never dips
#' below the line, the result is `NA`. The resting sample is never used.
#'
#' ### Dmax
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the first and the last stage (Cheng et al., 1992). The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
#'
#' ### Modified Dmax (ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the stage preceding the first rise in lactate of at least `moddmax_rise` (0.4 mmol/L by default)
#' and the last stage (Bishop et al., 1998). The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
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
#' flat. The first breaking point, representing
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
#' ## Confidence intervals
#'
#' With `ci = TRUE`, each threshold gets a percentile bootstrap interval. Each replicate keeps the curve fitted to
#' the data and adds residuals resampled with replacement (scaled by their leverage and centered; Davison &
#' Hinkley, 1997), then every method is run again. The interval shows how much a threshold moves with this much
#' scatter around the curve; it does not cover errors that are the same in every sample (analyser bias, sampling
#' site). This approach has not been validated specifically for lactate thresholds. Call `set.seed()` first to make
#' the intervals reproducible.
#'
#' ## IAT
#'
#' The individual anaerobic threshold (IAT) is the intensity at which lactate is 1.5 mmol/L above the
#' lactate at LTratio, on the same B-spline curve (Dickhuth et al., 1999; as described by Heuberger et al., 2018).
#' Like OBLA and Bsln+, it uses an absolute lactate step, so it assumes blood lactate.
#'
#' @references
#'
#' Beaver WL, Wasserman K, Whipp BJ. Improved detection of lactate threshold during exercise using a log-log transformation.
#' Journal of Applied Physiology. 1985;59(6):1936–40.
#'
#' Heck H, Mader A, Hess G, Mücke S, Müller R, Hollmann W. Justification of the 4-mmol/l Lactate Threshold.
#' International Journal of Sports Medicine. 1985;06(03):117–30.
#'
#' Kindermann W, Simon G, Keul J. The significance of the aerobic-anaerobic transition for the determination of work load
#' intensities during endurance training. European Journal of Applied Physiology and Occupational Physiology. 1979;42(1):25–34.
#'
#' Skinner JS, Mclellan TH. The Transition from Aerobic to Anaerobic Metabolism. Research Quarterly for Exercise and Sport. 1980;51(1):234–48.
#'
#' Berg A, Jakob E, Lehmann M, Dickhuth HH, Huber G, Keul J. Current aspects of modern ergometry. Pneumologie. 1990;44(1):2–13.
#'
#' Zoladz JA, Rademaker AC, Sargeant AJ. Non-linear relationship between O2 uptake and power output at high intensities of
#' exercise in humans. The Journal of Physiology. 1995;488(1):211–7.
#'
#' Cheng B, Kuipers H, Snyder A, Keizer H, Jeukendrup A, Hesselink M. A New Approach for the Determination of Ventilatory and
#' Lactate Thresholds. International Journal of Sports Medicine. 1992;13(07):518–22.
#'
#' Bishop D, Jenkins DG, Mackinnon LT. The relationship between plasma lactate parameters, Wpeak and 1-h cycling performance
#' in women. Med Sci Sports Exerc. 1998;30(8):1270–5.
#'
#' Hughson RL, Weisiger KH, Swanson GD. Blood lactate concentration increases as a continuous function in progressive exercise.
#' Journal of Applied Physiology. 1987;62(5):1975–81.
#'
#' Jamnick NA, Botella J, Pyne DB, Bishop DJ. Manipulating graded exercise test variables affects the validity of the lactate
#' threshold and VO2peak. PLOS ONE. 2018;13(7):e0199794.
#'
#' Davison AC, Hinkley DV. Bootstrap Methods and Their Application. Cambridge University Press; 1997.
#'
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and predictive value
#' of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#'
#' Hofmann P, Tschakert G. Intensity- and Duration-Based Options to Regulate Endurance Training. Front Physiol. 2017;8:337.
#'
#' Hofmann P, Pokan R, von Duvillard SP, Seibert FJ, Zweiker R, Schmid P. Heart rate performance curve during incremental
#' cycle ergometer exercise in healthy young male subjects. Med Sci Sports Exerc. 1997;29(6):762–8.
#'
#' Pokan R, Hofmann P, Von Duvillard SP, et al. Left ventricular function in response to the transition from aerobic to
#' anaerobic metabolism. Med Sci Sports Exerc. 1997;29(8):1040–7.
#'
#' Dickhuth H-H, Yin L, Niess A, et al. Ventilatory, Lactate-Derived and Catecholamine Thresholds During Incremental
#' Treadmill Running: Relationship and Reproducibility. International Journal of Sports Medicine. 1999;20(02):122–7.
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
#'   loglog_restrainer = 1,
#'   plot = TRUE
#' )
#' }
lactate_threshold <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column = NULL,
  method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio", "IAT"),
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming", "rowing"),
  loglog_restrainer = 1,
  plot = TRUE,
  dmax_anchor = c("fitted", "observed"),
  moddmax_rise = 0.4,
  ltp_fix_first_slope = FALSE,
  dmax_methods = c("Dmax", "ModDmax", "Exp-Dmax", "Log-Poly-ModDmax", "Log-Exp-ModDmax"),
  max_intensity = NULL,
  sample = c("blood", "plasma", "sweat", "interstitial fluid"),
  ci = FALSE,
  n_boot = 500,
  ci_level = 0.95,
  .by = NULL
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure 'loglog_restrainer' is numeric
  loglog_restrainer <- as.numeric(loglog_restrainer)
  if(length(loglog_restrainer) != 1 || is.na(loglog_restrainer))
    stop("`loglog_restrainer` must be a single numeric value between 0 and 1.", call. = FALSE)

  if(loglog_restrainer < 0 || loglog_restrainer > 1)
    stop("You can only choose a value between 0 and 1.", call. = FALSE)

  method <- match.arg(arg = method, several.ok = TRUE)
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
  if(!is.logical(ci) || length(ci) != 1 || is.na(ci))
    stop("`ci` must be `TRUE` or `FALSE`.", call. = FALSE)
  if(ci && (!is.numeric(n_boot) || length(n_boot) != 1 || n_boot < 20 || !is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1))
    stop("`n_boot` must be a number >= 20 and `ci_level` a number between 0 and 1.", call. = FALSE)

  ## column names work quoted or unquoted
  columns <- list(
    intensity_column = rlang::as_name(rlang::enexpr(intensity_column)),
    lactate_column = rlang::as_name(rlang::enexpr(lactate_column)),
    heart_rate_column = if(!is.null(rlang::enexpr(heart_rate_column))) rlang::as_name(rlang::enexpr(heart_rate_column))
  )

  if(!is.null(.by)) {
    arguments <- c(columns, list(
      method = method, fit = fit, include_baseline = include_baseline, sport = sport,
      loglog_restrainer = loglog_restrainer, plot = plot, dmax_anchor = dmax_anchor,
      moddmax_rise = moddmax_rise, ltp_fix_first_slope = ltp_fix_first_slope, dmax_methods = dmax_methods,
      max_intensity = max_intensity, sample = sample, ci = ci, n_boot = n_boot, ci_level = ci_level
    ))
    return(lactate_threshold_by(.data, .by, arguments))
  }

  data <- do.call(prepare_data, c(list(.data = .data), columns))
  if(!is.null(max_intensity)) {
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
    sample = sample
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
      out$intensity_low <- round(intervals[, "low"], digits = intensity_digits(sport))
      out$intensity_high <- round(intervals[, "high"], digits = intensity_digits(sport))
    }
  }
  if(sport != "cycling") out$pace <- convert_to_pace(rows$intensity, sport)
  out$lactate <- rows$lactate
  if("heart_rate" %in% names(data)) out$heart_rate <- retrieve_heart_rate(data, rows$intensity)

  if(plot) {
    heart_rate <- if("heart_rate" %in% names(out)) out$heart_rate else rep(NA_real_, nrow(out))
    out$plot <- lapply(seq_len(nrow(out)), function(i) {
      plot_threshold(
        method = as.character(out$method[i]),
        intensity = rows$intensity[i],
        lactate = out$lactate[i],
        heart_rate = heart_rate[i],
        plot_data = plot_data[[i]],
        data = data,
        settings = settings,
        interval = if(!is.null(intervals)) intervals[i, ]
      )
    })
  }

  new_lactate_thresholds(out, data = data, curve = curve, settings = settings, plot_data = plot_data, intensity = rows$intensity, intervals = intervals)
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

unclass_thresholds <- function(x) {
  attr(x, "lactater") <- NULL
  class(x) <- setdiff(class(x), "lactate_thresholds")
  x
}
