#' Lactate threshold
#'
#' This is a general function that applies several lactate threshold methods at the same time.
#'
#' @param .data The raw data.
#' @param intensity_column The name of the intensity column.
#' @param lactate_column The name of the lactate column.
#' @param heart_rate_column The name of the heart rate column, if applicable.
#' @param method The lactate threshold method to calculate. It can be one or many of the following:
#' `Log-log`, `OBLA`, `Bsln+`, `Dmax`, `LTP`, `LTratio`. See `Details` for more information.
#' Default to `c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio")`.
#' @param fit The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.
#' Please, note that a few lactate thresholds have default methods for this and cannot be changed.
#' Options are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline`. See `Details`.
#' @param include_baseline A boolean to indicate whether to include the baseline value in the fit.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method. For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods. Default to `TRUE`.
#'
#' @return a [tibble][tibble::tibble-package] with the following columns:
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
#' The lactate response (i.e., log of lactate vs intensity) is divided into two segments.
#' A segmented regression is then performed such that the lactate curve would present one breaking point. The exercise
#' intensity at which the breaking point occurs is then considered as Log-log (Beaver et al., 1985).
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
#' ### Dmax
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between the first and the last data point (Cheng et al., 1992). The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
#'
#' ### Modified Dmax (ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between data point preceding the first rise in lactate greater than 0.4 mmol/L and the last
#' data point (Bishop et al., 1998). The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
#'
#' ### Exponential Dmax (Exp-Dmax)
#'
#' The exercise intensity on the exponential plus-constant regression lactate curve that
#' yields the maximum perpendicular distance to the straight line between the first and the
#' last data point (Hughson et al., 1987). The lactate curve is fitted using an exponential curve,
#' and it can't be changed.
#'
#' ### Log-log modified Dmax (Log-Poly-ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between Log-log and the last data point in the 3rd order polynomial regression curve (Jamnick et al., 2018).
#' The lactate curve is fitted using a 3rd order polynomial regression curve,
#' and it can't be changed.
#'
#' ### Log-log exponential Dmax (Log-Exp-ModDmax)
#'
#' The exercise intensity that yields the maximum perpendicular distance to the straight line
#' between Log-log and the last data point in the exponential curve (Jamnick et al., 2018).
#' The lactate curve is fitted using an exponential curve,
#' and it can't be changed.
#'
#' ## LTP
#'
#' ### Lactate Turning Point 1 (LTP1) and Lactate Turning Point 2 (LTP2)
#'
#' the lactate response is divided into three segments. A segmented regression is performed
#' such that the lactate curve yields two breaking points. The first breaking point, representing
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
lactate_threshold <- function(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column,
  method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio"),
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming"),
  loglog_restrainer = 1,
  plot = TRUE
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure 'loglog_restrainer' is numeric
  loglog_restrainer <- as.numeric(loglog_restrainer)

  if(loglog_restrainer < 0 | loglog_restrainer > 1)
    stop("You can only choose a value between 0 and 1.", call. = FALSE)

  ## make sure column names work unquoted too
  intensity_column <- rlang::ensym(intensity_column)
  lactate_column <- rlang::ensym(lactate_column)

  method <- match.arg(arg = method, several.ok = TRUE)
  fit <- match.arg(arg = fit, several.ok = FALSE)
  sport <- match.arg(arg = sport, several.ok = FALSE)

  if(!missing(heart_rate_column)) {
    heart_rate_column <- rlang::ensym(heart_rate_column)

    data_prepared <- prepare_data(
      .data = .data,
      intensity_column = {{ intensity_column }},
      lactate_column = {{ lactate_column }},
      heart_rate_column = {{ heart_rate_column }}
    )

  } else {

    data_prepared <- prepare_data(
      .data = .data,
      intensity_column = {{ intensity_column }},
      lactate_column = {{ lactate_column }}
    )

  }

  data_fit_prepared <- data_prepared %>%
    prepare_fit(
      .data = .,
      fit = fit,
      include_baseline = include_baseline,
      sport = sport
    )

  out <- dplyr::tibble(method = method) %>%
    dplyr::rename(method_category = method) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      results = switch (
        method_category,
        "Log-log" = method_loglog(data_prepared = data_fit_prepared, fit = fit, sport = sport, loglog_restrainer = loglog_restrainer, plot = plot) %>% list(),
        "OBLA" = method_obla(data_prepared = data_fit_prepared, fit = fit, sport = sport, plot = plot) %>% list(),
        "Bsln+" = method_bsln_plus(data_prepared = data_fit_prepared, fit = fit, sport = sport, plot = plot) %>% list(),
        "Dmax" = method_dmax(data_prepared = data_fit_prepared, sport = sport, loglog_restrainer = loglog_restrainer, plot = plot) %>% list(),
        "LTP" = method_ltp(data_prepared = data_fit_prepared, fit = fit, sport = sport, plot = plot) %>% list(),
        "LTratio" = method_ltratio(data_prepared = data_fit_prepared, fit = fit, sport = sport, plot = plot) %>% list()
      )
    ) %>%
    tidyr::unnest(results) %>%
    dplyr::mutate(method_category = factor(method_category, levels = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio"))) %>%
    dplyr::arrange(method_category) %>%
    dplyr::mutate(method = forcats::as_factor(method))

  out
}
