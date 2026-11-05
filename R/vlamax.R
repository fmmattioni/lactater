#' Maximal lactate accumulation rate (VLamax)
#'
#' Estimates the maximal lactate accumulation rate (VLamax) from a short all-out sprint test (typically
#' 15 s), not from the incremental test:
#'
#' \deqn{VLamax = (La_{peak} - La_{pre}) / (t_{test} - t_{alactic})}
#'
#' @details
#' VLamax needs its own protocol: a resting lactate sample before the sprint, an all-out sprint, and
#' lactate sampled every minute for several minutes after it (Meixner et al. (2024) used up to 8 min).
#' The highest post-sprint value is the peak.
#'
#' The alactic time (`alactic_time`) is the part of the sprint assumed to be covered by phosphocreatine.
#' Fixing it at 3.5 s gave the most reliable VLamax in 50 cyclists (ICC 0.91, CV 3.1%); estimating it from
#' the power curve was much less reliable (CV 12-26%), and a 1-s change in it can change VLamax by up to
#' 26% (Meixner et al., 2024). Protocols vary a lot between studies (Langley et al., 2025), and whether
#' VLamax measures the maximal glycolytic rate is still debated: Clark & Macdermid (2025) found it adds
#' little beyond the sprint power itself. Compare values only within the same protocol, ergometer and
#' lactate analyzer.
#'
#' The default `test_duration` of 15 s is the sprint of Meixner et al. (2024) and of most cycling studies. A review of
#' 60 studies recommends a sport-specific all-out sprint of 10-12 s instead, with lactate sampled every minute for 10
#' min, and calls the rate from 15 s or longer a peak rather than a maximal rate; it also found the use of VLamax to
#' predict performances of 1 min or longer inconclusive (Quittmann, 2026). Set `test_duration` to your sprint.
#'
#' `vlapeak` is the same rise divided by the whole sprint, with no alactic time,
#' \eqn{vLa_{peak} = (La_{peak} - La_{pre}) / t_{test}}, so it is lower than `vlamax`. Wackerhage et al. (2025) proposed it
#' partly because the alactic-time correction is error-prone. In 182 cyclists and triathletes, both gave similar
#' associations with the lactate thresholds (Röhrs et al., 2026).
#'
#' @param lactate_pre Blood lactate before the sprint (mmol/L).
#' @param lactate_post Blood lactate samples after the sprint (mmol/L); the highest is used.
#' @param test_duration Sprint duration in seconds. Default to `15`.
#' @param alactic_time Alactic time in seconds. Default to `3.5` (Meixner et al., 2024).
#'
#' @return a tibble with `vlamax` and `vlapeak` (mmol/L/s), `lactate_peak` and `delta_lactate` (mmol/L), and
#' `glycolytic_time` (s).
#'
#' @references
#' Meixner B, Nusser V, Koehler K, Sablain M, Boone J, Sperlich B. Reliability of power output, maximal rate of
#' capillary blood lactate accumulation, and phosphagen contribution time following 15-s sprint cycling in amateur
#' cyclists. Physiological Reports. 2024;12(10):e16086. doi:10.14814/phy2.16086
#'
#' Langley J, Haase R, Nitzsche N, Porter M. Methodological approaches in testing maximal lactate accumulation rate -
#' vLamax: a
#' systematic review. Journal of Science and Cycling. 2025;14(1):9. doi:10.28985/1425.jsc.09
#'
#' Clark B, Macdermid PW. VLamax correlates strongly with glycolytic performance. Research Quarterly for
#' Exercise and Sport. 2025:1-8.
#'
#' Quittmann OJ. Maximal lactate accumulation rate (cLamax): current evidence and future directions for exercise
#' testing and training. European Journal of Applied Physiology. 2026;126(1):1-36. doi:10.1007/s00421-025-06022-7
#'
#' Röhrs D, Wagner CM, Reer R, Keiner M. Lactate accumulation rate is associated with lactate turnpoints in cyclists
#' and triathletes: a comparison of vLapeak and vLamaxblood. European Journal of Applied Physiology. 2026.
#' doi:10.1007/s00421-026-06444-x
#'
#' Wackerhage H, Kabasakalis A, Seiler S, Heck H. Is the vLamax for glycolysis what the VO2max is for oxidative
#' phosphorylation? Sports Medicine. 2025;55(8):1853-1866. doi:10.1007/s40279-025-02259-6
#'
#' @export
#'
#' @examples
#' vlamax(lactate_pre = 1.1, lactate_post = c(8.9, 10.4, 10.9, 10.6, 10.1))
vlamax <- function(lactate_pre, lactate_post, test_duration = 15, alactic_time = 3.5) {
  if(!is.numeric(lactate_pre) || length(lactate_pre) != 1 || is.na(lactate_pre) || lactate_pre <= 0)
    stop("`lactate_pre` must be a single lactate value > 0.", call. = FALSE)
  if(!is.numeric(lactate_post) || length(lactate_post) == 0 || all(is.na(lactate_post)))
    stop("`lactate_post` must contain at least one lactate value.", call. = FALSE)
  if(!is.numeric(test_duration) || !is.numeric(alactic_time) || length(test_duration) != 1 ||
       length(alactic_time) != 1 || alactic_time < 0 || test_duration <= alactic_time)
    stop("`test_duration` must be longer than `alactic_time` (both in seconds).", call. = FALSE)

  peak <- max(lactate_post, na.rm = TRUE)
  glycolytic_time <- test_duration - alactic_time
  tibble::tibble(
    vlamax = (peak - lactate_pre) / glycolytic_time,
    vlapeak = (peak - lactate_pre) / test_duration,
    lactate_peak = peak,
    delta_lactate = peak - lactate_pre,
    glycolytic_time = glycolytic_time
  )
}
