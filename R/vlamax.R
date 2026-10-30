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
#' @param lactate_pre Blood lactate before the sprint (mmol/L).
#' @param lactate_post Blood lactate samples after the sprint (mmol/L); the highest is used.
#' @param test_duration Sprint duration in seconds. Default to `15`.
#' @param alactic_time Alactic time in seconds. Default to `3.5` (Meixner et al., 2024).
#'
#' @return a tibble with `vlamax` (mmol/L/s), `lactate_peak` and `delta_lactate` (mmol/L), and
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
    lactate_peak = peak,
    delta_lactate = peak - lactate_pre,
    glycolytic_time = glycolytic_time
  )
}
