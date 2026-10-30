#' Check a lactate threshold analysis
#'
#' Runs a set of checks on the test and the results of [lactate_threshold()], so problems that are easy
#' to miss in a table of numbers are flagged: too few stages, no resting sample, thresholds that were not
#' reached or sit at the edge of the tested range, an early lactate dip or a final plateau, a test that may
#' have stopped before the second lactate turn point, and methods that rely on fixed blood lactate
#' concentrations used on another sample type.
#'
#' @param x The result of [lactate_threshold()].
#'
#' @return a tibble with one row per check: `check`, `status` (`"ok"`, `"note"`, or `"warning"`), and
#' `details`.
#'
#' @references
#' Hofmann P, Pokan R, von Duvillard SP, Seibert FJ, Zweiker R, Schmid P. Heart rate performance curve during
#' incremental cycle ergometer exercise in healthy young male subjects. Medicine & Science in Sports & Exercise.
#' 1997;29(6):762-768.
#'
#' Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ. Reliability and accuracy of six hand-held
#' blood lactate analysers. Journal of Sports Science and Medicine. 2015;14:203-214.
#'
#' Foxdal P, Sjodin B, Rudstam H, Ostman C, Ostman B, Hedenstierna GC. Lactate concentration differences in plasma,
#' whole blood, capillary finger blood and erythrocytes during submaximal graded exercise in humans. European
#' Journal of Applied Physiology. 1990;61(3-4):218-222.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' lactate_checks(results)
lactate_checks <- function(x) {
  info <- lactater_info(x)
  stages <- info$data[info$data$intensity > 0, ]
  n <- nrow(stages)
  intensity <- unname(info$intensity[as.character(x$method)])
  methods <- as.character(x$method)
  span <- diff(range(stages$intensity))
  check <- function(name, ok, problem, status = "warning", fine = "") {
    data.frame(check = name, status = if(ok) "ok" else status, details = if(ok) fine else problem)
  }

  missing_methods <- methods[is.na(intensity)]
  distance_to_end <- pmin(abs(intensity - min(stages$intensity)), abs(intensity - max(stages$intensity)))
  at_edge <- methods[!is.na(intensity) & distance_to_end < 0.01 * span]
  ## ModDmax only relies on a blood value when its 0.4 mmol/L default rise is kept
  absolute <- methods[grepl("^OBLA|^Bsln|^IAT$", methods) | (methods == "ModDmax" & info$settings$moddmax_rise == 0.4)]
  first_lowest <- stages$lactate[1] - min(stages$lactate[-1])
  ## two stages per segment
  ltp_needs <- 2 * (info$settings$ltp_breakpoints + 1)
  ## LTP2 is "the second increase of blood lactate concentration around 4 mmol/L" (Hofmann et al., 1997)
  peak <- max(stages$lactate)
  ltp2_reachable <- !"LTP2" %in% methods || info$settings$ltp_breakpoints == 1 ||
    !info$settings$sample %in% c("blood", "plasma") || peak >= 4

  out <- rbind(
    check(
      "Stages", n >= ltp_needs,
      sprintf("%d stages: LTP needs at least %d, and the fitted curves rest on few points.", n, ltp_needs),
      fine = sprintf("%d stages.", n)
    ),
    check(
      "Resting sample", any(info$data$intensity == 0),
      "No resting sample (intensity 0): Bsln+ uses the first stage as the baseline.",
      status = "note", fine = "Present."
    ),
    check(
      "Thresholds found", length(missing_methods) == 0,
      sprintf("Not found within the tested range: %s.", paste(missing_methods, collapse = ", ")),
      fine = "All thresholds were found."
    ),
    check(
      "Edge of the range", length(at_edge) == 0,
      sprintf("At the first or last stage, so possibly outside the tested range: %s.", paste(at_edge, collapse = ", ")),
      fine = "No threshold sits at the first or last stage."
    ),
    ## low-concentration analyzer error is about 0.1 mmol/L (Bonaventura et al., 2015)
    check(
      "Early lactate dip", first_lowest <= 0.2,
      sprintf(
        paste(
          "The first stage is %.1f mmol/L above the lowest later stage; it shapes Dmax and Bsln+.",
          "Check warm-up and sampling."
        ),
        first_lowest
      ),
      status = "note", fine = "No early dip."
    ),
    check(
      "Final stage", stages$lactate[n] > stages$lactate[n - 1],
      "Lactate did not rise in the last stage (incomplete stage or plateau); consider `max_intensity`.",
      status = "note", fine = "Lactate rose in the last stage."
    ),
    check(
      "Peak lactate", ltp2_reachable,
      sprintf(
        paste(
          "Peak lactate was %.1f mmol/L, below the ~4 mmol/L where LTP2 usually sits: the test may have stopped",
          "before LTP2. If it was submaximal, use `ltp_breakpoints = 1`."
        ),
        peak
      ),
      fine = sprintf("%.1f mmol/L.", peak)
    ),
    check(
      "Sample type", info$settings$sample == "blood" || length(absolute) == 0,
      sprintf(
        "%s lactate: %s use fixed blood lactate values that do not transfer to this sample (%s).",
        info$settings$sample, paste(absolute, collapse = ", "),
        if(info$settings$sample == "plasma") {
          "plasma reads higher than whole blood; Foxdal et al., 1990"
        } else {
          "they were defined on blood lactate"
        }
      ),
      fine = sprintf("%s lactate.", info$settings$sample)
    )
  )
  tibble::as_tibble(out)
}

#' Goodness of fit of the lactate curves
#'
#' Fits every available curve to the same points and reports how well each follows the measured
#' lactate, to help choose `fit` in [lactate_threshold()]. The statistics are computed on the measured
#' stages (plus the resting sample when `include_baseline = TRUE`).
#'
#' @param x The result of [lactate_threshold()].
#'
#' @return a tibble with one row per curve: `fit`, `used` (the curve chosen in [lactate_threshold()]),
#' `n_points`, `parameters`, `r_squared`, `rmse` and `see` (standard error of the estimate) in mmol/L, and
#' `max_abs_residual` in mmol/L.
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
#' lactate_fit_quality(results)
lactate_fit_quality <- function(x) {
  info <- lactater_info(x)
  points <- modeling_data(info$data, info$settings$include_baseline)
  fits <- c("3rd degree polynomial", "4th degree polynomial", "B-spline", "Exponential")
  rows <- lapply(fits, function(fit) {
    curve <- fit_curve(points, fit)
    residuals <- points$lactate - curve$predict(points$intensity)
    n <- length(residuals)
    p <- length(curve$coefficients)
    rss <- sum(residuals^2)
    data.frame(
      fit = fit,
      used = fit == info$settings$fit,
      n_points = n,
      parameters = p,
      r_squared = 1 - rss / sum((points$lactate - mean(points$lactate))^2),
      rmse = sqrt(rss / n),
      see = if(n > p) sqrt(rss / (n - p)) else NA_real_,
      max_abs_residual = max(abs(residuals))
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

lactater_info <- function(x) {
  info <- attr(x, "lactater")
  if(is.null(info))
    stop(
      "This result has lost its lactater attributes (for example after a dplyr verb). ",
      "Use the result of `lactate_threshold()` directly.",
      call. = FALSE
    )
  info
}
