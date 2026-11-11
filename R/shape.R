#' Did the lactate curve shift, or change its shape?
#'
#' Tests whether two tests of the same athlete differ only by a horizontal shift of the lactate curve, or also in its
#' shape. Both tests are fitted with the exponential plus constant, `lactate = a + b * exp(c * intensity)` (Hughson et
#' al., 1987), in which a pure horizontal shift by `d` changes only `b`:
#'
#' * **Shift only**: one shape for both tests (shared `a`, `b` and `c`), with the second test moved by `d`.
#' * **Free shape**: each test with its own `a`, `b` and `c`.
#'
#' The two fits are compared with an extra-sum-of-squares F test (Motulsky & Ransnas, 1987) and with AICc, the
#' Akaike criterion corrected for small samples (Hurvich & Tsai, 1989); the verdict is `"shape changed"` only when
#' both prefer the free shape. The shift `d` comes with a profile confidence interval.
#'
#' Read the result with care:
#' * The test finds a change of shape only when it is large and the tests are long. In simulated pairs of tests of a
#'   trained cyclist (analyzer error of 0.1 mmol/L plus 4%), a baseline 0.5 mmol/L higher was found in 96% of pairs
#'   with 8 stages per test and in 20% with 6; a curve steep enough to double the peak lactate in 55% and 10%. So "no
#'   shape change detected" is not proof of the same shape, least of all with 6 stages.
#' * The p-value is approximate: the F test is exact only for models that are linear in their parameters, and a low
#'   p-value from one pair of curves needs a repeat before it counts (Motulsky & Ransnas, 1987). In the same
#'   simulation it fell below 0.05 in 8% to 10% of pairs with no change of shape; with AICc beside it, the verdict was
#'   wrong in 1% to 6%.
#' * In this model, a shift to the right is the same as a proportional fall of lactate above baseline. Better
#'   endurance, a lower glycolytic rate (VLamax) at the same VO2max, low glycogen stores and overreaching can all
#'   produce it, so check the peak lactate, the diet before the test and the performance too (Maassen & Busse, 1989;
#'   Faude et al., 2009; Wackerhage et al., 2025). A shift to the left can likewise come from a higher VLamax, not only
#'   from lost endurance.
#' * The exponential cannot follow an early lactate dip; check both tests with [lactate_diagnostics()] first.
#'
#' @param before,after The results of [lactate_threshold()] for the two tests, with the same sport and at least five
#' stages each.
#' @param level The confidence level of the shift. Defaults to `0.95`.
#'
#' @return a `lactate_shape` list with `shift` (intensity units, positive when the curve moved to the right),
#' `shift_low` and `shift_high`, `f`, `df1`, `df2`, `p_value`, `aicc_shift` and `aicc_free`, `verdict`
#' (`"shape changed"` when the F test and AICc both prefer the free shape, `"shift"` otherwise: none was detected), the peak lactate of both tests and the two
#' exponential `curves`. Print it for a summary, and plot it with [autoplot()]: the two fitted curves, and the first one
#' moved by the shift (dashed).
#'
#' @references
#' Faude O, Kindermann W, Meyer T. Lactate threshold concepts: how valid are they? Sports Medicine.
#' 2009;39(6):469-490.
#'
#' Hurvich CM, Tsai CL. Regression and time series model selection in small samples. Biometrika.
#' 1989;76(2):297-307. doi:10.1093/biomet/76.2.297
#'
#' Hughson RL, Weisiger KH, Swanson GD. Blood lactate concentration increases as a continuous function in progressive
#' exercise. Journal of Applied Physiology. 1987;62(5):1975-1981.
#'
#' Maassen N, Busse MW. The relationship between lactic acid and work load: a measure for endurance capacity or an
#' indicator of carbohydrate deficiency? European Journal of Applied Physiology and Occupational Physiology.
#' 1989;58(7):728-737.
#'
#' Motulsky HJ, Ransnas LA. Fitting curves to data using nonlinear regression: a practical and nonmathematical review.
#' FASEB Journal. 1987;1(5):365-374.
#'
#' Wackerhage H, Kabasakalis A, Seiler S, Heck H. Is the vLamax for glycolysis what the VO2max is for oxidative
#' phosphorylation? Sports Medicine. 2025;55(8):1853-1866.
#'
#' @export
#'
#' @examples
#' series <- subset(lactate_tests, athlete == "cyclist_series")
#' before <- lactate_threshold(subset(series, day == 0), intensity, lactate, plot = FALSE)
#' after <- lactate_threshold(subset(series, day == 58), intensity, lactate, plot = FALSE)
#' curve_shape(before, after)
curve_shape <- function(before, after, level = 0.95) {
  info_before <- lactater_info(before)
  info_after <- lactater_info(after)
  if(info_before$settings$sport != info_after$settings$sport)
    stop("Both tests must be from the same sport.", call. = FALSE)
  one <- info_before$data[info_before$data$intensity > 0, ]
  two <- info_after$data[info_after$data$intensity > 0, ]
  ## the free model has six parameters: fewer stages leave the F test and AICc without degrees of freedom
  if(min(nrow(one), nrow(two)) < 5) stop("Each test needs at least five stages.", call. = FALSE)
  x <- c(one$intensity, two$intensity)
  y <- c(one$lactate, two$lactate)
  second <- rep(c(FALSE, TRUE), c(nrow(one), nrow(two)))
  n <- length(y)
  span <- diff(range(x))

  ## for fixed rate k and shift d the model is linear in a and b: search k for each d (variable projection)
  rss_at <- function(d) {
    shifted <- (x - d * second - min(x)) / span
    rss <- function(k) sum(stats::.lm.fit(cbind(1, exp(k * shifted)), y)$residuals^2)
    grid <- seq(0.05, 40, length.out = 100)
    refine_extremum(rss, grid, vapply(grid, rss, numeric(1)))$objective
  }
  ds <- seq(-span, span, length.out = 161)
  profile <- vapply(ds, rss_at, numeric(1))
  fit <- refine_extremum(rss_at, ds, profile)
  shift <- fit$minimum
  rss_shift <- fit$objective
  curves <- list(before = fit_curve(one, "Exponential"), after = fit_curve(two, "Exponential"))
  rss_free <- sum((one$lactate - curves$before$predict(one$intensity))^2) +
    sum((two$lactate - curves$after$predict(two$intensity))^2)

  df1 <- 2
  df2 <- n - 6
  f <- ((rss_shift - rss_free) / df1) / (rss_free / df2)
  p_value <- stats::pf(f, df1, df2, lower.tail = FALSE)
  aicc <- function(rss, k) n * log(rss / n) + 2 * k + 2 * k * (k + 1) / (n - k - 1)
  ## profile interval: shifts whose fit is not worse than the best by the F criterion with one degree of freedom
  limit <- rss_shift * (1 + stats::qf(level, 1, n - 4) / (n - 4))
  crossing <- function(from, to) stats::uniroot(function(d) rss_at(d) - limit, c(from, to))$root
  below <- ds < shift & profile > limit
  above <- ds > shift & profile > limit
  low <- if(any(below)) crossing(max(ds[below]), shift) else NA_real_
  high <- if(any(above)) crossing(shift, min(ds[above])) else NA_real_

  structure(
    list(
      shift = shift, shift_low = low, shift_high = high, f = f, df1 = df1, df2 = df2, p_value = p_value,
      aicc_shift = aicc(rss_shift, 5), aicc_free = aicc(rss_free, 7),
      verdict = if(p_value < 0.05 && aicc(rss_free, 7) < aicc(rss_shift, 5)) "shape changed" else "shift",
      peak_before = max(one$lactate), peak_after = max(two$lactate),
      curves = curves, sport = info_before$settings$sport, level = level
    ),
    class = "lactate_shape"
  )
}

#' @export
print.lactate_shape <- function(x, ...) {
  sport <- x$sport
  unit <- intensity_unit(sport)
  ## + 0 turns a rounded -0 into 0
  shift <- sprintf("%+g", round(x$shift, intensity_digits(sport)) + 0)
  interval <- if(is.na(x$shift_low) || is.na(x$shift_high)) {
    ""
  } else {
    sprintf(" (%g%% CI %s to %s)", 100 * x$level, format_intensity(x$shift_low, sport),
            format_intensity(x$shift_high, sport))
  }
  cat(sprintf(
    "Shift only vs free shape: F(%d, %d) = %.2f, approximate p = %.3f; AICc %.1f vs %.1f.\n",
    x$df1, x$df2, x$f, x$p_value, x$aicc_shift, x$aicc_free
  ))
  cat(if(x$verdict == "shift") {
    sprintf("No shape change detected: one curve shifted by %s %s%s describes both tests%s.\n",
            shift, unit, interval,
            if(min(nrow(x$curves$before$points), nrow(x$curves$after$points)) < 8) {
              ", but with fewer than 8 stages the test finds few changes of shape"
            } else "")
  } else {
    sprintf("The shape changed: a shift alone (%s %s) does not describe both tests.\n", shift, unit)
  })
  caveat <- if(x$verdict == "shift") {
    " A shift can also come from low glycogen or overreaching, not only fitness."
  } else {
    ""
  }
  cat(sprintf("Peak lactate %.1f -> %.1f mmol/L.%s\n", x$peak_before, x$peak_after, caveat))
  invisible(x)
}

#' @rdname curve_shape
#' @param object The result of [curve_shape()].
#' @param ... Not used.
#' @export
autoplot.lactate_shape <- function(object, ...) {
  colours <- lactater_colours()
  sport <- object$sport
  stages <- function(curve, label) cbind(curve$points[c("intensity", "lactate")], test = label)
  before <- cbind(curve_points(object$curves$before), test = "Before")
  after <- cbind(curve_points(object$curves$after), test = "After")
  moved <- transform(before, intensity = intensity + object$shift, test = "Before, shifted")
  curves <- rbind(before, after, moved)
  curves$test <- factor(curves$test, levels = c("Before", "Before, shifted", "After"))
  points <- rbind(stages(object$curves$before, "Before"), stages(object$curves$after, "After"))
  points$test <- factor(points$test, levels = levels(curves$test))
  ends <- do.call(rbind, lapply(split(curves, curves$test), function(d) d[nrow(d), ]))
  ends$hjust <- ifelse(ends$test == "Before", 1.25, -0.25)
  ## the first test in gray as in the comparison plot, and its shifted copy in the blue of the test it is held against
  palette <- c("Before" = colours[["faint"]], "Before, shifted" = colours[["curve"]], "After" = colours[["curve"]])
  top <- max(points$lactate, curves$lactate) * 1.05

  ggplot2::ggplot(curves, ggplot2::aes(intensity, lactate, colour = test)) +
    ggplot2::geom_line(ggplot2::aes(linetype = test), linewidth = 0.85) +
    ggplot2::geom_point(data = points, shape = 21, fill = colours[["paper"]], size = 2.2, stroke = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = test, hjust = hjust), ends, size = 3) +
    ggplot2::scale_colour_manual(values = palette, guide = "none") +
    ggplot2::scale_linetype_manual(values = c("Before" = "solid", "Before, shifted" = "22", "After" = "solid"),
                                   guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
    ggplot2::scale_y_continuous(limits = c(0, top), breaks = lactate_breaks(top), expand = c(0, 0)) +
    ggplot2::labs(
      title = if(object$verdict == "shift") "No shape change detected" else "The curve changed shape",
      subtitle = sprintf(
        "Dashed: the first curve moved by %+g %s; %s (F test, approximate p = %.3f)",
        round(object$shift, intensity_digits(sport)), intensity_unit(sport),
        if(object$verdict == "shift") "the second test does not clearly differ" else
          "it does not match the second test", object$p_value
      ),
      x = intensity_label(sport), y = "Blood lactate (mmol/L)"
    ) +
    theme_lactater()
}
