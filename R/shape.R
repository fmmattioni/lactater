#' Did the lactate curve shift, or change its shape?
#'
#' Tests whether two tests of the same athlete differ only by a horizontal shift of the lactate curve, or also in its
#' shape. Both tests are fitted with the exponential plus constant, `lactate = a + b * exp(c * intensity)` (Hughson et
#' al., 1987), in which a pure horizontal shift by `d` changes only `b`:
#'
#' * **Shift only**: one shape for both tests (shared `a`, `b` and `c`), with the second test moved by `d`.
#' * **Free shape**: each test with its own `a`, `b` and `c`.
#'
#' The two fits are compared with an extra-sum-of-squares F test and with AICc (global fitting with shared parameters;
#' Motulsky & Ransnas, 1987). The shift `d` comes with a profile confidence interval.
#'
#' Read the result with care:
#' * With 6 to 10 stages per test the F test has little power: "no evidence of a shape change" is not proof of the same
#'   shape.
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
#' `shift_low` and `shift_high`, `f`, `df1`, `df2`, `p_value`, `aicc_shift` and `aicc_free`, `verdict` (`"shift"`
#' when the shift-only model is not worse, `"shape changed"` otherwise), the peak lactate of both tests and the two
#' exponential `curves`. Print it for a summary, and plot it with [autoplot()]: the two fitted curves, and the first one
#' moved by the shift (dashed).
#'
#' @references
#' Faude O, Kindermann W, Meyer T. Lactate threshold concepts: how valid are they? Sports Medicine.
#' 2009;39(6):469-490.
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
    "Shift only vs free shape: F(%d, %d) = %.2f, p = %.3f; AICc %.1f vs %.1f.\n",
    x$df1, x$df2, x$f, x$p_value, x$aicc_shift, x$aicc_free
  ))
  cat(if(x$verdict == "shift") {
    sprintf("No evidence the shape changed: one curve shifted by %s %s%s describes both tests.\n",
            shift, unit, interval)
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
  ends$hjust <- ifelse(ends$test == "Before", 1.15, -0.1)
  palette <- c("Before" = colours[["muted"]], "Before, shifted" = colours[["accent"]], "After" = colours[["curve"]])
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
      title = if(object$verdict == "shift") "The curve shifted" else "The curve changed shape",
      subtitle = sprintf(
        "Dashed: the first curve moved by %+g %s; it %s the second test (F test p = %.3f)",
        round(object$shift, intensity_digits(sport)), intensity_unit(sport),
        if(object$verdict == "shift") "matches" else "does not match", object$p_value
      ),
      x = intensity_label(sport), y = "Blood lactate (mmol/L)"
    ) +
    theme_lactater()
}
