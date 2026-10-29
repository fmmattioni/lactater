#' Fit a lactate curve
#'
#' Fits the lactate curve used by the threshold methods. For internal use only.
#'
#' * `"3rd degree polynomial"` and `"4th degree polynomial"`: ordinary least squares on raw powers.
#' * `"B-spline"`: a natural cubic regression spline with 4 degrees of freedom (`splines::ns()`).
#' * `"Exponential"`: the exponential plus constant `a + b * exp(c * x)` (Hughson et al., 1987). For a
#'   fixed `c` the model is linear in `a` and `b`, so only `c` is searched (variable projection):
#'   no starting values are needed and the fit cannot fail to converge.
#'
#' @param points A data frame with `intensity` and `lactate`.
#' @param fit The curve type.
#'
#' @return a list with the `fit` type, the `points`, the `domain` (intensity range of the points),
#' the `coefficients`, `predict`, a function returning the fitted lactate at given intensities, and,
#' for the fits that are linear in their parameters, the `leverage` of each point.
#' @keywords internal
fit_curve <- function(points, fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline", "Exponential")) {
  fit <- match.arg(fit)
  x <- points$intensity
  y <- points$lactate
  curve <- switch(
    fit,
    "3rd degree polynomial" = fit_polynomial(x, y, degree = 3),
    "4th degree polynomial" = fit_polynomial(x, y, degree = 4),
    "B-spline" = fit_spline(x, y),
    "Exponential" = fit_exponential(x, y)
  )
  c(list(fit = fit, points = points, domain = range(x)), curve)
}

fit_polynomial <- function(x, y, degree) {
  powers <- function(v) outer(v, 0:degree, "^")
  fit <- stats::lm.fit(powers(x), y)
  coefficients <- unname(fit$coefficients)
  list(coefficients = coefficients, predict = function(v) drop(powers(v) %*% coefficients), leverage = leverage(fit))
}

fit_spline <- function(x, y) {
  basis <- splines::ns(x, df = 4)
  fit <- stats::lm.fit(cbind(1, basis), y)
  coefficients <- unname(fit$coefficients)
  list(
    coefficients = coefficients,
    predict = function(v) drop(cbind(1, stats::predict(basis, v)) %*% coefficients),
    leverage = leverage(fit)
  )
}

## diagonal of the hat matrix
leverage <- function(fit) {
  rowSums(qr.Q(fit$qr)[, seq_len(fit$rank), drop = FALSE]^2)
}

fit_exponential <- function(x, y) {
  ## search the rate on intensities scaled to [0, 1] for conditioning
  width <- diff(range(x))
  scaled <- (x - min(x)) / width
  linear_part <- function(k) stats::lm.fit(cbind(1, exp(k * scaled)), y)
  rss <- function(k) sum(linear_part(k)$residuals^2)
  ## shortcut: rates beyond 40 (a 2e17-fold rise across the test) are not lactate curves
  grid <- c(seq(-20, -0.05, length.out = 100), seq(0.05, 40, length.out = 400))
  best <- which.min(vapply(grid, rss, numeric(1)))
  k <- stats::optimize(rss, grid[c(max(best - 1, 1), min(best + 1, length(grid)))], tol = 1e-10)$minimum
  ab <- linear_part(k)$coefficients
  c_ <- k / width
  a <- unname(ab[1])
  b <- unname(ab[2] * exp(-c_ * min(x)))
  list(coefficients = c(a = a, b = b, c = c_), predict = function(v) a + b * exp(c_ * v))
}

#' Data used for modeling
#'
#' The exercise stages, plus the positioned resting sample when `include_baseline = TRUE`.
#' The prepared data keeps the resting sample at intensity 0; only the fit places it one step
#' before stage 1 (see `position_rest()`).
#'
#' @param .data The data retrieved from `prepare_data()`.
#' @param include_baseline Whether to include the resting sample.
#'
#' @keywords internal
modeling_data <- function(.data, include_baseline) {
  if(include_baseline) position_rest(.data) else .data[.data$intensity > 0, ]
}

#' Points along a fitted curve
#'
#' @param curve A curve from `fit_curve()`.
#' @param from Where to start (default: the start of the curve's domain).
#' @param n Number of points.
#'
#' @return a data frame with `intensity` and `lactate`.
#' @keywords internal
curve_points <- function(curve, from = curve$domain[1], n = 200) {
  intensity <- seq(from, curve$domain[2], length.out = n)
  data.frame(intensity = intensity, lactate = curve$predict(intensity))
}
