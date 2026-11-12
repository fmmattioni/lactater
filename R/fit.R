#' Fit a lactate curve
#'
#' Fits the lactate curve used by the threshold methods. For internal use only.
#'
#' * `"3rd degree polynomial"` and `"4th degree polynomial"`: ordinary least squares on raw powers.
#' * `"B-spline"`: a natural cubic regression spline with 4 degrees of freedom (`splines::ns()`).
#' * `"Exponential"`: the exponential plus constant `a + b * exp(c * x)` (Hughson et al., 1987). For a
#'   fixed `c` the model is linear in `a` and `b`, so only `c` is searched (variable projection), and only
#'   "in the positive range" as in Hughson et al.: no starting values are needed and the fit cannot fail to
#'   converge.
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
    "3rd degree polynomial" = fit_polynomial(x, y, degree = 3, fit),
    "4th degree polynomial" = fit_polynomial(x, y, degree = 4, fit),
    "B-spline" = fit_spline(x, y, fit),
    "Exponential" = fit_exponential(x, y)
  )
  c(list(fit = fit, points = points, domain = range(x)), curve)
}

fit_polynomial <- function(x, y, degree, label) {
  powers <- function(v) outer(v, 0:degree, "^")
  fit <- fit_linear(powers(x), y, label)
  coefficients <- unname(fit$coefficients)
  list(coefficients = coefficients, predict = function(v) drop(powers(v) %*% coefficients), leverage = leverage(fit))
}

fit_spline <- function(x, y, label) {
  basis <- splines::ns(x, df = 4)
  fit <- fit_linear(cbind(1, basis), y, label)
  coefficients <- unname(fit$coefficients)
  list(
    coefficients = coefficients,
    predict = function(v) drop(cbind(1, stats::predict(basis, v)) %*% coefficients),
    leverage = leverage(fit)
  )
}

#' Refine the extremum of a function found on a grid
#'
#' The grid finds the basin of the extremum; `optimize()` between the neighbours of the best grid point makes it
#' exact. For internal use only.
#'
#' @param f The function of one variable.
#' @param grid Where it was evaluated.
#' @param values `f(grid)`, when already computed.
#' @param maximum Whether to look for a maximum.
#' @param tol The tolerance of `optimize()`.
#' @return the result of `optimize()`: `minimum` or `maximum`, and `objective`.
#' @keywords internal
refine_extremum <- function(f, grid, values = f(grid), maximum = FALSE, tol = 1e-8) {
  best <- if (maximum) which.max(values) else which.min(values)
  stats::optimize(f, grid[c(max(best - 1, 1), min(best + 1, length(grid)))], maximum = maximum, tol = tol)
}

## a curve with as many parameters as points, or more, is not determined: lm.fit would return NA coefficients
fit_linear <- function(design, y, label) {
  fit <- stats::lm.fit(design, y)
  if(fit$rank < ncol(design)) {
    stop(sprintf("`%s` has %d parameters, so it needs at least %d points; this test has %d.",
                 label, ncol(design), ncol(design), length(y)), call. = FALSE)
  }
  fit
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
  grid <- seq(0.05, 40, length.out = 400)
  ## the residual sum of squares of y on a constant and exp(k * scaled), for every k of the grid at once
  e <- exp(outer(scaled, grid))
  e <- sweep(e, 2, colMeans(e))
  centred <- y - mean(y)
  k <- refine_extremum(rss, grid, values = sum(centred^2) - colSums(e * centred)^2 / colSums(e^2), tol = 1e-10)$minimum
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
