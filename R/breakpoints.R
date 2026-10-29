#' Fit a continuous piecewise-linear model by exhaustive breakpoint search
#'
#' For fixed breakpoints the model `y = b0 + b1 x + sum(c_j * max(x - psi_j, 0))` is linear, so
#' the residual sum of squares can be computed for every candidate. All breakpoint positions on a
#' grid are tried and the best one is refined locally. Unlike iterative fits (e.g. `segmented`), the
#' result is the global least-squares optimum (up to the grid) and is deterministic. For internal use only.
#'
#' @param x,y The measured points.
#' @param n_breakpoints Number of breakpoints (1 or 2).
#' @param flat_first Whether the first segment is flat (slope fixed at 0).
#' @param min_points Minimum number of measured points in each segment.
#'
#' @return a list with the breakpoints `psi`, the `coefficients`, `x`, `y`, and `flat_first`.
#' @keywords internal
fit_breakpoints <- function(x, y, n_breakpoints, flat_first = FALSE, min_points = 2) {
  n <- length(x)
  if (n < min_points * (n_breakpoints + 1)) {
    stop(sprintf(
      "At least %d measured points are needed to estimate %d breakpoint(s).",
      min_points * (n_breakpoints + 1), n_breakpoints
    ), call. = FALSE)
  }

  rss <- function(psi) {
    counts <- tabulate(findInterval(x, psi) + 1, n_breakpoints + 1)
    if (is.unsorted(psi, strictly = TRUE) || any(counts < min_points)) return(Inf)
    sum(stats::.lm.fit(breakpoint_design(x, psi, flat_first), y)$residuals^2)
  }

  ## a coarse grid finds the basin of the global optimum; the refinement below makes it exact
  candidates <- seq(x[min_points], x[n - min_points + 1], length.out = if (n_breakpoints == 1) 100 else 50)
  grid <- as.matrix(expand.grid(rep(list(candidates), n_breakpoints)))
  if (n_breakpoints == 2) grid <- grid[grid[, 1] < grid[, 2], , drop = FALSE]
  start <- grid[which.min(apply(grid, 1, rss)), ]

  psi <- if (n_breakpoints == 1) {
    step <- diff(candidates[1:2])
    stats::optimize(rss, c(start - step, start + step), tol = 1e-8)$minimum
  } else {
    stats::optim(start, rss, control = list(reltol = 1e-12))$par
  }
  if (!(rss(psi) <= rss(start))) psi <- start

  list(
    psi = unname(psi),
    coefficients = stats::.lm.fit(breakpoint_design(x, psi, flat_first), y)$coefficients,
    x = x,
    y = y,
    flat_first = flat_first
  )
}

#' @rdname fit_breakpoints
#' @param psi The breakpoints.
breakpoint_design <- function(x, psi, flat_first = FALSE) {
  hinges <- vapply(psi, function(p) pmax(x - p, 0), numeric(length(x)))
  cbind(1, if (!flat_first) x, matrix(hinges, nrow = length(x)))
}

#' @rdname fit_breakpoints
#' @param fit A fit from `fit_breakpoints()`.
#' @param newx Where to evaluate the fitted lines.
predict_breakpoints <- function(fit, newx) {
  drop(breakpoint_design(newx, fit$psi, fit$flat_first) %*% fit$coefficients)
}
