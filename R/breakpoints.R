#' Fit a continuous piecewise-linear model by exhaustive breakpoint search
#'
#' For fixed breakpoints the model `y = b0 + b1 x + sum(c_j max(x - psi_j, 0))` is linear, so the
#' residual sum of squares can be computed for every candidate. All breakpoint positions on a grid are
#' tried and the best one is refined locally. Unlike iterative fits (e.g. `segmented`), the result is the
#' global least-squares optimum (up to the grid) and is deterministic. For internal use only.
#'
#' @param x,y The measured points.
#' @param n_breakpoints Number of breakpoints (1 or 2).
#' @param flat_first Whether the first segment is flat (slope fixed at 0).
#' @param min_points Minimum number of measured points in each segment.
#'
#' @return a list with the breakpoints `psi`, the `coefficients`, `x`, `y`, and `flat_first`.
#' @keywords internal
fit_breakpoints <- function(x, y, n_breakpoints = 2, flat_first = FALSE, min_points = 2) {
  n <- length(x)
  needed <- min_points * (n_breakpoints + 1)
  if (n < needed) {
    stop(
      sprintf("At least %d measured points are needed to estimate %d breakpoint(s).", needed, n_breakpoints),
      call. = FALSE
    )
  }

  rss <- function(psi) {
    ## the optimizer can propose breakpoints out of order; findInterval() needs them sorted
    if (anyNA(psi) || is.unsorted(psi, strictly = TRUE)) return(Inf)
    counts <- tabulate(findInterval(x, psi) + 1, n_breakpoints + 1)
    if (any(counts < min_points)) return(Inf)
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
#' @param fit A fit from `fit_breakpoints()` or `fit_two_lines()`.
#' @param newx Where to evaluate the fitted lines.
predict_breakpoints <- function(fit, newx) {
  if (!is.null(fit$lines)) {
    ## an integer index, so a missing breakpoint gives one NA per value (a logical NA would recycle)
    line <- 1L + (newx > fit$psi)
    return(fit$lines[line, 1] + fit$lines[line, 2] * newx)
  }
  drop(breakpoint_design(newx, fit$psi, fit$flat_first) %*% fit$coefficients)
}

#' Fit two independent straight lines that share a division point
#'
#' Each measured point in turn is the division point, which belongs to both segments (Beaver et al., 1985).
#' Each segment gets its own least-squares line, and the division with the lowest total residual sum of
#' squares wins (Jamnick et al., 2018). The breakpoint `psi` is where the two lines cross. For internal use
#' only.
#'
#' @param x,y The measured points, sorted by `x`.
#' @param min_points Minimum number of points per line, the division point included. A line through
#'   two points has no residuals, so fewer than three would favor the shortest segments.
#'
#' @return a list with the breakpoint `psi` (`NA` when the lines cross outside the range of `x`), the
#'   intercept and slope of each line (`lines`, one row per line), `x`, and `y`.
#' @keywords internal
fit_two_lines <- function(x, y, min_points = 3) {
  n <- length(x)
  if (n < 2 * min_points - 1) {
    stop(sprintf("At least %d measured points are needed to fit two lines.", 2 * min_points - 1), call. = FALSE)
  }

  line <- function(i) stats::.lm.fit(cbind(1, x[i]), y[i])
  divisions <- lapply(min_points:(n - min_points + 1), function(k) list(line(1:k), line(k:n)))
  rss <- vapply(divisions, function(d) sum(d[[1]]$residuals^2, d[[2]]$residuals^2), numeric(1))
  best <- divisions[[which.min(rss)]]
  lines <- rbind(best[[1]]$coefficients, best[[2]]$coefficients)

  psi <- (lines[2, 1] - lines[1, 1]) / (lines[1, 2] - lines[2, 2])
  if (!is.finite(psi) || psi < x[1] || psi > x[n]) psi <- NA_real_
  list(psi = psi, lines = lines, x = x, y = y)
}
