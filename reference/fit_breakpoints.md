# Fit a continuous piecewise-linear model by exhaustive breakpoint search

For fixed breakpoints the model
`y = b0 + b1 x + sum(c_j max(x - psi_j, 0))` is linear, so the residual
sum of squares can be computed for every candidate. All breakpoint
positions on a grid are tried and the best one is refined locally.
Unlike iterative fits (e.g. `segmented`), the result is the global
least-squares optimum (up to the grid) and is deterministic. For
internal use only.

## Usage

``` r
fit_breakpoints(x, y, n_breakpoints = 2, flat_first = FALSE, min_points = 2)

breakpoint_design(x, psi, flat_first = FALSE)

predict_breakpoints(fit, newx)
```

## Arguments

- x, y:

  The measured points.

- n_breakpoints:

  Number of breakpoints (1 or 2).

- flat_first:

  Whether the first segment is flat (slope fixed at 0).

- min_points:

  Minimum number of measured points in each segment.

- psi:

  The breakpoints.

- fit:

  A fit from `fit_breakpoints()` or
  [`fit_two_lines()`](https://fmmattioni.github.io/lactater/reference/fit_two_lines.md).

- newx:

  Where to evaluate the fitted lines.

## Value

a list with the breakpoints `psi`, the `coefficients`, `x`, `y`, and
`flat_first`.
