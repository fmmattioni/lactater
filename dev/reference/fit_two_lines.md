# Fit two independent straight lines that share a division point

Each measured point in turn is the division point, which belongs to both
segments (Beaver et al., 1985). Each segment gets its own least-squares
line, and the division with the lowest total residual sum of squares
wins (Jamnick et al., 2018). The breakpoint `psi` is where the two lines
cross. For internal use only.

## Usage

``` r
fit_two_lines(x, y, min_points = 3)
```

## Arguments

- x, y:

  The measured points, sorted by `x`.

- min_points:

  Minimum number of points per line, the division point included. A line
  through two points has no residuals, so fewer than three would favor
  the shortest segments.

## Value

a list with the breakpoint `psi` (`NA` when the lines cross outside the
range of `x`), the intercept and slope of each line (`lines`, one row
per line), `x`, and `y`.
