# Points along a fitted curve

Points along a fitted curve

## Usage

``` r
curve_points(curve, from = curve$domain[1], n = 200)
```

## Arguments

- curve:

  A curve from
  [`fit_curve()`](https://fmmattioni.github.io/lactater/reference/fit_curve.md).

- from:

  Where to start (default: the start of the curve's domain).

- n:

  Number of points.

## Value

a data frame with `intensity` and `lactate`.
