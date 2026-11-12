# Lactate turn points (LTP)

It applies the `LTP` methods: `LTP1`, and `LTP2`. A three-segment
continuous linear regression is fitted to the measured points; the two
breakpoints with the lowest residual sum of squares are found by
exhaustive search (see
[`fit_breakpoints()`](https://fmmattioni.github.io/lactater/reference/fit_breakpoints.md)).
With `ltp_fix_first_slope` the first segment is flat. Hofmann et al.
(1997) and Pokan et al. (1997) fitted each turn point with its own
two-line regression inside a window, on tests with 1-min or 90-s stages;
with the 6 to 10 stages of a typical step test those windows hold too
few stages, so the three segments are fitted together. With
`settings$ltp_breakpoints = 1` (a submaximal test) two segments are
fitted, the breakpoint is LTP1 and LTP2 is `NA`. The lactate at each
turn point is read off the fitted segments. The resting sample is never
used: it has no exercise intensity.

## Usage

``` r
method_ltp(data, curve, settings)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- curve:

  The user-selected curve from
  [`fit_curve()`](https://fmmattioni.github.io/lactater/reference/fit_curve.md).

- settings:

  The analysis settings.

## Value

a list with the result `rows` (method, fitting, intensity, lactate) and
the `plot_data` for each row.
