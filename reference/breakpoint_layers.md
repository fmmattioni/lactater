# Layers of a breakpoint method

The fitted straight lines (solid where they were fitted, dashed beyond),
and for LTP the other turn point. For internal use only.

## Usage

``` r
breakpoint_layers(fit, method, intensity, log_axes, colours, sport)
```

## Arguments

- fit:

  A fit from
  [`fit_breakpoints()`](https://fmmattioni.github.io/lactater/reference/fit_breakpoints.md)
  or
  [`fit_two_lines()`](https://fmmattioni.github.io/lactater/reference/fit_two_lines.md).

- method:

  The method label.

- log_axes:

  Whether the fit is in log-log space.

- colours:

  The plot colours.

- sport:

  The sport, for the unit of the other turn point.

## Value

a list with the `layers`, the lactate `y` where the threshold sits, and
the `top`.
