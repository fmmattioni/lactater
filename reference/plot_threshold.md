# Plot one lactate threshold

Draws what the method fitted, the measured stages, and the threshold
with a two-line label joined by an arrow: the intensity, then the pace,
lactate and heart rate. For internal use only.

## Usage

``` r
plot_threshold(
  method,
  intensity,
  lactate,
  heart_rate,
  plot_data,
  data,
  settings,
  interval = NULL,
  rpe = NA
)
```

## Arguments

- method:

  The method label.

- intensity, lactate, heart_rate, rpe:

  The threshold values.

- plot_data:

  What the method drew on: the fitted `curve` (with a Dmax `line`, a
  Bsln+ `baseline`, or the IAT `from` point), a `breakpoints` fit, or a
  `ratio` curve, and the `fitting` label.

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- settings:

  The analysis settings (`sport`).

- interval:

  The confidence interval of the intensity (`low`, `high`), or `NULL`.

## Value

a `ggplot2` object, or `NA` when the threshold could not be estimated.
