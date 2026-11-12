# Overview plot

Every threshold of the test on one intensity axis: the fitted curve on
top, and below it one row per method, grouped by what it aims at (the
first rise, the second rise, or a fixed lactate), with the threshold as
a dot, its value beside it, and its confidence interval as a line (when
computed). Two shaded bands, through the curve and the rows, show how
far the methods aimed at LT1 and at LT2 lie apart. The title gives LT1
and LT2 when both `chosen` methods were computed. A table on the right
prints the lactate, heart rate, RPE, percentage of the peak intensity
and of the highest heart rate at each threshold, with the lactate shaded
by its value.

## Usage

``` r
plot_overview(object, info, chosen = character())
```

## Arguments

- object:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- info:

  Its `lactater` attribute.

- chosen:

  The methods that mark LT1 and LT2, drawn in green.
