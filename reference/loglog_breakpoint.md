# Log-log breakpoint

Fits two straight lines to log(lactate) vs log(intensity) of the
measured points and returns the unrounded intensity where they cross.
For internal use only.

## Usage

``` r
loglog_breakpoint(points, loglog_restrainer = 1)
```

## Arguments

- points:

  The measured points (`intensity`, `lactate`) used for the fit.

- loglog_restrainer:

  The share of the points to use (see
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)).

## Value

a list with the breakpoint `intensity` and the `fit` from
[`fit_two_lines()`](https://fmmattioni.github.io/lactater/reference/fit_two_lines.md)
(in log-log space).
