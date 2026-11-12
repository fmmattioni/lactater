# Log-log

It applies the `Log-log` method. Two straight lines are fitted to
log(lactate) vs log(intensity), split at the measured stage with the
lowest total residual sum of squares, and the threshold is where they
cross (see
[`fit_two_lines()`](https://fmmattioni.github.io/lactater/reference/fit_two_lines.md)).
The lactate at the threshold is read off the lines. The resting sample
is never used: it has no exercise intensity.

## Usage

``` r
method_loglog(data, curve, settings)
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
