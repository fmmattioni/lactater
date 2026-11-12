# Bsln+

It applies the `Bsln+` methods: the intensity at which the fitted curve
reaches each rise in `settings$baseline_plus` (0.5, 1.0, and 1.5 mmol/L
by default) above the baseline. The baseline is the first sample: the
resting sample when there is one, otherwise the first stage.

## Usage

``` r
method_bsln_plus(data, curve, settings)
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
