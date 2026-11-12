# Onset of Blood Lactate Accumulation (OBLA)

It applies the `OBLA` methods: the intensity at which the fitted curve
reaches each fixed concentration in `settings$fixed` (2.0, 2.5, 3.0,
3.5, and 4.0 mmol/L by default).

## Usage

``` r
method_obla(data, curve, settings)
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
