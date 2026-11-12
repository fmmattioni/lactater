# Minimum Lactate-Intensity Ratio (LTratio)

It applies the `LTratio` method: the intensity where lactate /
intensity, on a B-spline fit, is lowest. The search runs over the
exercise stages only.

## Usage

``` r
method_ltratio(data, curve, settings)
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
