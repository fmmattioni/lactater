# Individual Anaerobic Threshold (IAT)

It applies the `IAT` method: the intensity at which the B-spline curve
is `settings$iat_rise` (1.5 mmol/L by default) above the lactate at
LTratio (Dickhuth et al., 1999).

## Usage

``` r
method_iat(data, curve, settings)
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
