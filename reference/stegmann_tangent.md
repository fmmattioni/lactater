# The Stegmann tangent

The Stegmann tangent

## Usage

``` r
stegmann_tangent(data, curve, settings)
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

a list with the contact `intensity` (or `NA` when recovery lactate never
fell back to its end-of-exercise value after a rise), the `back` time
(minutes after exercise), the point `from` which the tangent is drawn,
the `recovery` samples placed on the intensity axis, and the maximal
`elimination` rate (mmol/L/min).
