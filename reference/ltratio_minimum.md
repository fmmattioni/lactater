# LTratio minimum

LTratio minimum

## Usage

``` r
ltratio_minimum(data, curve, settings)
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

a list with the B-spline `spline`, the `intensity` of the minimum ratio,
its value `ratio_min`, and the `ratio` curve over the exercise stages.
