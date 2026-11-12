# Retrieve lactate

Retrieves the fitted lactate at given intensities, rounded to 0.1
mmol/L.

## Usage

``` r
retrieve_lactate(curve, intensity_value)
```

## Arguments

- curve:

  A curve from
  [`fit_curve()`](https://fmmattioni.github.io/lactater/reference/fit_curve.md).

- intensity_value:

  The intensity values.

## Value

the lactate associated with the estimated lactate threshold
