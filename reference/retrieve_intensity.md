# Retrieve intensity

Retrieves the intensity at which the fitted lactate curve reaches a
given lactate value, by solving the fitted curve. When the curve crosses
the value more than once (an early lactate dip, or a plateau at the
end), the last crossing where lactate rises through the value is used:
from there on, lactate stays above it.

## Usage

``` r
retrieve_intensity(curve, lactate_value)
```

## Arguments

- curve:

  A curve from
  [`fit_curve()`](https://fmmattioni.github.io/lactater/reference/fit_curve.md).

- lactate_value:

  The lactate value.

## Value

the intensity associated with the estimated lactate threshold, or `NA`
when the curve never rises through `lactate_value` within the tested
range.
