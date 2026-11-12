# Fit a lactate curve

Fits the lactate curve used by the threshold methods. For internal use
only.

## Usage

``` r
fit_curve(
  points,
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline", "Exponential")
)
```

## Arguments

- points:

  A data frame with `intensity` and `lactate`.

- fit:

  The curve type.

## Value

a list with the `fit` type, the `points`, the `domain` (intensity range
of the points), the `coefficients`, `predict`, a function returning the
fitted lactate at given intensities, and, for the fits that are linear
in their parameters, the `leverage` of each point.

## Details

- `"3rd degree polynomial"` and `"4th degree polynomial"`: ordinary
  least squares on raw powers.

- `"B-spline"`: a natural cubic regression spline with 4 degrees of
  freedom ([`splines::ns()`](https://rdrr.io/r/splines/ns.html)).

- `"Exponential"`: the exponential plus constant `a + b * exp(c * x)`
  (Hughson et al., 1987). For a fixed `c` the model is linear in `a` and
  `b`, so only `c` is searched (variable projection), and only "in the
  positive range" as in Hughson et al.: no starting values are needed
  and the fit cannot fail to converge.
