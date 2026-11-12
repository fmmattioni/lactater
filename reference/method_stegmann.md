# Individual anaerobic threshold of Stegmann et al. (1981)

It applies the `Stegmann` method: blood lactate is followed into
recovery until it falls back to its end-of-exercise value, and from that
point a tangent is drawn to the lactate curve of the test; the point of
contact is the threshold, and the slope of the tangent the maximal rate
of lactate elimination (Stegmann et al., 1981). Lactate is followed over
time, so recovery minutes are put on the intensity axis at the rate the
test rose (the median increment between stages divided by
`settings$stage_duration`). The time lactate is back at its
end-of-exercise value is interpolated linearly between the recovery
samples after the post-exercise peak.

## Usage

``` r
method_stegmann(data, curve, settings)
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
the `plot_data`.
