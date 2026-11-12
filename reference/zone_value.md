# Heart rate or RPE at zone boundaries

Interpolated between the stages, like the thresholds; a boundary below
the first stage (e.g. the zone 1\|2 split) is extrapolated from a
straight line through the first three stages, where heart rate and RPE
rise about linearly with intensity. For internal use only.

## Usage

``` r
zone_value(data, column, x, digits, lowest = 0)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- column:

  `"heart_rate"` or `"rpe"`.

- x:

  The boundary intensities.

- digits:

  Decimals.

- lowest:

  The lowest value the measure can take.
