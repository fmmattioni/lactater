# Check and tidy the recovery samples

Check and tidy the recovery samples

## Usage

``` r
prepare_recovery(recovery, stage_duration)
```

## Arguments

- recovery:

  The `recovery` argument of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- stage_duration:

  The `stage_duration` argument of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

## Value

a data frame with `time` and `lactate`, ordered by time, without missing
samples.
