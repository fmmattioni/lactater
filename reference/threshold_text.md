# Threshold label

Threshold label

## Usage

``` r
threshold_text(
  intensity,
  lactate,
  heart_rate,
  sport,
  interval = NULL,
  rpe = NA
)
```

## Arguments

- intensity, lactate, heart_rate, rpe:

  The threshold values.

- sport:

  The sport, which sets the intensity unit.

- interval:

  The confidence interval of the intensity (`low`, `high`), or `NULL`.

## Value

a list with the `value` (the intensity, e.g. `"140 W"`) and the `detail`
line (the interval, pace, lactate and heart rate, joined with middle
dots).
