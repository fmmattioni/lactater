# The lactate curves and training zones of a few tests

`plot_series()` draws every lactate curve (the latest test in blue, the
first in dark gray, the tests between in light gray), then the heart
rate of the stages in the same colors with LT1 and LT2 as filled points
(when the tests have it), above the five zones of each test as one bar
that ends at its highest stage, on one intensity axis. `series_zones()`
gives the five zones of each test, and names the test when a threshold
is missing. `date_labels()` names tests by date ("3 Nov 2026") or by day
("Day 58"). For internal use only.

## Usage

``` r
plot_series(tests, labels, lt1, lt2)

series_zones(tests, labels, lt1, lt2)

date_labels(day)
```

## Arguments

- tests:

  Results of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md),
  oldest first.

- labels:

  The name of each test, from `date_labels()`.

- lt1, lt2:

  The thresholds that mark LT1 and LT2, as in
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md).

- day:

  Dates, or days since the first test.
