# Bootstrap intervals

Wild bootstrap around the user-selected curve: each replicate keeps the
fitted values and adds to each stage its own residual, scaled by
`1 / sqrt(1 - leverage)` and widened by `qt(level) / qnorm(level)` for
the n - p degrees of freedom of the curve, with a random sign
(Rademacher weights; Wu, 1986; Davidson & Flachaire, 2008). Residuals
never move between stages, because the error of a lactate sample grows
with its concentration. The widening brought the coverage of 95%
intervals from 75-93% to 84-97% in `data-raw/bootstrap_coverage.R`.
Davidson & Flachaire (2008) used residuals divided by `1 - leverage`
(HC3), which they found made little difference; in that simulation it
gave intervals two to three times too wide for Bsln+ (coverage 95-99%).
A resting sample outside the fit gets the residual size of the first
stage. Every method is then run again on the replicate. The interval is
the percentile interval of the replicate intensities. For internal use
only.

## Usage

``` r
bootstrap_intervals(
  data,
  settings,
  method,
  curve,
  n_rows,
  n_boot,
  level,
  seed = NULL
)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- settings:

  The analysis settings.

- method:

  The method categories to run.

- curve:

  The user-selected curve fitted to the original data.

- n_rows:

  Number of thresholds the methods return.

- n_boot:

  Number of replicates.

- level:

  Confidence level.

- seed:

  A seed for the replicates, or `NULL` to draw from the current random
  stream.

## Value

a matrix with one row per threshold and the columns `low` and `high`.
