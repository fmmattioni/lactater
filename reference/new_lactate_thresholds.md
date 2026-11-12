# Build a lactate threshold result

Adds the class and keeps what plotting needs (data, curve, settings,
unrounded intensities). For internal use only.

## Usage

``` r
new_lactate_thresholds(
  out,
  data,
  curve,
  settings,
  plot_data,
  intensity,
  intervals = NULL
)
```

## Arguments

- out:

  The result tibble.

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- curve:

  The user-selected curve.

- settings:

  The analysis settings.

- plot_data:

  What each row's plot needs.

- intensity:

  The unrounded threshold intensities.

- intervals:

  The unrounded confidence intervals (a `low`/`high` matrix), or `NULL`.
