# Compute thresholds

Fits the user-selected curve and runs the requested methods. For
internal use only.

## Usage

``` r
compute_thresholds(data, settings, method)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- settings:

  The analysis settings.

- method:

  The method categories to run.

## Value

a list with the user `curve`, the result `rows` (with `method_category`)
and the `plot_data` for each row.
