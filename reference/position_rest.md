# Position the resting sample

The resting sample (intensity 0) is placed one step before the first
stage, so it can be plotted next to the curve and, with
`include_baseline = TRUE`, fitted. The step is the median step between
stages, so a missed stage or a short last stage does not shift it.

## Usage

``` r
position_rest(.data)
```

## Arguments

- .data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

## Value

`.data` with the resting intensity replaced; unchanged when there is no
resting sample.
