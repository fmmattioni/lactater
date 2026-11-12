# The leave-one-stage-out grid

One row per method and one column per stage left out, red where the
threshold rises without that stage and blue where it falls, darker the
further it moves, with the move written in the cells where it is at
least 3% of the threshold. For internal use only.

## Usage

``` r
leave_one_out_panel(object, colours, sport)
```

## Arguments

- object:

  The result of
  [`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md).

- colours:

  The plot colours.

- sport:

  The sport.
