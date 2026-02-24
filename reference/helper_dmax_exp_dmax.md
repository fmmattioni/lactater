# Helper - Exp-Dmax

For internal use only and it won't be exported.

## Usage

``` r
helper_dmax_exp_dmax(
  data_prepared_dmax,
  data_raw,
  sport,
  plot,
  model_heart_rate = NULL
)
```

## Arguments

- data_prepared_dmax:

  The prepared exponential fit for Exp-Dmax.

- data_raw:

  The raw data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

- plot:

  A boolean to indicate whether to generate a plot from each one of the
  methods.

- model_heart_rate:

  A pre-fit heart-rate model.
