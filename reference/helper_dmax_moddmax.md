# Helper - ModDmax

For internal use only and it won't be exported.

## Usage

``` r
helper_dmax_moddmax(
  data_prepared,
  data_first_rise,
  data_prepared_dmax,
  sport,
  plot,
  model_heart_rate = NULL
)
```

## Arguments

- data_prepared:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- data_first_rise:

  The first rise in lactate data point used for ModDmax.

- data_prepared_dmax:

  The prepared polynomial fit for ModDmax.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

- plot:

  A boolean to indicate whether to generate a plot from each one of the
  methods.

- model_heart_rate:

  A pre-fit heart-rate model.
