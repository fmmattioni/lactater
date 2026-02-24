# Helper - Log-Poly-ModDmax

For internal use only and it won't be exported.

## Usage

``` r
helper_dmax_log_poly_moddmax(
  data_prepared,
  results_loglog,
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

- results_loglog:

  Results from the Log-log method using polynomial fit.

- data_prepared_dmax:

  The prepared polynomial fit starting at Log-log intensity.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

- plot:

  A boolean to indicate whether to generate a plot from each one of the
  methods.

- model_heart_rate:

  A pre-fit heart-rate model.
