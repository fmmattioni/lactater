# Dmax

It applies the `Dmax` methods: `"Dmax"`, `"ModDmax"`, `"Exp-Dmax"`,
`"Log-Poly-ModDmax"`, and `"Log-Exp-ModDmax"`.

## Usage

``` r
method_dmax(data_prepared, sport, loglog_restrainer = 1, plot)
```

## Arguments

- data_prepared:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

- loglog_restrainer:

  A scalar from 0 to 1 indicating the percentage of the data that you
  would like to restrain for fitting the Log-Log method - This is going
  to be used in the `Log-Poly-ModDmax` and `Log-Exp-ModDmax` methods
  only. For example, `1` means no restriction (fits using the whole
  data), and `0.5` means that only the first 50% of the data will be
  used. Default to `1`.

- plot:

  A boolean to indicate whether to generate a plot from each one of the
  methods.

  ### Fit

  The `method_dmax()` function does not have a `fit` argument because
  all the `Dmax` methods have their own default fitting methods:

  - Dmax = 3rd degree polynomial

  - ModDmax = 3rd degree polynomial

  - Exp-Dmax = exponential

  - Log-Poly-ModDmax = 3rd degree polynomial

  - Log-Exp-ModDmax = exponential

## Value

a tibble with the following columns:

- method:

  the method used to estimate the lactate threshold

- fitting:

  the fitting method used to predict the lactate curve

- intensity:

  the intensity associated with the estimated lactate threshold

- lactate:

  the lactate concentration associated with the estimated lactate
  threshold

- heart_rate:

  the heart rate associated with the estimated lactate threshold

- plot:

  the plot produced to display the lactate threshold
