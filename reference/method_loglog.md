# Log-log

It applies the `Log-log` method.

## Usage

``` r
method_loglog(data_prepared, fit, sport, loglog_restrainer = 1, plot)
```

## Arguments

- data_prepared:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- fit:

  The fit you would like to use for finding the lactate values
  associated to each one of the lactate thresholds.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

- loglog_restrainer:

  A scalar from 0 to 1 indicating the percentage of the data that you
  would like to restrain for fitting the Log-Log method. For example,
  `1` means no restriction (fits using the whole data), and `0.5` means
  that only the first 50% of the data will be used. Default to `1`.

- plot:

  A boolean to indicate whether to generate a plot from each one of the
  methods.

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
