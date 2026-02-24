# Onset of Blood Lactate Accumulation (OBLA)

It applies the `OBLA` methods: `OBLA 2.0 mmol/L`, `OBLA 2.5 mmol/L`,
`OBLA 3.0 mmol/L`, `OBLA 3.5 mmol/L`, and `OBLA 4.0 mmol/L`.

## Usage

``` r
method_obla(data_prepared, fit, sport, plot)
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
