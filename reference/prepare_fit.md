# Prepare fit

This is an obligatory step before applying any lactate threshold method.
This function will model the raw data as well as make all the necessary
data wrangling.

## Usage

``` r
prepare_fit(
  .data,
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline", "Exponential"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming")
)
```

## Arguments

- .data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- fit:

  The fit you would like to use for finding the lactate values
  associated to each one of the lactate thresholds. Please, note that a
  few lactate thresholds have default methods for this and cannot be
  changed. See `Details`.

- include_baseline:

  A boolean to indicate whether to include the baseline value in the
  fit.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

## Value

a tibble with the following nested columns:

- data:

  The raw data.

- data_interpolated:

  The interpolated data.

- model:

  The model chosen in the `fit` parameter.

- data_augmented:

  The augmented data retrieved from the model.

- bsln:

  A boolean indicating the `include_baseline` argument.
