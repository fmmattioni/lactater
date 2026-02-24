# Lactate curve

It retrieves the lactate curve for plotting purposes.

## Usage

``` r
lactate_curve(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column,
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming")
)
```

## Arguments

- .data:

  The raw data.

- intensity_column:

  The name of the intensity column.

- lactate_column:

  The name of the lactate column.

- heart_rate_column:

  The name of the heart rate column, if applicable.

- fit:

  The fit you would like to use for plotting the lactate curve. Options
  are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline`.

- include_baseline:

  A boolean to indicate whether to include the baseline value in the
  fit.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

## Value

a list with the following elements:

- data:

  a tibble containing the raw data with the columns `intensity`,
  `lactate`, and `heart_rate`.

- lactate_curve:

  a tibble containing the data with the columns `intensity` and
  `lactate` for plotting the lactate curve according to the `fit` method
  chosen.

- heart_rate_response:

  a tibble containing the data with the columns `intensity` and
  `heart_rate` for plotting the heart rate response using the linear
  method.

## Examples

``` r
if (FALSE) { # \dontrun{
lactate_curve(
  .data = demo_data,
  intensity_column = "intensity",
  lactate_column = "lactate",
  heart_rate_column = "heart_rate",
  fit = "3rd degree polynomial",
  include_baseline = TRUE,
  sport = "cycling"
)
} # }
```
