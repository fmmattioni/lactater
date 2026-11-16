# Prepare raw data

It organizes and renames the raw data.

## Usage

``` r
prepare_data(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column = NULL,
  rpe_column = NULL,
  rpe_scale = "6-20"
)
```

## Arguments

- .data:

  The raw data.

- intensity_column:

  The name of the intensity column, as a string.

- lactate_column:

  The name of the lactate column, as a string.

- heart_rate_column:

  The name of the heart rate column, as a string, or `NULL`.

- rpe_column:

  The name of the rating of perceived exertion column, as a string, or
  `NULL`.

- rpe_scale:

  The RPE scale, `"6-20"` or `"CR10"`, used to check the values.

## Value

a tibble with the following columns:

- intensity:

  The intensity column.

- lactate:

  The lactate column.

- heart_rate:

  The heart rate column, if applicable.

- rpe:

  The rating of perceived exertion column, if applicable.
