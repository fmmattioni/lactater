# Prepare raw data

It organizes and renames the raw data.

## Usage

``` r
prepare_data(.data, intensity_column, lactate_column, heart_rate_column)
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

## Value

a tibble with the following columns:

- intensity:

  The intensity column.

- lactate:

  The lactate column.

- heart_rate:

  The heart rate column, if applicable.
