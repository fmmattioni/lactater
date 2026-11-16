# Retrieve RPE

The rating of perceived exertion at given intensities, interpolated
linearly between the measured stages like the heart rate, to 0.1. For
internal use only.

## Usage

``` r
retrieve_rpe(raw_data, intensity_value)
```

## Arguments

- raw_data:

  The raw data, with a `heart_rate` column.

- intensity_value:

  The intensity values.

## Value

the RPE at each intensity.
