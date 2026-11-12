# Retrieve heart rate

Heart rate at given intensities, interpolated linearly between the
measured stages. A straight line through the whole test misses the bend
of the heart-rate curve at high intensities (2-6 bpm on `demo_data`).
Outside the stages (e.g. below the first one) it is `NA`.

## Usage

``` r
retrieve_heart_rate(raw_data, intensity_value)
```

## Arguments

- raw_data:

  The raw data, with a `heart_rate` column.

- intensity_value:

  The intensity values.

## Value

the heart rate (bpm, rounded) at each intensity.
