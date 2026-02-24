# Retrieve heart rate

Retrieves the heart rate associated to a given intensity value. It is
mainly for internal use but it is exported for possible extensions.

## Usage

``` r
retrieve_heart_rate(raw_data, intensity_value, model_heart_rate = NULL)
```

## Arguments

- raw_data:

  The raw data.

- intensity_value:

  The intensity value.

- model_heart_rate:

  A pre-fit heart-rate model. If `NULL`, it will be fit from `raw_data`.

## Value

the heart rate associated with the estimated lactate threshold
