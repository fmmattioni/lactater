# OBLA helper

For internal use only and it won't be exported.

## Usage

``` r
obla_helper(
  raw_data,
  data_augmented,
  fit,
  model,
  obla,
  model_heart_rate = NULL
)
```

## Arguments

- raw_data:

  The raw data.

- data_augmented:

  The augmented data from the model.

- fit:

  The fit you would like to use for finding the lactate values
  associated to each one of the lactate thresholds.

- model:

  The model chosen in `fit`,

- obla:

  The lactate value to be analyzed.

- model_heart_rate:

  A pre-fit heart-rate model.

## Value

The results (intensity, lactate, heart rate) from the lactate threshold
method.
