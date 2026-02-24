# Retrieve intensity

Retrieves the intensity associated to a given lactate value. It is
mainly for internal use but it is exported for possible extensions.

## Usage

``` r
retrieve_intensity(data_augmented, fit, model, lactate_value)
```

## Arguments

- data_augmented:

  The augmented data from the model.

- fit:

  The fit you would like to use for finding the lactate values
  associated to each one of the lactate thresholds.

- model:

  The model chosen in `fit`.

- lactate_value:

  The lactate value.

## Value

the intensity associated with the estimated lactate threshold
