# Measured stages and the resting sample

The stages as open dots; the resting sample, placed one step before the
first stage, fainter and labeled. For internal use only.

## Usage

``` r
stage_layers(points, data, colours, label_rest = TRUE, rest_below = FALSE)
```

## Arguments

- points:

  The stages to draw (`intensity`, `lactate`), with the resting sample
  positioned.

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md),
  to tell the resting sample apart.

- colours:

  The plot colours.

- label_rest:

  Whether to label the resting sample.

- rest_below:

  Whether that label goes under the dot instead of over it.
