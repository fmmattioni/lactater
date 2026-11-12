# Data used for modeling

The exercise stages, plus the positioned resting sample when
`include_baseline = TRUE`. The prepared data keeps the resting sample at
intensity 0; only the fit places it one step before stage 1 (see
[`position_rest()`](https://fmmattioni.github.io/lactater/reference/position_rest.md)).

## Usage

``` r
modeling_data(.data, include_baseline)
```

## Arguments

- .data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- include_baseline:

  Whether to include the resting sample.
