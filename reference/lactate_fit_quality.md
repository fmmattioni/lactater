# Goodness of fit of the lactate curves

Fits every available curve to the same points and reports how well each
follows the measured lactate, to help choose `fit` in
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).
The statistics are computed on the measured stages (plus the resting
sample when `include_baseline = TRUE`).

## Usage

``` r
lactate_fit_quality(x)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

## Value

a tibble with one row per curve: `fit`, `used` (the curve chosen in
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)),
`n_points`, `parameters`, `r_squared`, `rmse` and `see` (standard error
of the estimate) in mmol/L, and `max_abs_residual` and the leave-one-out
`prediction_error` (root-mean-square) in mmol/L.

## Details

A curve with more parameters always follows the points more closely, so
the RMSE favors it. The prediction error does not: each stage is
predicted by the curve fitted without it, which punishes a curve that
bends to follow single points (leave-one-out cross-validation).

## Examples

``` r
results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
lactate_fit_quality(results)
#> # A tibble: 4 × 9
#>   fit         used  n_points parameters r_squared   rmse    see max_abs_residual
#>   <chr>       <lgl>    <int>      <int>     <dbl>  <dbl>  <dbl>            <dbl>
#> 1 3rd degree… TRUE         7          4     1.000 0.0488 0.0746           0.0758
#> 2 4th degree… FALSE        7          5     1.000 0.0433 0.0810           0.0703
#> 3 B-spline    FALSE        7          5     1.000 0.0223 0.0417           0.0364
#> 4 Exponential FALSE        7          3     1.000 0.0563 0.0745           0.100 
#> # ℹ 1 more variable: prediction_error <dbl>
```
