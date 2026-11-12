# Shift of the whole lactate curve between two tests

How far the fitted lactate curve moved between two tests: the intensity
gained at each lactate value, and, when `intensity` is given, the
lactate saved at each intensity. A rightward shift (more intensity for
the same lactate) is the classic sign of better endurance; check the
peak lactate too (see
[`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)).

## Usage

``` r
curve_shift(before, after, lactate = seq(1.5, 4, by = 0.5), intensity = NULL)
```

## Arguments

- before, after:

  The results of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  for the two tests, with the same sport.

- lactate:

  Lactate values (mmol/L) at which to compare the intensity. Values
  outside either curve are `NA`. Defaults to `seq(1.5, 4, by = 0.5)`.

- intensity:

  Intensities at which to compare the lactate, or `NULL`.

## Value

a tibble with `lactate`, `intensity_before`, `intensity_after` and
`shift`; with `intensity`, rows with `intensity`, `lactate_before`,
`lactate_after` and `lactate_change` are added.

## Examples

``` r
series <- subset(lactate_tests, athlete == "cyclist_series")
before <- lactate_threshold(subset(series, day == 0), intensity, lactate, plot = FALSE)
after <- lactate_threshold(subset(series, day == 58), intensity, lactate, plot = FALSE)
curve_shift(before, after)
#> # A tibble: 6 × 4
#>   lactate intensity_before intensity_after shift
#>     <dbl>            <dbl>           <dbl> <dbl>
#> 1     1.5              262             283    20
#> 2     2                282             301    19
#> 3     2.5              296             316    19
#> 4     3                308             328    20
#> 5     3.5              318             338    20
#> 6     4                327             348    21
```
