# Thresholds relative to body mass, peak intensity and peak heart rate

Expresses each threshold so that different athletes can be compared: per
kg of body mass (power only), as a percentage of the highest intensity
of the test, and the heart rate as a percentage of the highest heart
rate of the test. A fixed lactate concentration falls at a similar
percentage of peak power in trained and untrained people (Maassen &
Busse, 1989), so percentages of peak do not rank fitness by themselves.

## Usage

``` r
relative_thresholds(x, body_mass = NULL)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- body_mass:

  Body mass (kg), or `NULL`.

## Value

`x` with `intensity_per_kg` (with `body_mass`, cycling and rowing),
`percent_peak` and, with heart rate, `percent_peak_heart_rate`.

## References

Maassen N, Busse MW. The relationship between lactic acid and work load:
a measure for endurance capacity or an indicator of carbohydrate
deficiency? European Journal of Applied Physiology and Occupational
Physiology. 1989;58(7):728-737.

## Examples

``` r
elite <- subset(lactate_tests, test == "cycling_elite")
results <- lactate_threshold(elite, intensity, lactate, heart_rate, plot = FALSE)
relative_thresholds(results, body_mass = elite$body_mass[1])
#> # Lactate thresholds: cycling, 3rd degree polynomial, 9 stages + rest
#> # LT1:                286 W (Bsln + 0.5)
#> # LT2:                367 W (Log-Poly-ModDmax)
#>    method_category method  fitting intensity lactate heart_rate intensity_per_kg
#>    <fct>           <fct>   <chr>       <dbl>   <dbl>      <dbl>            <dbl>
#>  1 Log-log         Log-log 3rd de…       280     1.5        169             4.17
#>  2 OBLA            OBLA 2… 3rd de…       304     2          174             4.54
#>  3 OBLA            OBLA 2… 3rd de…       320     2.5        178             4.77
#>  4 OBLA            OBLA 3… 3rd de…       332     3          179             4.95
#>  5 OBLA            OBLA 3… 3rd de…       342     3.5        180             5.1 
#>  6 OBLA            OBLA 4… 3rd de…       350     4          181             5.23
#>  7 Bsln+           Bsln +… 3rd de…       286     1.6        170             4.27
#>  8 Bsln+           Bsln +… 3rd de…       308     2.1        175             4.59
#>  9 Bsln+           Bsln +… 3rd de…       322     2.6        178             4.81
#> 10 Dmax            Dmax    3rd de…       322     2.6        178             4.81
#> 11 Dmax            ModDmax 3rd de…       351     4          181             5.24
#> 12 Dmax            Exp-Dm… Expone…       337     3.4        180             5.03
#> 13 Dmax            Log-Po… 3rd de…       367     5.1        183             5.47
#> 14 Dmax            Log-Ex… Expone…       375     5.5        184             5.6 
#> 15 LTP             LTP1    3rd de…       306     1.7        175             4.56
#> 16 LTP             LTP2    3rd de…       377     5.2        185             5.63
#> 17 LTratio         LTratio B-Spli…       219     1.2        155             3.27
#> 18 IAT             IAT     B-Spli…       330     2.7        179             4.92
#> # ℹ 2 more variables: percent_peak <dbl>, percent_peak_heart_rate <dbl>
```
