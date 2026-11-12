# Maximal lactate steady state from constant-load bouts

Finds the maximal lactate steady state (MLSS) from two or more
constant-load bouts of at least 30 min on separate days: a bout is
steady when blood lactate rises by no more than 1.0 mmol/L from minute
10 to minute 30, and the MLSS is the highest steady bout. Heck et al.
(1985), who introduced the test and the term, allowed a rise of 1 mmol/L
over the last 20 min of 3 + 25-min runs; the minute 10 to 30 window is
that of Beneke (2003). Against the highest steady oxygen uptake, a
window from minute 15 or 20 to minute 30 was more accurate (Iannetta et
al., 2022); set it with `from`. With the result of
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
for a step test of the same athlete, it also gives how far each
threshold lies from the MLSS, your own check of which threshold
estimates it.

## Usage

``` r
lactate_mlss(bouts, x = NULL, rise = 1, from = 10)

# S3 method for class 'lactate_mlss'
autoplot(object, ...)
```

## Arguments

- bouts:

  A data frame with one row per sample: `intensity` (the load of the
  bout, in the unit of the step test), `time` (minutes from the start of
  the bout) and `lactate` (mmol/L).

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  for a step test of the same athlete, or `NULL`.

- rise:

  The largest rise in lactate (mmol/L) from minute `from` to minute 30
  of a steady bout. Defaults to `1`.

- from:

  The minute the rise is measured from, to minute 30. Defaults to `10`
  (Beneke, 2003). In 10 men cycling, the MLSS from minute 10 lay 8 W
  below the highest steady oxygen uptake on average, and from minute
  `15` or `20` 2 W above it, where oxygen uptake was steady in 7 of the
  10 rather than 9 (Iannetta et al., 2022).

- object:

  The result of `lactate_mlss()`.

- ...:

  Not used.

## Value

a `lactate_mlss` list with `mlss` (the highest steady bout, `NA` when
none was steady), `next_bout` (the lowest bout above it, which was not
steady, or `NA`), `bouts` (a tibble with one row per bout: `intensity`,
`lactate_from` (at minute `from`), `lactate_30`, `rise`, `minutes` (the
last sample), and `steady`), the `samples`, `rise` and `from`, and, with
`x`, `thresholds` (a tibble with `method`, `intensity`, and the `gap` to
the MLSS in intensity units and in %, positive when the threshold lies
above it). Plot it with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html);
the article [Beyond the step
test](https://fmmattioni.github.io/lactater/articles/beyond-the-step-test.html)
shows it.

## Details

The lactate at minute `from` and at minute 30 is interpolated linearly
between the samples, so sample at or around both. A bout that ended
before minute 30 (exhaustion) is not steady. The true MLSS lies between
the highest steady bout and the next bout up, so bouts closer together
pin it down better. Thresholds against a measured MLSS land from about
40 W below to 40 W above it on average, depending on the method and the
protocol (Hauser et al., 2014; Ploszczyca et al., 2020). Whether the
MLSS or the critical power marks the boundary between the heavy and
severe domains is debated (Keir et al., 2015; Mattioni Maturana et al.,
2016; Jones et al., 2019).

## References

Beneke R. Methodological aspects of maximal lactate steady state -
implications for performance testing. European Journal of Applied
Physiology. 2003;89(1):95-99. doi:10.1007/s00421-002-0783-1

Hauser T, Adam J, Schulz H. Comparison of selected lactate threshold
parameters with maximal lactate steady state in cycling. International
Journal of Sports Medicine. 2014;35(6):517-521.

Heck H, Mader A, Hess G, Mücke S, Müller R, Hollmann W. Justification of
the 4-mmol/l lactate threshold. International Journal of Sports
Medicine. 1985;6(3):117-130. doi:10.1055/s-2008-1025824

Iannetta D, Ingram CP, Keir DA, Murias JM. Methodological reconciliation
of CP and MLSS and their agreement with the maximal metabolic steady
state. Medicine & Science in Sports & Exercise. 2022;54(4):622-632.
doi:10.1249/MSS.0000000000002831

Jones AM, Burnley M, Black MI, Poole DC, Vanhatalo A. The maximal
metabolic steady state: redefining the 'gold standard'. Physiological
Reports. 2019;7(10):e14098.

Keir DA, Fontana FY, Robertson TC, Murias JM, Paterson DH, Kowalchuk JM,
Pogliaghi S. Exercise intensity thresholds: identifying the boundaries
of sustainable performance. Medicine & Science in Sports & Exercise.
2015;47(9):1932-1940. doi:10.1249/MSS.0000000000000613

Mattioni Maturana F, Keir DA, McLay KM, Murias JM. Can measures of
critical power precisely estimate the maximal metabolic steady-state?
Applied Physiology, Nutrition, and Metabolism. 2016;41(11):1197-1203.
doi:10.1139/apnm-2016-0248

Ploszczyca K, Jazic D, Piotrowicz Z, Chalimoniuk M, Langfort J, Czuba M.
Comparison of maximal lactate steady state with anaerobic threshold
determined by various methods based on graded exercise test with
3-minute stages in elite cyclists. BMC Sports Science, Medicine and
Rehabilitation. 2020;12:70.

## Examples

``` r
bouts <- data.frame(
  intensity = rep(c(130, 140, 150), each = 4),
  time = rep(c(5, 10, 20, 30), 3),
  lactate = c(2.4, 2.6, 2.7, 2.8, 3.1, 3.4, 3.8, 4.1, 3.9, 4.5, 5.4, 6.3)
)
results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
mlss <- lactate_mlss(bouts, results)
mlss
#> 3 constant-load bouts, steady when lactate rose by 1 mmol/L or less from minute 10 to 30.
#> MLSS at 140 W: the next bout (150 W) was not steady.
#> Closest thresholds: ModDmax (+0 W), OBLA 3.5 (-1 W), Exp-Dmax (-5 W).
mlss$thresholds
#> # A tibble: 18 × 4
#>    method           intensity   gap gap_percent
#>    <chr>                <dbl> <dbl>       <dbl>
#>  1 Log-log                101   -39       -28.1
#>  2 OBLA 2.0               106   -34       -24.5
#>  3 OBLA 2.5               119   -21       -14.8
#>  4 OBLA 3.0               130   -10        -7.2
#>  5 OBLA 3.5               139    -1        -1  
#>  6 OBLA 4.0               146     6         4.4
#>  7 Bsln + 0.5              83   -57       -41  
#>  8 Bsln + 1.0             103   -37       -26.1
#>  9 Bsln + 1.5             118   -22       -16  
#> 10 Dmax                   132    -8        -5.9
#> 11 ModDmax                140     0         0.1
#> 12 Exp-Dmax               135    -5        -3.7
#> 13 Log-Poly-ModDmax       149     9         6.7
#> 14 Log-Exp-ModDmax        152    12         8.2
#> 15 LTP1                   111   -29       -20.7
#> 16 LTP2                   160    20        14  
#> 17 LTratio                 77   -63       -44.8
#> 18 IAT                    125   -15       -10.9
## the plot is short and wide: see it in the article Beyond the step test
if(interactive()) autoplot(mlss)
```
