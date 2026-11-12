# Predict performance from a step test

Estimates race times in running and time-trial power in cycling from the
thresholds of a step test, with a 95% prediction range. Each estimate
comes from an equation fitted on one threshold, in one population, with
one test protocol, so it only applies to tests run the same way:

## Usage

``` r
predict_performance(x, method = NULL, equation = NULL)

# S3 method for class 'lactate_performance'
autoplot(object, ...)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- method:

  The thresholds to predict from. Defaults to every threshold of `x`
  that has an equation.

- equation:

  Your own equation(s) from
  [`calibrate_performance()`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md),
  one or a list of them, used in place of the published ones.

- object:

  The result of `predict_performance()`.

- ...:

  Not used.

## Value

a `lactate_performance` tibble with one row per estimate: `event`,
`method`, `threshold`, the predicted mean race `intensity` (speed or
power, in the unit of the sport) with its 95% range (`intensity_low`,
`intensity_high`), for races over a distance the `time` with its range
(`time_low`, `time_high`), and the `source`. Plot it with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html);
the article [Predicting
performance](https://fmmattioni.github.io/lactater/articles/performance.html)
shows it.

## Details

- **Running**: 5 km, 10 km, half marathon and marathon from IAT (Roecker
  et al., 1998; 427 competitive runners, treadmill at 2% grade, 3-min
  stages, +2 km/h, earlobe hemolyzed whole blood), and the marathon from
  OBLA 4.0 (Sjödin & Jacobs, 1981; 18 male marathon runners, 4-min
  stages, fingertip blood). lactater's `IAT` follows the definition of
  Roecker et al. (the lowest lactate/speed ratio + 1.5 mmol/L).

- **Cycling**: 45-min time-trial power from LTP2, IAT, ModDmax, OBLA
  4.0, Dmax or Log-Poly-ModDmax, fitted with lactater's own thresholds
  on the open data of Heuberger et al. (2018; 48 well-trained men, 5-min
  stages, +25 W, venous blood).

- **Your own equation**, from
  [`calibrate_performance()`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md):
  any threshold, sport and protocol, fitted on your own athletes' tests
  and races. This is the best option when you have the data.

Other thresholds are not offered with the published equations: an
equation fitted on one threshold gives wrong numbers for another.

The 95% range is the prediction interval of the regression: the
predicted intensity +/- t x the standard error of the estimate x
`sqrt(1 + 1/n + (threshold - mean)^2 / ((n - 1) * sd^2))`. Roecker et
al. (1998) reported neither that error nor the mean of the thresholds
per distance, so the error is derived from their tables as
`SD x sqrt(1 - r^2)`, with the SD of race speed computed from the SD of
race time, and the last term is left out.

Expect an individual error of about +/- 10% of race speed in running and
+/- 25 W in cycling with the published equations. A warning is given
when the steps of the test differ from those of the source study (a
different step changes the threshold: Coen et al., 2001; Kuipers et al.,
2003), and when a threshold lies outside the range of the source sample.
Stage duration, sampling site and treadmill grade are checked too when
the test was analyzed with `stage_duration`, `site` and `grade` (see
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md));
otherwise compare them with the protocol in the printed header. A recent
race is a better predictor of the next one than any step test (Vickers &
Vertosick, 2016).

## References

Coen B, Urhausen A, Kindermann W. Individual anaerobic threshold:
methodological aspects of its assessment in running. International
Journal of Sports Medicine. 2001;22(1):8-16.

Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda
Y, Cohen AF. Repeatability and predictive value of lactate threshold
concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.

Kuipers H, Rietjens G, Verstappen F, Schoenmakers H, Hofman G. Effects
of stage duration in incremental running tests on physiological
variables. International Journal of Sports Medicine. 2003;24(7):486-491.

Roecker K, Schotte O, Niess AM, Horstmann T, Dickhuth HH. Predicting
competition performance in long-distance running by means of a treadmill
test. Medicine & Science in Sports & Exercise. 1998;30(10):1552-1557.

Sjödin B, Jacobs I. Onset of blood lactate accumulation and marathon
running performance. International Journal of Sports Medicine.
1981;2(1):23-26.

Vickers AJ, Vertosick EA. An empirical study of race times in
recreational endurance runners. BMC Sports Science, Medicine and
Rehabilitation. 2016;8:26.

## Examples

``` r
runner <- data.frame(
  intensity = c(0, 8, 10, 12, 14, 16, 18),
  lactate = c(1.0, 1.1, 1.2, 1.6, 2.6, 4.6, 8.0)
)
results <- lactate_threshold(runner, intensity, lactate, sport = "running", plot = FALSE)
prediction <- predict_performance(results)
prediction
#> # Predicted performance: running, with 95% prediction ranges
#> # LT2:                   Log-Poly-ModDmax has no equation here; these come from
#> #   IAT and OBLA 4.0
#> # Roecker et al. 1998:   427 competitive runners (88 women); treadmill at 2%
#> #   grade, 3-min stages, +2 km/h, earlobe hemolyzed whole blood; best race
#> #   within 2 months of the test
#> # Sjodin & Jacobs 1981:  18 male marathon runners; treadmill, 4-min stages, +1
#> #   to 2 km/h, fingertip blood; marathon 2 to 3 weeks after the test
#>   event        method threshold time  time_low time_high intensity intensity_low
#>   <chr>        <chr>      <dbl> <chr> <chr>    <chr>         <dbl>         <dbl>
#> 1 5 km         IAT         14.2 17:44 16:05    19:47          16.9          15.2
#> 2 10 km        IAT         14.2 38:26 34:22    43:35          15.6          13.8
#> 3 Half marath… IAT         14.2 1:25… 1:16:24  1:37:58        14.7          12.9
#> 4 Marathon     IAT         14.2 3:08… 2:48:34  3:34:43        13.4          11.8
#> 5 Marathon     OBLA …      15.5 3:04… 2:47:23  3:25:38        13.7          12.3
#> # ℹ 3 more variables: intensity_high <dbl>, extrapolated <lgl>, source <chr>
## the plot is short and wide: see it in the article Predicting performance
if(interactive()) autoplot(prediction)
```
