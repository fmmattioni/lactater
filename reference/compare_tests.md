# Compare two lactate tests

Compares two tests of the same athlete (e.g. before and after a training
block): the change of each threshold, whether it is larger than the
noise of the method, the heart rate at each threshold, and notes on what
could make the comparison unfair.

## Usage

``` r
compare_tests(before, after, cv = NULL, swc = 0, level = 0.9)

# S3 method for class 'lactate_comparison'
autoplot(object, ...)
```

## Arguments

- before, after:

  The results of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  for the two tests, with the same sport.

- cv:

  The test-retest coefficient of variation (%) of each method: one
  number for all methods, or a named vector (e.g. `c("LTP2" = 3)`) that
  adds to or replaces the defaults, or the result of
  [`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
  for your own tests.

- swc:

  The smallest worthwhile change, in % of the intensity before. Defaults
  to `0`.

- level:

  The confidence level of the interval. Defaults to `0.9`.

- object:

  The result of `compare_tests()`.

- ...:

  Not used.

## Value

a `lactate_comparison` tibble with one row per method found in both
tests: `method`, `before`, `after`, `change`, `change_percent`, the
interval of the change (`change_low`, `change_high`), the `cv` (%) it
was computed with, the `noise`, the `ratio` of the change to it, the
`verdict`, and the heart rate at each threshold in both tests (when
recorded). The `notes` attribute lists what differs between the two
tests (stages, step, resting sample, sample type) and a fall in peak
lactate. Plot both curves with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Details

A threshold moves between two tests even without any change in fitness,
by the day-to-day variation of the athlete and the error of the analyzer
and the method. Each change is therefore given with an interval, change
+/- z \* sqrt(2) \* typical error, where the typical error is the
coefficient of variation (`cv`) times the intensity before (Hopkins,
2000; Swinton et al., 2018). This is the model behind the repeatability
of Bland & Altman (1996), for whom two measurements of the same person
differ by less than `1.96 * sqrt(2)` within-subject standard deviations
in 95% of pairs, a size also known as the minimal detectable change
(Sainani, 2017). With a `cv` from
[`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md),
`z` is the t quantile for its degrees of freedom, because a typical
error from few athletes is itself uncertain (Swinton et al., 2018): with
10 athletes tested twice, the normal quantile would call 13% of
unchanged pairs "beyond the noise" at the 90% level instead of 10%.

Three columns say how the change stands against that noise, without a
probability attached to them:

- `noise`: `sqrt(2)` x the typical error, the typical difference between
  two tests when nothing changed, in the unit of the intensity.

- `ratio`: the change divided by the noise. At 1, the change is as large
  as two tests usually differ by anyway.

- `verdict`: `"beyond the noise"` when the whole interval lies beyond
  the smallest worthwhile change (`swc`; beyond zero by default), which
  at the default 90% is a ratio above 1.64, and `"within the noise"`
  otherwise. A change within the noise is not a sign that nothing
  changed: one pair of tests cannot tell. The words describe this one
  comparison against the `cv` in the table; graded terms such as
  "likely" are not used, because they read a frequentist interval as a
  probability of the true change (Lohse et al., 2020).

Three things decide how often a verdict is wrong, so settle them before
looking at the result:

- **The level.** At the default 90%, an unchanged threshold is called
  "beyond the noise" in 10% of comparisons: 5% as a gain and 5% as a
  loss, which is a one-sided test for a gain at 5%, the test
  Sainani (2018) recommends in place of graded terms. No level is agreed
  on: 95% is the classic choice, and narrower intervals are defended for
  decisions in practice (Swinton et al., 2018).

- **The smallest worthwhile change.** With `swc` above zero, the verdict
  asks for a change that is certainly larger than `swc`, not merely
  larger than zero. No value is agreed on for lactate thresholds; one
  convention is 0.2 times the standard deviation between athletes
  (Swinton et al., 2018). Choosing `swc` or `level` after seeing the
  verdict lets one pick the verdict one likes (Sainani, 2018).

- **The threshold.** Each method gets its own verdict, and with every
  method in the table some of them will lie beyond the noise by chance.
  In 157 pairs of consecutive tests of the cyclists of Heuberger et al.
  (2018), each method was beyond the noise in 6% to 15% of pairs, but at
  least one of the 18 was in 50%, and the default LT1 or LT2 in 19%. The
  LT1 and LT2 chosen in
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  are the ones to read, as the header does; a method picked because its
  verdict stands out proves nothing.

The default `cv` of each method is lactater's own calculation:
[`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
on the five step tests of each of 47 well-trained male cyclists in the
open data of Heuberger et al. (2018; 204 tests, 5-min stages, venous
blood, about two weeks apart), so that it is the noise of each method as
lactater computes it, on a step test. The output gives it in the `cv`
column.

|  |  |  |
|----|----|----|
| Method | Default `cv` (%) | Published (%) |
| OBLA 2.0, 2.5, 3.0, 3.5 | 7.7, 5.2, 4.6, 4.8 |  |
| OBLA 4.0 | 4.4 | 3.8 (Heuberger et al., 2018) |
| Bsln + 0.5, 1.0, 1.5 | 8.2, 5.0, 4.5 | 3.7, 3.4, 3.4 (Pallares et al., 2016; ramp test) |
| Dmax | 5.9 | 3.4 (Heuberger et al., 2018) |
| ModDmax | 5.1 | 4.3 (Heuberger et al., 2018) |
| Exp-Dmax, Log-Poly-ModDmax, Log-Exp-ModDmax | 4.3, 5.5, 4.9 |  |
| IAT | 4.4 | 3.6 (Heuberger et al., 2018) |
| LTP1, LTP2 | 10.6, 6.0 |  |
| Log-log, LTratio | 7.8, 9.4 |  |

The defaults are on the careful side. Heuberger et al. computed their
values on the riders who received placebo, with their own implementation
of each method; the open data do not say which riders received
erythropoietin, so the defaults use every rider and are, if anything,
too high (pairs of consecutive tests gave values of the same size, so a
drift between the two groups does not explain them). Those tests have no
resting sample, so Bsln+ rose from the first stage. To use the published
values instead, give them: `cv = c("IAT" = 3.6, "Dmax" = 3.4)`.
`Stegmann`, `Keul`, `Simon` and your own `fixed` or `baseline_plus`
values have no default: their `verdict` is `NA` unless you give one.

The defaults come from cycling and are used for every sport, as a
percentage of the intensity; the `notes` say so for other sports. No
open test-retest data of running step tests were found to compute
running values the same way. Published running values are of a similar
size in trained runners: typical errors of about 0.3 to 0.8 km/h (2% to
6%) for 22 trained men on a treadmill with 1-min stages, from the SDs of
their test-retest differences (the CVs they report, 1.2% to 3.5%, are
smaller; Cerezuela-Espejo et al., 2018), and 0.4 to 1.4 km/h for 19
healthy, untrained people with 4-min and 8-min stages (Gavin et al.,
2014). The best `cv` is the one of your own athletes, measured with your
protocol and analyzer. A `cv` assumes that the noise grows with the
intensity, which is common for such measures but should be checked where
the data allow (Atkinson & Nevill, 1998).

## References

Atkinson G, Nevill AM. Statistical methods for assessing measurement
error (reliability) in variables relevant to sports medicine. Sports
Medicine. 1998;26(4):217-238.

Bland JM, Altman DG. Measurement error. BMJ. 1996;312(7047):1654.
doi:10.1136/bmj.312.7047.1654

Cerezuela-Espejo V, Courel-Ibáñez J, Morán-Navarro R, Martínez-Cava A,
Pallarés JG. The relationship between lactate and ventilatory thresholds
in runners: validity and reliability of exercise test performance
parameters. Frontiers in Physiology. 2018;9:1320.
doi:10.3389/fphys.2018.01320

Gavin JP, Willems MET, Myers SD. Reproducibility of lactate markers
during 4 and 8 min stage incremental running: a pilot study. Journal of
Science and Medicine in Sport. 2014;17(6):635-639.
doi:10.1016/j.jsams.2013.08.006

Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda
Y, Cohen AF. Repeatability and predictive value of lactate threshold
concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.

Hopkins WG. Measures of reliability in sports medicine and science.
Sports Medicine. 2000;30(1):1-15.

Lohse KR, Sainani KL, Taylor JA, Butson ML, Knight EJ, Vickers AJ.
Systematic review of the use of "magnitude-based inference" in sports
science and medicine. PLOS ONE. 2020;15(6):e0235318.

Pallares JG, Moran-Navarro R, Ortega JF, Fernandez-Elias VE,
Mora-Rodriguez R. Validity and reliability of ventilatory and blood
lactate thresholds in well-trained cyclists. PLOS ONE.
2016;11(9):e0163389.

Sainani KL. Reliability statistics. PM&R. 2017;9(6):622-628.
doi:10.1016/j.pmrj.2017.05.001

Sainani KL. The problem with "magnitude-based inference". Medicine &
Science in Sports & Exercise. 2018;50(10):2166-2176.
doi:10.1249/MSS.0000000000001645

Swinton PA, Hemingway BS, Saunders B, Gualano B, Dolan E. A statistical
framework to interpret individual response to intervention: paving the
way for personalized nutrition and exercise prescription. Frontiers in
Nutrition. 2018;5:41.

## Examples

``` r
series <- subset(lactate_tests, athlete == "cyclist_series")
before <- lactate_threshold(subset(series, day == 0), intensity, lactate, heart_rate, plot = FALSE)
after <- lactate_threshold(subset(series, day == 58), intensity, lactate, heart_rate, plot = FALSE)
compare_tests(before, after)
#> # Tests compared: cycling, 18 methods; noise: what two tests differ by when
#> #   nothing changed; verdict at 90%
#> # LT1:            257 to 268 W (+10, 0.3 x the noise of 30 W: within the
#> #   noise), by Bsln + 0.5
#> # LT2:            304 to 326 W (+22, 0.9 x the noise of 24 W: within the
#> #   noise), by Log-Poly-ModDmax
#>    method  before after change change_percent change_low change_high    cv noise
#>    <fct>    <dbl> <dbl>  <dbl>          <dbl>      <dbl>       <dbl> <dbl> <dbl>
#>  1 Log-log    239   236     -3           -1.1        -46          41   7.8    26
#>  2 OBLA 2…    282   301     19            6.8        -31          70   7.7    31
#>  3 OBLA 2…    296   316     19            6.5        -17          55   5.2    22
#>  4 OBLA 3…    308   328     20            6.4        -13          53   4.6    20
#>  5 OBLA 3…    318   338     20            6.3        -15          56   4.8    22
#>  6 OBLA 4…    327   348     21            6.3        -13          54   4.4    20
#>  7 Bsln +…    257   268     10            4          -39          59   8.2    30
#>  8 Bsln +…    279   291     12            4.4        -20          45   5      20
#>  9 Bsln +…    294   307     14            4.6        -17          44   4.5    19
#> 10 Dmax       270   291     22            8.1        -15          59   5.9    22
#> 11 ModDmax    290   327     36           12.6          2          71   5.1    21
#> 12 Exp-Dm…    277   303     26            9.3         -2          53   4.3    17
#> 13 Log-Po…    304   326     22            7.1        -17          61   5.5    24
#> 14 Log-Ex…    309   333     24            7.8        -11          59   4.9    21
#> 15 LTP1       222   257     35           15.8        -20          90  10.6    33
#> 16 LTP2       301   341     40           13.3         -2          82   6      26
#> 17 LTratio    192   180    -12           -6.1        -54          30   9.4    25
#> 18 IAT        294   304     10            3.4        -20          40   4.4    18
#> # ℹ 4 more variables: ratio <dbl>, verdict <chr>, heart_rate_before <dbl>,
#> #   heart_rate_after <dbl>
```
