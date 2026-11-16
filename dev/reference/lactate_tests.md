# Lactate step tests from different sports, athletes and scenarios

Sixteen incremental step tests in long format, one row per stage, to try
every part of lactater on real data: cycling, running, rowing and
swimming; elite, trained and untrained athletes; one athlete tested six
times over five years; and the cases that need care (an early lactate
dip, no resting sample, 5-min steps, a submaximal test). Analyze one
test with [`subset()`](https://rdrr.io/r/base/subset.html), or several
tests of one sport with `.by = test`.

## Usage

``` r
lactate_tests
```

## Format

A tibble with one row per stage and 13 columns:

- test:

  the test: `cycling_elite`, `cycling_female`, `cycling_early_dip`,
  `cycling_series_1` to `cycling_series_6`, `running`, `rowing`,
  `cycling_5min_no_rest`, `swimming`, `untrained`, `submaximal`,
  `running_recreational`.

- athlete:

  the athlete; the six `cycling_series` tests share `cyclist_series`.

- sport:

  `cycling`, `running`, `rowing`, or `swimming`, as in
  `lactate_threshold(sport = )`.

- sex:

  `M` or `F`.

- body_mass:

  body mass (kg) on the test day, `NA` when not reported.

- day:

  days since the athlete's first test (0 for single tests).

- description:

  who was tested, and how.

- source:

  `Trinity College Dublin (CC BY 4.0)`,
  `Heuberger et al. 2018 (CC BY 4.0)`, or `simulated`.

- step:

  the stage, 0 for the resting sample.

- intensity:

  power (W) for cycling and rowing, speed (km/h) for running, speed
  (m/s) for swimming; 0 for the resting sample.

- lactate:

  blood lactate (mmol/L).

- heart_rate:

  heart rate (bpm), `NA` when not recorded.

- rpe:

  rating of perceived exertion (Borg 6-20), simulated tests only; `NA`
  for the real tests.

## Source

Trinity College Dublin graded exercise test data. Zenodo.
[doi:10.5281/zenodo.10841412](https://doi.org/10.5281/zenodo.10841412) .
License CC BY 4.0.

Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda
Y, Cohen AF. Repeatability and predictive value of lactate threshold
concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846. Data:
figshare,
[doi:10.6084/m9.figshare.7240571](https://doi.org/10.6084/m9.figshare.7240571)
. License CC BY 4.0.

## Details

Twelve tests are real: eleven from the Trinity College Dublin graded
exercise test data (3-min stages, lactate sampled at 2 min, YSI
analyzer; running on a treadmill at 0% grade; rowing with 1 min of rest
between stages) and one from Heuberger et al. (2018) (5-min steps from
175 W). Names, birth dates and test dates were removed. Four tests are
simulated, where no open data exists: a swimmer's 7 x 200 m step test
(+0.05 m/s per step; Pelarigo et al., 2017), an untrained client tested
to exhaustion and stopped early (25 W steps; untrained men peaked near
250 W in Messonnier et al., 2013), and a recreational runner on the
protocol of Roecker et al. (1998) (treadmill at 2% grade, +2 km/h every
3 min from 6 km/h), so that their race-time equations apply as
published. Their lactate rises exponentially through about 2.5 mmol/L
near the first threshold, with analyzer scatter of 3% from 2 mmol/L and
5% below (Bonaventura et al., 2015). Their simulated RPE rises with
lactate, from about 11 at 2 mmol/L to 14 at 4 mmol/L (close to the
cohort means at the first rise in lactate and at 4 mmol/L of Scherr et
al., 2013) and 19 to 20 at the last stage. See
`data-raw/lactate_tests.R`.

## References

Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ.
Reliability and accuracy of six hand-held blood lactate analysers.
Journal of Sports Science and Medicine. 2015;14:203-214.

Messonnier LA, Emhoff CAW, Fattor JA, Horning MA, Carlson TJ, Brooks GA.
Lactate kinetics at the lactate threshold in trained and untrained men.
Journal of Applied Physiology. 2013;114(11):1593-1602.

Pelarigo JG, Machado L, Fernandes RJ, Greco CC, Vilas-Boas JP. Oxygen
uptake kinetics and energy system's contribution around maximal lactate
steady state swimming intensity. PLOS ONE. 2017;12(2):e0167263.

Roecker K, Schotte O, Niess AM, Horstmann T, Dickhuth HH. Predicting
competition performance in long-distance running by means of a treadmill
test. Medicine & Science in Sports & Exercise. 1998;30(10):1552-1557.

Scherr J, Wolfarth B, Christle JW, Pressler A, Wagenpfeil S, Halle M.
Associations between Borg's rating of perceived exertion and
physiological measures of exercise intensity. European Journal of
Applied Physiology. 2013;113(1):147-155.

## Examples

``` r
unique(lactate_tests[c("test", "sport", "description")])
#> # A tibble: 16 × 3
#>    test                 sport    description                                    
#>    <chr>                <chr>    <chr>                                          
#>  1 cycling_elite        cycling  Elite male cyclist, 6.6 W/kg at peak (3-min st…
#>  2 cycling_female       cycling  Trained female cyclist, 5.0 W/kg at peak (3-mi…
#>  3 cycling_early_dip    cycling  Trained male cyclist whose lactate dips after …
#>  4 cycling_series_1     cycling  Trained male cyclist, test 1 of 6 (3-min stage…
#>  5 cycling_series_2     cycling  Trained male cyclist, test 2 of 6 (3-min stage…
#>  6 cycling_series_3     cycling  Trained male cyclist, test 3 of 6 (3-min stage…
#>  7 cycling_series_4     cycling  Trained male cyclist, test 4 of 6 (3-min stage…
#>  8 cycling_series_5     cycling  Trained male cyclist, test 5 of 6 (3-min stage…
#>  9 cycling_series_6     cycling  Trained male cyclist, test 6 of 6 (3-min stage…
#> 10 running              running  Trained male runner, treadmill at 0% grade (3-…
#> 11 rowing               rowing   Trained male rower, 3 min work + 1 min rest pe…
#> 12 cycling_5min_no_rest cycling  Well-trained male cyclist, 5-min steps, no res…
#> 13 swimming             swimming Simulated elite female swimmer, 7 x 200 m step…
#> 14 untrained            cycling  Simulated untrained man, fitness client (3-min…
#> 15 submaximal           cycling  Simulated submaximal test, stopped at about 3 …
#> 16 running_recreational running  Simulated recreational male runner, treadmill …
rowing <- subset(lactate_tests, test == "rowing")
lactate_threshold(rowing, intensity, lactate, heart_rate, sport = "rowing", plot = FALSE)
#> # Lactate thresholds: rowing, 3rd degree polynomial, 8 stages + rest
#> # LT1:                203 W (Bsln + 0.5)
#> # LT2:                309 W (Log-Poly-ModDmax)
#>    method_category method           fitting   intensity  pace lactate heart_rate
#>    <fct>           <fct>            <chr>         <dbl> <dbl>   <dbl>      <dbl>
#>  1 Log-log         Log-log          3rd degr…       201  120.     1          146
#>  2 OBLA            OBLA 2.0         3rd degr…       250  112.     2          163
#>  3 OBLA            OBLA 2.5         3rd degr…       266  110.     2.5        169
#>  4 OBLA            OBLA 3.0         3rd degr…       279  108.     3          173
#>  5 OBLA            OBLA 3.5         3rd degr…       291  106.     3.5        175
#>  6 OBLA            OBLA 4.0         3rd degr…       301  105.     4          177
#>  7 Bsln+           Bsln + 0.5       3rd degr…       203  120.     1.1        147
#>  8 Bsln+           Bsln + 1.0       3rd degr…       233  114.     1.6        158
#>  9 Bsln+           Bsln + 1.5       3rd degr…       253  111.     2.1        165
#> 10 Dmax            Dmax             3rd degr…       282  108.     3.1        174
#> 11 Dmax            ModDmax          3rd degr…       294  106      3.6        176
#> 12 Dmax            Exp-Dmax         Exponent…       295  106.     3.7        176
#> 13 Dmax            Log-Poly-ModDmax 3rd degr…       309  104.     4.4        179
#> 14 Dmax            Log-Exp-ModDmax  Exponent…       317  103.     4.8        180
#> 15 LTP             LTP1             3rd degr…       256  111      1.8        166
#> 16 LTP             LTP2             3rd degr…       340  101.     6.1        184
#> 17 LTratio         LTratio          B-Spline…       166  128.     0.8        129
#> 18 IAT             IAT              B-Spline…       258  111.     2.3        166
```
