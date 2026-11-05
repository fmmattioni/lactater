
<!-- README.md is generated from README.Rmd. Please edit that file -->

<div align="center">

<a href="https://fmmattioni.github.io/lactater/"><img src="man/figures/logo.png" width="200px" alt="lactater logo"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/lactater)](https://CRAN.R-project.org/package=lactater)
[![R-CMD-check](https://github.com/fmmattioni/lactater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fmmattioni/lactater/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fmmattioni/lactater/graph/badge.svg)](https://app.codecov.io/gh/fmmattioni/lactater)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/lactater?color=blue)](https://CRAN.R-project.org/package=lactater)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/lactater?color=blue)](https://CRAN.R-project.org/package=lactater)
<!-- badges: end -->

</div>

**lactater** takes the lactate samples of a step incremental test and
gives you the thresholds, how sure they are, what to double-check, the
training zones they make, how they changed since the last test, and a
report for the athlete. It computes the methods sport scientists use,
each as its original paper defined it: Log-log, OBLA, Bsln+, the
D<sub>max</sub> family, the lactate turning points, LT<sub>ratio</sub>,
and the individual anaerobic thresholds of Dickhuth and of Stegmann. It
is written for teaching too: every function and article says what the
evidence behind it is.

## What it does

| To | Use | You get |
|----|----|----|
| Estimate the thresholds | `lactate_threshold()` | every method at once, with heart rate, RPE and pace at each threshold |
| Know how sure each one is | `ci = TRUE` | bootstrap intervals for the noise of the samples |
| Check the data and the fit | `lactate_checks()`, `lactate_diagnostics()`, `lactate_fit_quality()` | what to double-check, residuals, influential stages, how much each threshold rests on one stage |
| Make training zones | `lactate_zones()` | three or five zones from LT1 and LT2 (Bsln + 0.5 and Log-Poly-ModDmax unless you choose others), with intensity, pace, heart rate, RPE and lactate |
| Compare two tests | `compare_tests()`, `curve_shift()`, `curve_shape()` | each change against the noise of the method, and how far and how the curve moved |
| Measure your own noise | `lactate_reliability()` | the test-retest typical error of each threshold from your repeated tests, for `compare_tests()` |
| Follow a series of tests | `lactate_trend()` | the curves and zones of up to five tests, and a trend for longer series |
| Compare athletes | `relative_thresholds()` | thresholds per kg and as a percentage of peak intensity and heart rate |
| Compare with the population | `reference_percentiles()` | the percentile of power at 3 mmol/L and at baseline + 1 mmol/L among German adults of the same age and sex (cycling, general population) |
| Count training | `training_load()` | time in each zone and the polarization index |
| Predict a race | `predict_performance()`, `calibrate_performance()` | race time or time-trial power with a 95% range, from published equations or your own |
| Report the test | `test_report()`, `report_start()` | a web page and PDF for the athlete or the coach |
| Record a test | the [web form](https://fmmattioni.github.io/lactater/step-test-form.html), `read_recording()`, `recording_sheet()` | a form that saves itself and downloads an .xlsx lactater reads; a printable sheet and a CSV template |
| Check the MLSS | `lactate_mlss()` | the maximal lactate steady state from constant-load bouts, and how far each threshold lies from it |
| Analyze a sprint test | `vlamax()` | the maximal lactate accumulation rate (V̇La<sub>max</sub>) |
| Practice | `lactate_tests`, `demo_data` | 16 real and simulated tests across sports and fitness levels |

## The charts

Every result has a plot, made to explain itself: the method as the
title, how it works as the subtitle, and the values written where you
look. Click a chart for its help page.

<table>

<tr>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/autoplot.lactate_thresholds.html"><img src="man/figures/readme/gallery-overview.png" alt="Every threshold of a test on one intensity axis, grouped by method family, with a table of lactate, heart rate and percentages of peak"></a><br><b>One
test, every method</b>: each threshold on one axis, grouped by family,
with its values. <code>autoplot(results)</code>
</td>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/lactate_trend.html"><img src="man/figures/readme/gallery-series.png" alt="The lactate curves of five tests, light to dark by date, then their heart rate, above the five training zones of each test"></a><br><b>Up
to five tests</b>: where the curve, the heart rate and the zones moved,
by date. <code>autoplot(lactate_trend(…))</code>
</td>

</tr>

<tr>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/autoplot.lactate_thresholds.html"><img src="man/figures/readme/gallery-method.png" alt="The ModDmax method: the line from the first rise to the last stage and the threshold on the fitted curve"></a><br><b>How
a method works</b>: its construction drawn on the curve, and the
threshold with lactate and heart rate. <code>autoplot(results, method =
“ModDmax”)</code>
</td>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/lactate_zones.html"><img src="man/figures/readme/gallery-zones.png" alt="Five training zones on the lactate curve, colored by intensity domain, with the heart rate of the stages in a panel below"></a><br><b>Training
zones</b>: three or five, colored by intensity domain, with the heart
rate at LT1 and LT2. <code>autoplot(lactate_zones(…))</code>
</td>

</tr>

<tr>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/compare_tests.html"><img src="man/figures/readme/gallery-compare.png" alt="Two lactate curves with arrows showing the intensity gained at 2 and 4 mmol/L"></a><br><b>Two
tests</b>: the intensity gained at 2 and 4 mmol/L.
<code>autoplot(compare_tests(before, after))</code>
</td>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/curve_shape.html"><img src="man/figures/readme/gallery-shape.png" alt="Two exponential curves, one shifted onto the other, to test whether the shape changed"></a><br><b>Shifted
or reshaped?</b> One curve moved onto the other, with a test of whether
the shape changed. <code>autoplot(curve_shape(before, after))</code>
</td>

</tr>

<tr>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/training_load.html"><img src="man/figures/readme/gallery-load.png" alt="The share of time in each training zone for three sessions"></a><br><b>Time
in zone</b>: how each session or week was spread across the zones.
<code>autoplot(training_load(…))</code>
</td>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/predict_performance.html"><img src="man/figures/readme/gallery-performance.png" alt="Predicted race times from 5 km to the marathon with their 95% ranges"></a><br><b>Race
prediction</b>: expected times or time-trial power with a 95% range.
<code>autoplot(predict_performance(results))</code>
</td>

</tr>

<tr>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.html"><img src="man/figures/readme/gallery-diagnostics.png" alt="Six diagnostic panels: fit, residuals, normality, influential stages, leave one stage out, and which curve predicts best"></a><br><b>Is
the curve right?</b> Six panels, each with a line on what to look for.
<code>autoplot(lactate_diagnostics(results))</code>
</td>

<td width="50%" valign="top">

<a href="https://fmmattioni.github.io/lactater/report-example.html"><img src="man/figures/readme/report-page.png" alt="The first page of a test report: LT1 and LT2, what they mean, and the training zones"></a><br><b>A
report for the athlete</b>: the whole test on two A4 pages, as a web
page and PDF. <code>test_report(results)</code>
</td>

</tr>

</table>

## Installation

``` r
install.packages("lactater")

# development version
# install.packages("pak")
pak::pak("fmmattioni/lactater")
```

## Get started

One row per stage: intensity, lactate and, if you have them, heart rate
and RPE. The resting sample is the row with intensity 0.
`lactate_threshold()` computes every method at once:

``` r
demo_data
#> # A tibble: 8 × 6
#>    step length intensity lactate heart_rate   rpe
#>   <int>  <int>     <int>   <dbl>      <int> <int>
#> 1     0      0         0    0.93         96     6
#> 2     1      3        50    0.98        114     7
#> 3     2      3        75    1.23        134     8
#> 4     3      3       100    1.88        154    11
#> 5     4      3       125    2.8         170    12
#> 6     5      3       150    4.21        182    14
#> 7     6      3       175    6.66        193    17
#> 8     7      2       191    8.64        198    19

results <- lactate_threshold(
  demo_data,
  intensity_column = intensity,
  lactate_column = lactate,
  heart_rate_column = heart_rate,
  rpe_column = rpe,
  sport = "cycling",
  plot = FALSE
)

results
#> # Lactate thresholds: cycling, 3rd degree polynomial, 7 stages + rest
#>    method_category method           fitting   intensity lactate heart_rate   rpe
#>    <fct>           <fct>            <chr>         <dbl>   <dbl>      <dbl> <dbl>
#>  1 Log-log         Log-log          3rd degr…       101    1.8         154  11  
#>  2 OBLA            OBLA 2.0         3rd degr…       106    2           158  11.2
#>  3 OBLA            OBLA 2.5         3rd degr…       119    2.5         166  11.8
#>  4 OBLA            OBLA 3.0         3rd degr…       130    3           172  12.4
#>  5 OBLA            OBLA 3.5         3rd degr…       139    3.5         177  13.1
#>  6 OBLA            OBLA 4.0         3rd degr…       146    4           180  13.7
#>  7 Bsln+           Bsln + 0.5       3rd degr…        83    1.43        140   8.9
#>  8 Bsln+           Bsln + 1.0       3rd degr…       103    1.93        156  11.1
#>  9 Bsln+           Bsln + 1.5       3rd degr…       118    2.43        165  11.7
#> 10 Dmax            Dmax             3rd degr…       132    3.1         173  12.5
#> 11 Dmax            ModDmax          3rd degr…       140    3.6         177  13.2
#> 12 Dmax            Exp-Dmax         Exponent…       135    3.3         175  12.8
#> 13 Dmax            Log-Poly-ModDmax 3rd degr…       149    4.2         182  13.9
#> 14 Dmax            Log-Exp-ModDmax  Exponent…       152    4.4         183  14.2
#> 15 LTP             LTP1             3rd degr…       111    2           161  11.4
#> 16 LTP             LTP2             3rd degr…       160    4.7         186  15.1
#> 17 LTratio         LTratio          B-Spline…        77    1.3         136   8.3
#> 18 IAT             IAT              B-Spline…       125    2.8         170  12
```

With 7 to 10 samples, a threshold to 0.1 W is false precision.
`ci = TRUE` adds bootstrap confidence intervals:

``` r
set.seed(1)
demo_data |>
  lactate_threshold(intensity, lactate, heart_rate, method = c("OBLA", "Dmax", "LTP"), plot = FALSE, ci = TRUE) |>
  select(method, intensity, intensity_low, intensity_high)
#> # Lactate thresholds: cycling, 3rd degree polynomial, 7 stages + rest
#>    method           intensity intensity_low intensity_high
#>    <fct>                <dbl>         <dbl>          <dbl>
#>  1 OBLA 2.0               106           100            111
#>  2 OBLA 2.5               119           115            123
#>  3 OBLA 3.0               130           127            133
#>  4 OBLA 3.5               139           136            142
#>  5 OBLA 4.0               146           143            149
#>  6 Dmax                   132           124            138
#>  7 ModDmax                140           132            150
#>  8 Exp-Dmax               135           133            137
#>  9 Log-Poly-ModDmax       149           146            161
#> 10 Log-Exp-ModDmax        152           149            162
#> 11 LTP1                   111            75            123
#> 12 LTP2                   160           140            171
```

`lactate_checks()` flags what is easy to miss in a table of numbers:

``` r
lactate_checks(results)
#> 8 checks: 7 passed, 1 note, 0 warnings
#> ℹ Stages             7 stages: the LTP fit has 6 parameters, so 1 degree(s) of freedom are left and LTP can move a lot with one stage (at least 8 stages are better); the fitted curves rest on few points too.
#> ✔ Resting sample     Present.
#> ✔ Thresholds found   All thresholds were found.
#> ✔ Edge of the range  No threshold sits at the first or last stage.
#> ✔ Early lactate dip  No early dip.
#> ✔ Final stage        Lactate rose in the last stage.
#> ✔ Peak lactate       8.6 mmol/L.
#> ✔ Sample type        blood lactate.
```

You choose which thresholds mark LT1 and LT2 (Bsln + 0.5 and
Log-Poly-ModDmax by default), and three or five zones:

``` r
lactate_zones(results, model = 5)
#> # A tibble: 5 × 11
#>   zone   domain  description intensity_from intensity_to lactate_from lactate_to
#>   <chr>  <chr>   <chr>                <dbl>        <dbl>        <dbl>      <dbl>
#> 1 Zone 1 modera… recovery                NA           66         NA          1.2
#> 2 Zone 2 modera… endurance               66           83          1.2        1.4
#> 3 Zone 3 heavy   tempo                   83          149          1.4        4.2
#> 4 Zone 4 severe  threshold              149          170          4.2        6.1
#> 5 Zone 5 severe  VO2max                 170           NA          6.1       NA  
#> # ℹ 4 more variables: heart_rate_from <dbl>, heart_rate_to <dbl>,
#> #   rpe_from <dbl>, rpe_to <dbl>
```

## Compare tests

`compare_tests()` gives each change an interval from the test-retest
variation of the method, and calls it an increase or a decrease only
when the whole interval is on one side of zero:

``` r
series <- lactate_tests |>
  filter(athlete == "cyclist_series")

before <- series |>
  filter(day == 0) |>
  lactate_threshold(intensity, lactate, heart_rate, plot = FALSE)

after <- series |>
  filter(day == 58) |>
  lactate_threshold(intensity, lactate, heart_rate, plot = FALSE)

compare_tests(before, after) |>
  select(method, before, after, change, change_low, change_high, verdict)
#> # A tibble: 18 × 7
#>    method           before after change change_low change_high verdict 
#>    <fct>             <dbl> <dbl>  <dbl>      <dbl>       <dbl> <chr>   
#>  1 Log-log             239   236     -3         NA          NA <NA>    
#>  2 OBLA 2.0            282   301     19         NA          NA <NA>    
#>  3 OBLA 2.5            296   316     19         NA          NA <NA>    
#>  4 OBLA 3.0            308   328     20         NA          NA <NA>    
#>  5 OBLA 3.5            318   338     20         NA          NA <NA>    
#>  6 OBLA 4.0            327   348     21         -8          50 unclear 
#>  7 Bsln + 0.5          257   268     10        -12          33 unclear 
#>  8 Bsln + 1.0          279   291     12        -10          34 unclear 
#>  9 Bsln + 1.5          294   307     14        -10          37 unclear 
#> 10 Dmax                270   291     22          0          43 increase
#> 11 ModDmax             290   327     36          7          65 increase
#> 12 Exp-Dmax            277   303     26         NA          NA <NA>    
#> 13 Log-Poly-ModDmax    304   326     22         NA          NA <NA>    
#> 14 Log-Exp-ModDmax     309   333     24         NA          NA <NA>    
#> 15 LTP1                222   257     35         NA          NA <NA>    
#> 16 LTP2                301   341     40         NA          NA <NA>    
#> 17 LTratio             192   180    -12         NA          NA <NA>    
#> 18 IAT                 294   304     10        -15          35 unclear
```

## Report the test

`test_report()` writes the whole test as one web page for the athlete or
the coach, two A4 pages when printed (see [a
cyclist](https://fmmattioni.github.io/lactater/report-example.html) and
[a
runner](https://fmmattioni.github.io/lactater/report-running-example.html)).
Give it two to five tests of one athlete and their dates, and it reports
the progress over them ([see an
example](https://fmmattioni.github.io/lactater/report-series-example.html)).
Or build it step by step, with your lab’s name and logo and only the
parts you want:

``` r
report_start(results) |>
  report_header(
    name = "Ana Silva", date = "3 November 2026", sex = "female", age = 27, body_mass = 61,
    lab = "Exercise physiology lab", logo = "logo.png"
  ) |>
  report_summary() |>
  report_zones() |>
  report_prediction() |>
  report_text("Training", "Two sessions a week in zone 2, one in zone 3.") |>
  report_save("ana-2026-11-03.html", pdf = TRUE)
```

To record every test the same way, the [web
form](https://fmmattioni.github.io/lactater/step-test-form.html) takes
the test on a laptop or tablet, saves itself in the browser and
downloads an .xlsx that `read_recording()` turns into the inputs of
`lactate_threshold()` and `test_report()`; `recording_sheet()` copies
the form, a printable sheet
([PDF](https://fmmattioni.github.io/lactater/step-test-sheet.pdf)) and a
CSV template into your folder. The
[cheatsheet](https://fmmattioni.github.io/lactater/lactater-cheatsheet.pdf)
puts the whole package on two pages.

## More options

- Set your own targets (`fixed`, `baseline_plus`, `iat_rise`), fit one
  turn point for submaximal tests (`ltp_breakpoints = 1`), or add
  recovery samples for the Stegmann threshold (`recovery`,
  `stage_duration`).
- Cycling, running, rowing and swimming, with pace where it applies;
  blood or other samples (`sample`).
- `.by` analyzes a whole squad in one call; `lactate_report()` formats
  the results as a table for reports and papers.
- `options(lactater.style = "night")` gives every plot a dark style, and
  `"journal"` a quieter one for print.
- `run_data_input()` is an RStudio gadget that writes the test protocol
  for you.

## Learn more

- [Get
  started](https://fmmattioni.github.io/lactater/articles/lactater.html):
  the whole workflow, task by task.
- [Best practices for a step
  test](https://fmmattioni.github.io/lactater/articles/best-practices.html):
  how to run a test and analyze it, as a checklist with sources.
- [The methods and the
  evidence](https://fmmattioni.github.io/lactater/articles/methods.html):
  how each method is defined, and what the studies say about it.
- [Checking the
  analysis](https://fmmattioni.github.io/lactater/articles/checking.html):
  the checks, the diagnostics panel by panel, and bootstrap intervals.
- [Training
  zones](https://fmmattioni.github.io/lactater/articles/zones.html) and
  [training
  load](https://fmmattioni.github.io/lactater/articles/training-load.html):
  from LT1 and LT2 to zones, and time in each.
- [Comparing
  tests](https://fmmattioni.github.io/lactater/articles/compare.html):
  is a change real, did the curve shift or change its shape, and series
  of tests.
- [Predicting
  performance](https://fmmattioni.github.io/lactater/articles/performance.html):
  race times from a step test, how sure they are, and your own equation.
- [The example
  data](https://fmmattioni.github.io/lactater/articles/example-data.html):
  16 tests across sports.
- [Beyond the step
  test](https://fmmattioni.github.io/lactater/articles/beyond-the-step-test.html):
  the maximal lactate steady state from constant-load bouts, and
  V̇La<sub>max</sub> from a sprint test.

**lactater** 1.0 fixed several calculations of earlier versions; see
[what
changed](https://fmmattioni.github.io/lactater/articles/whats-new.html).
