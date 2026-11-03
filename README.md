
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

**lactater** estimates lactate thresholds from a step incremental test,
in one call and with the methods sport scientists use: Log-log, OBLA,
Bsln+, the Dmax family, the lactate turning points, LTratio and the
individual anaerobic threshold. It shows how much the methods agree, how
sure each threshold is, what to double-check in your data, and turns the
thresholds you choose into training zones. It is written for teaching
too: every function and article says what the evidence behind it is.

## Installation

``` r
install.packages("lactater")

# development version
# install.packages("pak")
pak::pak("fmmattioni/lactater")
```

## Estimate every threshold

One row per stage: intensity, lactate and, if you have it, heart rate.
The resting sample is the row with intensity 0.

``` r
library(lactater)

results <- lactate_threshold(
  demo_data,
  intensity_column = intensity,
  lactate_column = lactate,
  heart_rate_column = heart_rate,
  sport = "cycling",
  plot = FALSE
)

results
#> # Lactate thresholds: cycling, 3rd degree polynomial, 7 stages + rest
#>    method_category method           fitting         intensity lactate heart_rate
#>    <fct>           <fct>            <chr>               <dbl>   <dbl>      <dbl>
#>  1 Log-log         Log-log          3rd degree pol~       101    1.8         154
#>  2 OBLA            OBLA 2.0         3rd degree pol~       106    2           158
#>  3 OBLA            OBLA 2.5         3rd degree pol~       119    2.5         166
#>  4 OBLA            OBLA 3.0         3rd degree pol~       130    3           172
#>  5 OBLA            OBLA 3.5         3rd degree pol~       139    3.5         177
#>  6 OBLA            OBLA 4.0         3rd degree pol~       146    4           180
#>  7 Bsln+           Bsln + 0.5       3rd degree pol~        83    1.43        140
#>  8 Bsln+           Bsln + 1.0       3rd degree pol~       103    1.93        156
#>  9 Bsln+           Bsln + 1.5       3rd degree pol~       118    2.43        165
#> 10 Dmax            Dmax             3rd degree pol~       132    3.1         173
#> 11 Dmax            ModDmax          3rd degree pol~       140    3.6         177
#> 12 Dmax            Exp-Dmax         Exponential (d~       135    3.3         175
#> 13 Dmax            Log-Poly-ModDmax 3rd degree pol~       149    4.2         182
#> 14 Dmax            Log-Exp-ModDmax  Exponential (d~       152    4.4         183
#> 15 LTP             LTP1             3rd degree pol~       111    2           161
#> 16 LTP             LTP2             3rd degree pol~       160    4.7         186
#> 17 LTratio         LTratio          B-Spline (defa~        77    1.3         136
#> 18 IAT             IAT              B-Spline (defa~       125    2.8         170
```

## See how the methods agree

One row per method, grouped by family, with a table of the values at
each threshold:

``` r
autoplot(results)
```

<img src="man/figures/readme/README-overview-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Or look at one method in detail:

``` r
autoplot(results, method = "ModDmax")
```

<img src="man/figures/readme/README-dmax-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## Know how sure you are

With 7 to 10 samples, a threshold to 0.1 W is false precision.
`ci = TRUE` adds bootstrap confidence intervals:

``` r
set.seed(1)
with_ci <- lactate_threshold(
  demo_data, intensity, lactate, heart_rate,
  method = c("OBLA", "Dmax", "LTP"), plot = FALSE, ci = TRUE
)
with_ci[c("method", "intensity", "intensity_low", "intensity_high")]
#> # Lactate thresholds: cycling, 3rd degree polynomial, 7 stages + rest
#>    method           intensity intensity_low intensity_high
#>    <fct>                <dbl>         <dbl>          <dbl>
#>  1 OBLA 2.0               106           103            109
#>  2 OBLA 2.5               119           117            121
#>  3 OBLA 3.0               130           128            132
#>  4 OBLA 3.5               139           137            140
#>  5 OBLA 4.0               146           145            148
#>  6 Dmax                   132           128            135
#>  7 ModDmax                140           131            150
#>  8 Exp-Dmax               135           134            136
#>  9 Log-Poly-ModDmax       149           148            159
#> 10 Log-Exp-ModDmax        152           151            160
#> 11 LTP1                   111            84            120
#> 12 LTP2                   160           144            168
```

## Check before you trust

`lactate_checks()` flags what is easy to miss in a table of numbers, and
`lactate_diagnostics()` checks the fitted curve and how much each
threshold rests on single stages, with a line under each panel on what
to look for:

``` r
lactate_checks(results)
#> # A tibble: 8 x 3
#>   check             status details                                      
#>   <chr>             <chr>  <chr>                                        
#> 1 Stages            ok     7 stages.                                    
#> 2 Resting sample    ok     Present.                                     
#> 3 Thresholds found  ok     All thresholds were found.                   
#> 4 Edge of the range ok     No threshold sits at the first or last stage.
#> 5 Early lactate dip ok     No early dip.                                
#> 6 Final stage       ok     Lactate rose in the last stage.              
#> 7 Peak lactate      ok     8.6 mmol/L.                                  
#> 8 Sample type       ok     blood lactate.
```

``` r
autoplot(lactate_diagnostics(results))
```

<img src="man/figures/readme/README-diagnostics-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## Turn thresholds into zones

You choose which thresholds mark LT1 and LT2, and three or five zones:

``` r
zones <- lactate_zones(results, lt1 = "LTP1", lt2 = "ModDmax")
zones
#> # A tibble: 3 x 9
#>   zone   domain  description intensity_from intensity_to lactate_from lactate_to
#>   <chr>  <chr>   <chr>                <dbl>        <dbl>        <dbl>      <dbl>
#> 1 Zone 1 modera~ below LT1               NA          111         NA          2.2
#> 2 Zone 2 heavy   LT1 to LT2             111          140          2.2        3.6
#> 3 Zone 3 severe  above LT2              140           NA          3.6       NA  
#> # i 2 more variables: heart_rate_from <dbl>, heart_rate_to <dbl>
autoplot(zones)
```

<img src="man/figures/readme/README-zones-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## Compare tests and count training

``` r
series <- subset(lactate_tests, athlete == "cyclist_series")
before <- lactate_threshold(subset(series, day == 0), intensity, lactate, heart_rate, plot = FALSE)
after <- lactate_threshold(subset(series, day == 58), intensity, lactate, heart_rate, plot = FALSE)
comparison <- compare_tests(before, after)
comparison[c("method", "before", "after", "change", "verdict")]
#> # A tibble: 18 x 5
#>    method           before after change verdict 
#>    <fct>             <dbl> <dbl>  <dbl> <chr>   
#>  1 Log-log             239   236     -3 <NA>    
#>  2 OBLA 2.0            282   301     19 <NA>    
#>  3 OBLA 2.5            296   316     19 <NA>    
#>  4 OBLA 3.0            308   328     20 <NA>    
#>  5 OBLA 3.5            318   338     20 <NA>    
#>  6 OBLA 4.0            327   348     21 unclear 
#>  7 Bsln + 0.5          257   268     10 unclear 
#>  8 Bsln + 1.0          279   291     12 unclear 
#>  9 Bsln + 1.5          294   307     14 unclear 
#> 10 Dmax                270   291     22 increase
#> 11 ModDmax             290   327     36 increase
#> 12 Exp-Dmax            277   303     26 <NA>    
#> 13 Log-Poly-ModDmax    304   326     22 <NA>    
#> 14 Log-Exp-ModDmax     309   333     24 <NA>    
#> 15 LTP1                222   257     35 <NA>    
#> 16 LTP2                301   341     40 <NA>    
#> 17 LTratio             192   180    -12 <NA>    
#> 18 IAT                 294   304     10 unclear
autoplot(comparison)
```

<img src="man/figures/readme/README-compare-1.png" alt="" width="100%" style="display: block; margin: auto;" />

`compare_tests()` says whether each change is larger than the noise of
the method, `curve_shift()` how far the whole curve moved,
`curve_shape()` whether it only shifted or changed its shape,
`lactate_trend()` where a series of tests is heading, and
`training_load()` how the training was spread across the zones.

## And more

- `lactate_tests`: 15 real and simulated tests across cycling, running,
  rowing and swimming, from elite to untrained.
- `predict_performance()` estimates race times (running) and time-trial
  power (cycling) with a 95% range, from published equations or one you
  fit on your own athletes with `calibrate_performance()`.
- Heart rate and RPE (6-20 or CR10) at every threshold and in every
  zone.
- Set your own targets (`fixed`, `baseline_plus`, `iat_rise`) and fit
  one turn point for submaximal tests (`ltp_breakpoints = 1`).
- `options(lactater.style = "night")` gives every plot a dark style, and
  `"journal"` a quieter one for print.
- `lactate_report()` formats the results as a table for reports and
  papers; `.by` analyses a whole squad in one call.
- `lactate_fit_quality()` helps you choose the curve, by how well each
  one predicts a stage it was not fitted to.
- `vlamax()` estimates the maximal lactate accumulation rate from a
  sprint test.
- `run_data_input()` is an RStudio gadget that writes the test protocol
  for you.

## The science

Every method follows its original definition, and the [methods
article](https://fmmattioni.github.io/lactater/articles/methods.html)
says how each is computed and what the evidence says about it. [Best
practices for a step
test](https://fmmattioni.github.io/lactater/articles/best-practices.html)
is a checklist, with sources, for running a test and analyzing it.
lactater 1.0 fixed several calculations; see [what
changed](https://fmmattioni.github.io/lactater/articles/whats-new.html).
