# What's new in lactater 1.0

lactater 1.0 is a different package from 0.3.0: every method follows its
original paper and is solved exactly, the results come with checks and
confidence intervals, and the thresholds go on to training zones,
training load, comparisons between tests, race predictions and a report
for the athlete. It is also much faster. The numbers of a test can
change a little, for reasons given at the end.

## What is new

- **Methods as their papers define them.** Log-log as Beaver et
  al. (1985) drew it, the D_(max) family with the line where each paper
  put it, LTP1 and LTP2 as the exact optimum of three segments,
  LT_(ratio) as an exact minimum, OBLA and Bsln+ solved on the curve
  itself; new IAT (Dickhuth) and Stegmann thresholds.
- **Plots that explain themselves.** Each method drawn on the curve with
  its construction, a forest-style overview of every threshold with a
  table of values, zones colored by intensity domain, night and journal
  styles.
- **How sure, and what to double-check.** Bootstrap intervals
  (`ci = TRUE`),
  [`lactate_checks()`](https://fmmattioni.github.io/lactater/reference/lactate_checks.md)
  (including whether the LT1 and LT2 methods agree),
  [`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md)
  and
  [`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md).
- **From thresholds to training.**
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md)
  (three or five zones),
  [`training_load()`](https://fmmattioni.github.io/lactater/reference/training_load.md)
  (time in zone and the polarization index),
  [`predict_performance()`](https://fmmattioni.github.io/lactater/reference/predict_performance.md)
  and
  [`calibrate_performance()`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md).
- **From test to test.**
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  against the test-retest noise of each method,
  [`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
  for your own noise,
  [`curve_shift()`](https://fmmattioni.github.io/lactater/reference/curve_shift.md),
  [`curve_shape()`](https://fmmattioni.github.io/lactater/reference/curve_shape.md),
  [`lactate_trend()`](https://fmmattioni.github.io/lactater/reference/lactate_trend.md),
  [`relative_thresholds()`](https://fmmattioni.github.io/lactater/reference/relative_thresholds.md)
  and
  [`reference_percentiles()`](https://fmmattioni.github.io/lactater/reference/reference_percentiles.md).
- **For the athlete and the lab.**
  [`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md)
  (a web page and a two-page PDF),
  [`lactate_report()`](https://fmmattioni.github.io/lactater/reference/lactate_report.md)
  for papers, a web form that records a test and
  [`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
  to read it back.
- **Beyond the step test.**
  [`lactate_mlss()`](https://fmmattioni.github.io/lactater/reference/lactate_mlss.md)
  from constant-load bouts and
  [`vlamax()`](https://fmmattioni.github.io/lactater/reference/vlamax.md)
  from a sprint test.
- **Data and inputs.** `lactate_tests` (16 real and simulated tests
  across four sports), rowing, RPE, flexible targets (`fixed`,
  `baseline_plus`, `iat_rise`), one turn point for submaximal tests
  (`ltp_breakpoints = 1`), and `.by` for many tests in one call.

## Faster

Every method is now computed in closed form or by a deterministic
search, with no iterative model fitting, and eleven dependencies were
dropped. On `demo_data` with every method,
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
takes about 10 ms without plots where 0.3.0 took about 500 ms, some
fifty times faster; with the plot of every method it takes 0.5 s instead
of 0.9 s. A whole squad goes through `.by` in under a second, and
bootstrap intervals with 500 replicates take a few seconds (measured on
a laptop; your numbers will differ, the ratio less so).

## Why some numbers change, and an example

Because the methods are now computed as their authors defined them, a
threshold can land a little away from where 0.3.0 put it. The size of
the change depends on the test: on a smooth curve OBLA and Bsln+ move by
a watt or two, while a Log-log split or an LTP turn point can move more
when the stages leave the fit a choice. Here is one example,
`cycling_female` from `lactate_tests`, run with
`include_baseline = TRUE` as the 0.3.0 values were computed (the default
of 1.0 is `FALSE`). It is one test, not a rule: your own tests will
differ by other amounts.

``` r

library(lactater)
library(dplyr)

## 0.3.0 run on the same test (fit = "3rd degree polynomial", include_baseline = TRUE)
old <- tibble(
  method = c("Log-log", "OBLA 2.0", "OBLA 2.5", "OBLA 3.0", "OBLA 3.5", "OBLA 4.0",
             "Bsln + 0.5", "Bsln + 1.0", "Bsln + 1.5", "Dmax", "ModDmax", "Exp-Dmax",
             "Log-Poly-ModDmax", "Log-Exp-ModDmax", "LTP1", "LTP2", "LTratio"),
  version_0.3.0 = c(199.1, 237.7, 245.9, 253.1, 260.2, 265.0, 223.1, 235.3, 244.4,
                    232.0, 254.5, 241.9, 254.1, 260.7, 203.4, 259.9, 190.8)
)
test <- lactate_tests |> filter(test == "cycling_female")
new <- lactate_threshold(test, intensity, lactate, include_baseline = TRUE, plot = FALSE)
## the unrounded intensities, to compare at 0.1 W
unrounded <- attr(new, "lactater")$intensity
old |>
  inner_join(tibble(method = names(unrounded), version_1.0 = round(unname(unrounded), 1)), by = "method") |>
  mutate(difference = version_1.0 - version_0.3.0) |>
  knitr::kable()
```

| method           | version_0.3.0 | version_1.0 | difference |
|:-----------------|--------------:|------------:|-----------:|
| Log-log          |         199.1 |       199.3 |        0.2 |
| OBLA 2.0         |         237.7 |       238.2 |        0.5 |
| OBLA 2.5         |         245.9 |       246.9 |        1.0 |
| OBLA 3.0         |         253.1 |       254.0 |        0.9 |
| OBLA 3.5         |         260.2 |       260.2 |        0.0 |
| OBLA 4.0         |         265.0 |       265.8 |        0.8 |
| Bsln + 0.5       |         223.1 |       224.1 |        1.0 |
| Bsln + 1.0       |         235.3 |       236.2 |        0.9 |
| Bsln + 1.5       |         244.4 |       245.3 |        0.9 |
| Dmax             |         232.0 |       231.2 |       -0.8 |
| ModDmax          |         254.5 |       254.5 |        0.0 |
| Exp-Dmax         |         241.9 |       241.9 |        0.0 |
| Log-Poly-ModDmax |         254.1 |       254.3 |        0.2 |
| Log-Exp-ModDmax  |         260.7 |       260.0 |       -0.7 |
| LTP1             |         203.4 |       207.5 |        4.1 |
| LTP2             |         259.9 |       264.7 |        4.8 |
| LTratio          |         190.8 |       190.8 |        0.0 |

What moved the numbers:

- **OBLA and Bsln+.** The intensity at a target lactate is solved on the
  fitted curve itself (issue \#8, thanks to Simon Dierickx).
- **D_(max) family.** Each variant takes the point where the fitted
  curve lies furthest below its line, from its start to the last stage,
  with the ends where each paper put them (`dmax_anchor`); the Log-log
  start is computed without the resting sample.
- **Log-log.** Two straight lines that share the stage between them
  (Beaver et al., 1985), split at the stage with the lowest residual sum
  of squares.
- **LTP.** The exact least-squares optimum on the measured stages, found
  by exhaustive search, instead of an iterative fit that depended on its
  starting values.
- **LT_(ratio).** The exact minimum, within the exercise stages.
- **Heart rate.** Interpolated between the measured stages, so it
  follows the bend of the heart-rate curve.

See the [news](https://fmmattioni.github.io/lactater/news/index.md) for
the full list of changes.
