# Check a lactate threshold analysis

Runs a set of checks on the test and the results of
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md),
so problems that are easy to miss in a table of numbers are flagged: too
few stages, no resting sample, thresholds that were not reached or sit
at the edge of the tested range, an early lactate dip or a final
plateau, a test that may have stopped before the second lactate turn
point, and methods that rely on fixed blood lactate concentrations used
on another sample type.

## Usage

``` r
lactate_checks(x)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

## Value

a tibble with one row per check: `check`, `status` (`"ok"`, `"note"`, or
`"warning"`), and `details`. It prints as a checklist, with a tick for
each check that passed.

## References

Hofmann P, Pokan R, von Duvillard SP, Seibert FJ, Zweiker R, Schmid P.
Heart rate performance curve during incremental cycle ergometer exercise
in healthy young male subjects. Medicine & Science in Sports & Exercise.
1997;29(6):762-768.

Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ.
Reliability and accuracy of six hand-held blood lactate analysers.
Journal of Sports Science and Medicine. 2015;14:203-214.

Foxdal P, Sjodin B, Rudstam H, Ostman C, Ostman B, Hedenstierna GC.
Lactate concentration differences in plasma, whole blood, capillary
finger blood and erythrocytes during submaximal graded exercise in
humans. European Journal of Applied Physiology. 1990;61(3-4):218-222.

## Examples

``` r
results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
lactate_checks(results)
#> 10 checks: 9 passed, 1 note, 0 warnings
#> ℹ Stages             7 stages: the LTP fit has 6 parameters, so 1 degree of freedom is left and LTP can move a lot with one stage (at least 8 stages are better); the fitted curves rest on few points too.
#> ✔ Resting sample     Present.
#> ✔ Thresholds found   All thresholds were found.
#> ✔ Edge of the range  No threshold sits at the first or last stage.
#> ✔ Early lactate dip  No early dip.
#> ✔ Final stage        Lactate rose in the last stage.
#> ✔ Peak lactate       8.6 mmol/L.
#> ✔ Sample type        blood lactate.
#> ✔ LT1                Within 18% of peak (5 methods, LTratio 77 to LTP1 111).
#> ✔ LT2                Within 22% of peak (8 methods, Bsln + 1.5 118 to LTP2 160).
```
