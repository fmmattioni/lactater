# Check the lactate curve and how much each threshold depends on single stages

Regression diagnostics for the fitted lactate curve, adapted to tests of
6 to 10 stages, and a leave-one-out check of every threshold. Print it
for a short summary, and plot it with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
for six panels, each with a line on what to look for; they need a tall
plot, about 9 x 10 inches, and the article [Checking the
analysis](https://fmmattioni.github.io/lactater/articles/checking.html)
shows them panel by panel.

## Usage

``` r
lactate_diagnostics(x)

# S3 method for class 'lactate_diagnostics'
autoplot(object, ...)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- object:

  The result of `lactate_diagnostics()`.

- ...:

  Not used.

## Value

a `lactate_diagnostics` object: a list with `stages` (one row per stage
used in the fit: `intensity`, `lactate`, `fitted`, `residual`,
`standardized`, `leverage`, `cooks_distance`, `influential`),
`leave_one_out` (`method`, `stage` left out, `intensity` without it,
`change`), `jackknife` (`method`, `intensity`, `lowest`, `highest`,
`jackknife_se`), and `fits`
([`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md),
best prediction error first).

## Details

- **Residuals**: the measured lactate minus the curve. A run of
  residuals of the same sign, or an arch against the fitted values,
  means the shape of the curve is wrong for this test.

- **Standardized residuals**: residuals divided by their standard error.
  With so few stages, normality and equal-variance tests have almost no
  power, so a check that "passes" means "could not tell", not "fine";
  analyzer error also grows with lactate (Bonaventura et al., 2015).

- **Leverage and Cook's distance**: how much one stage pulls the curve
  (Cook, 1977). The first and last stages always have high leverage, by
  design. A stage is flagged when its Cook's distance is above the
  median of the F(p, n - p) distribution, where p is the number of curve
  terms; rules such as 2p/n for leverage cannot flag anything when p is
  close to n.

- **Leave one stage out**: every threshold is computed again without
  each stage in turn (a jackknife; Efron, 1982). The range shows how
  much a threshold rests on one stage, and the jackknife standard error
  is `sqrt((n - 1) / n * sum((theta_i - mean(theta_i))^2))`. Dropping
  the first or last stage changes the Dmax line itself, so large changes
  there are expected for the Dmax family. The resting sample is left out
  too (column "rest"), which shows how much Bsln+ rests on that one
  baseline sample (with `include_baseline = TRUE` the curve changes too,
  so the curve methods move as well); it is not part of the jackknife.

- **Which curve predicts best**: the leave-one-out prediction error of
  every curve type, from
  [`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md).
  It rewards curves that follow the data without overfitting it, which
  the RMSE of the fit alone cannot do.

## References

Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ.
Reliability and accuracy of six hand-held blood lactate analysers.
Journal of Sports Science and Medicine. 2015;14:203-214.

Cook RD. Detection of influential observation in linear regression.
Technometrics. 1977;19(1):15-18.

Efron B. The jackknife, the bootstrap and other resampling plans.
Philadelphia: SIAM; 1982.

## Examples

``` r
female <- subset(lactate_tests, test == "cycling_female")
results <- lactate_threshold(female, intensity, lactate, heart_rate, plot = FALSE)
diagnostics <- lactate_diagnostics(results)
diagnostics
#> Lactate curve: 3rd degree polynomial on 10 points; residual SD 0.14 mmol/L.
#> No stage has a Cook's distance above 0.94 (the median of F(4, 6)).
#> Most sensitive to single stages: Log-log (jackknife SE 12 W), Dmax (jackknife SE 12 W), LTP1 (jackknife SE 12 W).
#> Best-predicting curve: 3rd degree polynomial, which missed each stage by 0.17 mmol/L on average when fitted without it (root-mean-square leave-one-out error).
## six panels: draw them in a tall window, about 9 x 10 inches
if(interactive()) autoplot(diagnostics)
```
