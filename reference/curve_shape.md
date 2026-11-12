# Did the lactate curve shift, or change its shape?

Tests whether two tests of the same athlete differ only by a horizontal
shift of the lactate curve, or also in its shape. Both tests are fitted
with the exponential plus constant,
`lactate = a + b * exp(c * intensity)` (Hughson et al., 1987), in which
a pure horizontal shift by `d` changes only `b`:

## Usage

``` r
curve_shape(before, after, level = 0.95)

# S3 method for class 'lactate_shape'
autoplot(object, ...)
```

## Arguments

- before, after:

  The results of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  for the two tests, with the same sport and at least five stages each.

- level:

  The confidence level of the shift. Defaults to `0.95`.

- object:

  The result of `curve_shape()`.

- ...:

  Not used.

## Value

a `lactate_shape` list with `shift` (intensity units, positive when the
curve moved to the right), `shift_low` and `shift_high`, `f`, `df1`,
`df2`, `p_value`, `aicc_shift` and `aicc_free`, `verdict`
(`"shape changed"` when the F test and AICc both prefer the free shape,
`"shift"` otherwise: none was detected), the peak lactate of both tests
and the two exponential `curves`. Print it for a summary, and plot it
with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html):
the two fitted curves, and the first one moved by the shift (dashed).

## Details

- **Shift only**: one shape for both tests (shared `a`, `b` and `c`),
  with the second test moved by `d`.

- **Free shape**: each test with its own `a`, `b` and `c`.

The two fits are compared with an extra-sum-of-squares F test (Motulsky
& Ransnas, 1987) and with AICc, the Akaike criterion corrected for small
samples (Hurvich & Tsai, 1989); the verdict is `"shape changed"` only
when both prefer the free shape. The shift `d` comes with a profile
confidence interval.

Read the result with care:

- The test finds a change of shape only when it is large and the tests
  are long. In simulated pairs of tests of a trained cyclist (analyzer
  error of 0.1 mmol/L plus 4%), a baseline 0.5 mmol/L higher was found
  in 96% of pairs with 8 stages per test and in 20% with 6; a curve
  steep enough to double the peak lactate in 55% and 10%. So "no shape
  change detected" is not proof of the same shape, least of all with 6
  stages.

- The p-value is approximate: the F test is exact only for models that
  are linear in their parameters, and a low p-value from one pair of
  curves needs a repeat before it counts (Motulsky & Ransnas, 1987). In
  the same simulation it fell below 0.05 in 8% to 10% of pairs with no
  change of shape; with AICc beside it, the verdict was wrong in 1% to
  6%.

- In this model, a shift to the right is the same as a proportional fall
  of lactate above baseline. Better endurance, a lower glycolytic rate
  (VLamax) at the same VO2max, low glycogen stores and overreaching can
  all produce it, so check the peak lactate, the diet before the test
  and the performance too (Maassen & Busse, 1989; Faude et al., 2009;
  Wackerhage et al., 2025). A shift to the left can likewise come from a
  higher VLamax, not only from lost endurance.

- The exponential cannot follow an early lactate dip; check both tests
  with
  [`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md)
  first.

## References

Faude O, Kindermann W, Meyer T. Lactate threshold concepts: how valid
are they? Sports Medicine. 2009;39(6):469-490.

Hurvich CM, Tsai CL. Regression and time series model selection in small
samples. Biometrika. 1989;76(2):297-307. doi:10.1093/biomet/76.2.297

Hughson RL, Weisiger KH, Swanson GD. Blood lactate concentration
increases as a continuous function in progressive exercise. Journal of
Applied Physiology. 1987;62(5):1975-1981.

Maassen N, Busse MW. The relationship between lactic acid and work load:
a measure for endurance capacity or an indicator of carbohydrate
deficiency? European Journal of Applied Physiology and Occupational
Physiology. 1989;58(7):728-737.

Motulsky HJ, Ransnas LA. Fitting curves to data using nonlinear
regression: a practical and nonmathematical review. FASEB Journal.
1987;1(5):365-374.

Wackerhage H, Kabasakalis A, Seiler S, Heck H. Is the vLamax for
glycolysis what the VO2max is for oxidative phosphorylation? Sports
Medicine. 2025;55(8):1853-1866.

## Examples

``` r
series <- subset(lactate_tests, athlete == "cyclist_series")
before <- lactate_threshold(subset(series, day == 0), intensity, lactate, plot = FALSE)
after <- lactate_threshold(subset(series, day == 58), intensity, lactate, plot = FALSE)
curve_shape(before, after)
#> Shift only vs free shape: F(2, 9) = 2.88, approximate p = 0.108; AICc -49.7 vs -43.8.
#> No shape change detected: one curve shifted by +24 W (95% CI 21 to 28) describes both tests, but with fewer than 8 stages the test finds few changes of shape.
#> Peak lactate 6.4 -> 7.9 mmol/L. A shift can also come from low glycogen or overreaching, not only fitness.
```
