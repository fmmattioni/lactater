# lactater (development version)

## Corrected results

* OBLA and Bsln+ now solve the fitted curve for the target lactate instead of
  interpolating linearly between the fitted values at the measured stages. On a
  convex lactate curve the old shortcut under-estimated the intensity by about
  1-2 W on `demo_data` (more with larger steps) (#8, thanks @dierickxsimon for
  the report and for #10, which this supersedes).
* The "target not reached" check for OBLA and Bsln+ now uses the fitted curve
  over the whole tested range, not just the fitted values at the measured
  stages.
* All Dmax variants now take the point where the fitted curve lies furthest
  *below* the line. Before, the polynomial variants picked a root of the
  derivative by position and fell back to the other root when lactate passed
  8 mmol/L, so curves that level off at the end (a plateau or drop in the last
  stage, or sweat lactate) could get a Dmax up to ~175 W too high, and the
  result depended on the lactate scale. The 8 mmol/L fallback and its warnings
  are gone; `NA` is returned when the curve never dips below the line.
* The Dmax line now runs from the first to the last stage. Before, its slope
  came from the range (max - min) of lactate and intensity, which differs when
  the first stage is not the lowest value (an early lactate dip). ModDmax now
  ends at the last stage, not at the highest lactate.
* The Log-log threshold used by Log-Poly-ModDmax and Log-Exp-ModDmax is now
  always computed without the resting sample, as documented, so the Dmax
  family no longer changes with `include_baseline` (on `demo_data` with
  `include_baseline = TRUE` they move from 143.0/145.7 W to 151.4/153.7 W).
  It is also computed once instead of twice, and the line starts from the
  unrounded threshold.
* ModDmax's first rise of 0.4 mmol/L is now detected reliably: `1.4 - 1.0` is
  `0.3999...` in floating point and was missed.

* Data without a resting sample no longer loses its first stage. The resting
  sample is now the row with intensity 0 (before: always row 1), so
  `include_baseline = FALSE`, the Dmax family and the heart-rate model drop
  only a real resting sample.
* The resting sample is placed one *median* step before the first stage
  (before: `intensity[3] - intensity[2]`), so a missed stage no longer shifts
  it.
* LTratio is now searched within the exercise stages only. Before, it could
  land on the positioned resting sample (e.g. `-0.1 W` when the first stage
  was smaller than the step, or the resting speed in swimming). Its plot now
  uses the B-spline the value comes from, not the user-selected fit.

* Heart rate at each threshold is now interpolated between the measured
  stages instead of read off one straight line fitted through the whole test.
  The heart-rate curve bends at high intensities, so the straight line was
  2-6 bpm too low in the middle of `demo_data`. Outside the stages (e.g. a
  threshold below the first stage) the heart rate is `NA`.
* `lactate_curve()$heart_rate_response` now holds the measured heart rate at
  each stage (a line through these points is the interpolation used for the
  thresholds), instead of a fitted straight line.

* Log-log and LTP breakpoints are now found by an exhaustive search for the
  lowest residual sum of squares on the *measured* stages, as defined by
  Jamnick et al. (2018) ("the intersection point of the two lines with the
  lowest residuals sum of squares"), with at least two stages per segment.
  Before, `segmented` was run on ~1,600 linearly interpolated points per test:
  the interpolated points counted as data, and the iterative fit could stop in
  a local optimum (the old LTP breakpoints on the noisy fixture have 1.9x the
  residual error of the best fit). Results are deterministic and Log-log/LTP
  numbers change (e.g. `demo_data` with the resting sample: Log-log 83.4 ->
  85.3 W, LTP2 147.6 -> 157.3 W). LTP now needs at least six stages.
* New `ltp_fix_first_slope` fixes the slope before LTP1 at zero.

* LTratio is now the exact minimum of lactate / intensity instead of the
  nearest point on a 0.1 W (0.1 km/h, 0.01 m/s) grid.

## New look

* Every plot was redesigned: the method as the title, the threshold values
  (intensity, lactate, heart rate, and pace for swimming) as the subtitle,
  measured stages as dots with white rings, the fitted curve in red, the
  threshold in blue with thin guides to both axes, the fit in the caption, and
  axis titles with units. Log-log is drawn on log axes in real units. Colours
  are checked for colour-vision deficiency.
* New `theme_lactater()`, the ggplot2 theme behind every plot, to make your own
  plots match.
* `lactate_threshold()` now returns a `lactate_thresholds` tibble that prints
  a one-line summary of the test and can be plotted directly: `autoplot(x)`
  (or `plot(x)`) draws an overview with the fitted curve on top and every
  threshold below on the same intensity axis, one row per method, so you can
  see how much the methods agree; `autoplot(x, method = "Dmax")` draws one
  method. `autoplot()` is re-exported from ggplot2. The `plot` column is still
  there with `plot = TRUE`.

## Faster and leaner

* `lactate_threshold()` is ~18x faster than 0.3.0 (426 -> 24 ms for all
  methods on `demo_data` without plots).
* The exponential fit no longer needs starting values and cannot fail to
  converge: for a fixed rate the model is linear, so only the rate is searched
  (variable projection). Results match the previous `minpack.lm` fit on all
  test datasets.
* Eight dependencies were dropped: broom, dplyr, forcats, ggtext, lubridate,
  minpack.lm, stringr and tidyr (plus pracma and segmented, above). ggplot2
  4.0.0 or later is now required.
* `method_category` is now the first column of the output.
* A column name that is not in the data now gives a clear error.

## Data handling

* Rows with a missing intensity or lactate are dropped with a warning instead
  of stopping the analysis (a missed sample should not void a test).
* Lactate values must now be `> 0` (a 0 is usually an empty cell, and the
  log-based methods failed on it with an unclear error).
* `intensity` must be strictly increasing; this is now checked up front with a
  clear message.

## New methods

* `IAT`: the individual anaerobic threshold of Dickhuth et al. (1999), the
  intensity 1.5 mmol/L above the lactate at LTratio on the same B-spline curve.
  It is part of the default `method` set.
* `vlamax()` estimates the maximal lactate accumulation rate from a separate
  all-out sprint test: (peak post-sprint lactate - pre-sprint lactate) /
  (sprint duration - alactic time), with the alactic time fixed at 3.5 s by
  default, the most reliable option in Meixner et al. (2024). Its help page
  explains the protocol and why VLamax should be compared only within the same
  protocol.
* `dmax_methods` chooses the Dmax variants, and adds `"LTP1-Poly-ModDmax"`
  (a line from LTP1 instead of Log-log) on request.
* `max_intensity` leaves stages above an intensity out of every method, for
  example an incomplete last stage or a final plateau.

## Analysis tools

* `lactate_zones()` builds training zones (zone 1 below LT1, zone 2 between,
  zone 3 above LT2; Hofmann & Tschakert, 2017) from the thresholds you choose
  as LT1 and LT2 (any method in the results, or an intensity), with intensity,
  pace, lactate and heart-rate ranges and a plot.
* `ci = TRUE` adds bootstrap confidence intervals (`intensity_low`,
  `intensity_high`) to every threshold: residuals around the fitted curve are
  resampled (leverage-adjusted, Davison & Hinkley, 1997) and every method is
  run again. Plots show them as bands and whiskers. On `demo_data` OBLA moves
  by about +/-2 W, Dmax by about +/-4 W and the breakpoint methods by 15-35 W,
  so reporting a threshold to 0.1 W is false precision.
* `lactate_checks()` flags problems: too few stages, no resting sample,
  thresholds not found or at the edge of the tested range, an early lactate dip,
  no rise in the last stage, and fixed-concentration methods used on a
  non-blood `sample`. The printed result says how many checks need attention.
* `lactate_fit_quality()` fits every curve to the same points and reports
  R-squared, RMSE, standard error of the estimate and the largest residual, to
  help choose `fit` (thanks @Victorivus for the idea and #11).
* `compare_tests()` compares two tests of the same athlete, method by method,
  with a plot of both curves.
* `.by` analyses many tests in one call (e.g. `.by = c("athlete", "date")`).
* New `sample` argument (`"blood"`, `"plasma"`, `"sweat"`,
  `"interstitial fluid"`), used by `lactate_checks()`.
* New sport `"rowing"` (power in W) with the Concept2 split (s/500 m). Running
  now gets a `pace` column too (s/km), and plot subtitles show pace as m:ss.
* `heart_rate_column` now defaults to `NULL` (no heart rate), so it can be
  passed programmatically.

## New arguments

* `dmax_anchor` chooses where the ends of the Dmax lines sit: `"fitted"`
  (default, on the fitted curve, as in Jamnick et al. 2018) or `"observed"`
  (at the measured lactate values, as in Heuberger et al. 2018).
* `moddmax_rise` sets the ModDmax first-rise criterion (default 0.4 mmol/L, a
  blood lactate value), for example for sweat lactate.

## Internal

* `pracma` and `segmented` are no longer dependencies.

# lactater 0.3.0

* Improved `lactate_threshold()` performance by reducing repeated model preparation and minimizing expensive data wrangling in method pipelines.
* Optimized plotting performance by removing `ggplot_build()` from x-axis adjustment and simplifying plot construction paths.
* Added stronger input/model error handling for edge cases in fitting and segmentation routines.
* Expanded test coverage with validation and golden-output regression tests for numerical stability.
* Updated CI quality checks and coverage gates to better protect performance and correctness changes.

# lactater 0.2.0

* Added the function `lactate_curve()` to retrieve data for plotting the lactate curve and heart rate response.

# lactater 0.1.4

* Patched a modification to the `Dmax` method, which was producing negative values in a few edge cases.

* Make sure that infinite values are not retrieved when converting intensity to log.

# lactater 0.1.3

* Patched a fix to the `Dmax` method that was producing very high estimations in a few cases.

# lactater 0.1.2

* Fixed missing log in intensity in the `Log-log` method.

# lactater 0.1.1

* Removed `align` attribute in `img` tag.

# lactater 0.1.0

* Added a `NEWS.md` file to track changes to the package.
