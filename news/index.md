# Changelog

## lactater 1.0.0

CRAN release: 2026-11-13

Every method follows its original paper and is solved exactly; new tools
check the analysis, build zones, compare tests, predict performance and
report the test. [What’s new in lactater
1.0](https://fmmattioni.github.io/lactater/articles/whats-new.html)
shows how the numbers compare with 0.3.0, and [The methods and the
evidence](https://fmmattioni.github.io/lactater/articles/methods.html)
how each method is computed.

### Before you upgrade

- Some thresholds differ from 0.3.0: the methods now follow their
  original definitions and are solved exactly.
- The native pipe `|>` replaces `%>%`, which is no longer re-exported.
- `run_data_input()`, the RStudio gadget, is removed: the web form
  ([`recording_sheet()`](https://fmmattioni.github.io/lactater/reference/recording_sheet.md))
  records a test and
  [`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
  reads it back; seven packages leave Suggests with it.
- `loglog_restrainer` is deprecated (Log-log uses all stages, as Beaver
  et al., 1985, did); a value below 1 warns.
- Log-log needs at least five stages and LTP six (four with
  `ltp_breakpoints = 1`); a curve with more parameters than points is an
  error.
- [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  returns a `lactate_thresholds` tibble with `method_category` first;
  intensities are whole watts, km/h to 0.1 and m/s to 0.01.
- R 4.1 and ggplot2 4.0.0 or later are required; broom, dplyr, forcats,
  lubridate, magrittr, minpack.lm, pracma, segmented, stringr and tidyr
  are no longer imported.

### Methods

- Log-log: two lines sharing the division stage, the split with the
  lowest residual sum of squares (Beaver et al., 1985; Jamnick et al.,
  2018).
- Dmax family: the point of the curve furthest below the line; the
  line’s ends sit where each paper put them (`dmax_anchor`), `NA` when
  the curve never dips below the line; ModDmax counts a rise of exactly
  0.4 mmol/L.
- LTP1 and LTP2: the global least-squares optimum of three connected
  segments by exhaustive search; the segment before LTP1 can be held
  flat (`ltp_fix_first_slope`); one turn point for submaximal tests
  (`ltp_breakpoints = 1`).
- New `IAT` (Dickhuth et al., 1999) and `Stegmann` (Stegmann et al.,
  1981, from `recovery` samples and `stage_duration`).
- New `Keul` and `Simon` for running (Keul et al., 1979; Simon et al.,
  1981): the speed where the polynomial rises by 1.26 or 1 mmol/L per
  km/h.
- LTratio is the exact minimum of lactate / intensity on its B-spline;
  the exponential fit needs no starting values.
- OBLA, Bsln+ and IAT solve the curve exactly for the target
  ([\#8](https://github.com/fmmattioni/lactater/issues/8), thanks
  [@dierickxsimon](https://github.com/dierickxsimon); supersedes
  [\#10](https://github.com/fmmattioni/lactater/issues/10)); with
  several crossings, the last one where lactate rises through the
  target.
- Heart rate at a threshold is interpolated between the stages, `NA`
  outside them.
- Log-log and LTP fit the exercise stages only and report the lactate on
  their own lines.
- The resting sample is the row with intensity 0, placed one median step
  before the first stage when fitted.
- Own targets: `fixed` (OBLA), `baseline_plus` (Bsln+), `iat_rise`
  (IAT), `moddmax_rise` (ModDmax), `dmax_methods` (adds
  `"LTP1-Poly-ModDmax"`), `max_intensity` (drop stages above it).
- New `last_stage_duration`: the intensity of a last stage that was not
  completed is time-weighted (Kuipers et al., 2003);
  [`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
  does it from the form’s time in the last stage.
- New sport `"rowing"`; running, swimming and rowing get a `pace`
  column.
- Column arguments take a bare name, a string or a variable;
  `heart_rate_column` is optional; new `rpe_column` and `rpe_scale`.
- `.by` analyzes many tests in one call; rows with a missing lactate are
  dropped with a warning.
- Deterministic results, about 50x faster than 0.3.0 without plots (500
  -\> 10 ms for every method on `demo_data`) and nearly twice as fast
  with them.

### Checks and uncertainty

- `ci = TRUE`: wild-bootstrap intervals for the noise of the samples
  (`n_boot`, `ci_level`, `seed`); failed replicates are counted in a
  warning. Calibration in “Checking the analysis”.
- New
  [`lactate_checks()`](https://fmmattioni.github.io/lactater/reference/lactate_checks.md):
  stages, resting sample, thresholds not found or at the edge, early
  dip, no rise in the last stage, test stopped before LTP2, non-blood
  `sample`, and whether the LT1 and LT2 methods agree.
- New
  [`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md)
  (thanks [@Victorivus](https://github.com/Victorivus),
  [\#11](https://github.com/fmmattioni/lactater/issues/11)) and
  [`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md):
  residuals, leverage, Cook’s distance, every threshold without each
  stage, and a six-panel plot.
- New `sample`, `stage_duration`, `site` and `grade` record the
  protocol;
  [`predict_performance()`](https://fmmattioni.github.io/lactater/reference/predict_performance.md),
  [`reference_percentiles()`](https://fmmattioni.github.io/lactater/reference/reference_percentiles.md)
  and
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  check it against theirs.

### Plots and reports

- Every method’s plot explains itself: the method as title, how it works
  as subtitle, the construction drawn on the curve, the threshold
  labeled with its values. `autoplot(x)` draws an overview of every
  threshold on one axis, grouped by what the method aims at (the first
  rise, the second rise, a fixed lactate), with a table of values;
  `autoplot(x, method = )` one method.
- `autoplot(lactate_zones(x), view = "ruler")` draws the zones as one
  bar over the intensity, pace, heart rate and RPE at each boundary, to
  hand to the athlete.
- `options(lactater.style = "night")` and `"journal"`,
  `options(lactater.font = )`, and
  [`theme_lactater()`](https://fmmattioni.github.io/lactater/reference/theme_lactater.md)
  for your own plots.
- New
  [`lactate_report()`](https://fmmattioni.github.io/lactater/reference/lactate_report.md)
  (a gt table) and
  [`gt_lactater()`](https://fmmattioni.github.io/lactater/reference/gt_lactater.md);
  `options(lactater.web_font = FALSE)` leaves the web font out of the
  tables.
- New
  [`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md):
  the test as a web page and a two-page PDF for the athlete or the
  coach;
  [`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  and the `report_*()` steps build one part by part; two to five tests
  of one athlete give a progress report.
- New
  [`recording_sheet()`](https://fmmattioni.github.io/lactater/reference/recording_sheet.md):
  a web form, a printable sheet and a CSV template;
  [`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
  reads the form’s .xlsx.

### Zones, comparisons, performance

- `lactate_threshold(lt1 = , lt2 = )` chooses the athlete’s LT1 and LT2
  once: the print, the overview, the zones, the report and the headers
  of
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  and
  [`predict_performance()`](https://fmmattioni.github.io/lactater/reference/predict_performance.md)
  follow that pair.
- [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  sets every change against the `noise` of its method (what two tests
  differ by when nothing changed), with their `ratio` and a verdict of
  “beyond” or “within the noise”, and shows the `cv` behind it. The
  default noise of each method is lactater’s own calculation on the open
  data of Heuberger et al. (2018), in `data-raw/reliability_cycling.R`;
  the help page lists the published values beside it.
  [`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
  gives the `bias` between tests, the interval of the `cv` and its
  degrees of freedom, and
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  widens its intervals for a `cv` from few athletes;
  [`lactate_trend()`](https://fmmattioni.github.io/lactater/reference/lactate_trend.md)
  takes a `level`.
- New
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md):
  three or five zones from LT1 and LT2 (Bsln + 0.5 and Log-Poly-ModDmax
  by default) at the intensity domains, with intensity, pace, lactate,
  heart rate and RPE; `split` sets the cuts inside the domains. Heart
  rate is drawn in a panel under the curve, or on a second axis
  (`heart_rate = "axis"`).
- New
  [`training_load()`](https://fmmattioni.github.io/lactater/reference/training_load.md):
  time in each zone and the polarization index (Treff et al., 2019).
- New
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md):
  each change against the test-retest noise of the method, with a
  verdict and notes on what differs between the tests;
  [`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
  measures that noise on your own athletes;
  [`curve_shift()`](https://fmmattioni.github.io/lactater/reference/curve_shift.md),
  [`curve_shape()`](https://fmmattioni.github.io/lactater/reference/curve_shape.md)
  (shift or new shape, by an F test and AICc),
  [`lactate_trend()`](https://fmmattioni.github.io/lactater/reference/lactate_trend.md)
  (series of tests),
  [`relative_thresholds()`](https://fmmattioni.github.io/lactater/reference/relative_thresholds.md)
  (per kg, % of peak).
- New
  [`reference_percentiles()`](https://fmmattioni.github.io/lactater/reference/reference_percentiles.md):
  percentiles of the power at 3 mmol/L and at baseline + 1 mmol/L among
  German adults of the same sex and age (Fiedler et al., 2025).
- New
  [`predict_performance()`](https://fmmattioni.github.io/lactater/reference/predict_performance.md)
  (running from Roecker et al., 1998, and Sjodin & Jacobs, 1981; cycling
  from the open data of Heuberger et al., 2018) and
  [`calibrate_performance()`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md)
  for your own equation.
- New
  [`lactate_mlss()`](https://fmmattioni.github.io/lactater/reference/lactate_mlss.md)
  (the maximal lactate steady state from constant-load bouts, Beneke’s
  10 to 30 min window) and
  [`vlamax()`](https://fmmattioni.github.io/lactater/reference/vlamax.md)
  (the maximal lactate accumulation rate from a sprint test).

### Data and documentation

- New `lactate_tests`: 16 step tests across cycling, running, rowing and
  swimming, from elite to untrained; twelve real (Trinity College Dublin
  and Heuberger et al. 2018, CC BY 4.0), four simulated. `demo_data`
  gains a simulated RPE.
- New articles on best practices, the methods and the evidence, checking
  the analysis, zones, training load, comparing tests, predicting
  performance, the example data and what is new; every citation links to
  its paper.

## lactater 0.3.0

- Improved
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  performance by reducing repeated model preparation and minimizing
  expensive data wrangling in method pipelines.
- Optimized plotting performance by removing
  [`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  from x-axis adjustment and simplifying plot construction paths.
- Added stronger input/model error handling for edge cases in fitting
  and segmentation routines.
- Expanded test coverage with validation and golden-output regression
  tests for numerical stability.
- Updated CI quality checks and coverage gates to better protect
  performance and correctness changes.

## lactater 0.2.0

CRAN release: 2023-11-16

- Added the function
  [`lactate_curve()`](https://fmmattioni.github.io/lactater/reference/lactate_curve.md)
  to retrieve data for plotting the lactate curve and heart rate
  response.

## lactater 0.1.4

CRAN release: 2023-04-16

- Patched a modification to the `Dmax` method, which was producing
  negative values in a few edge cases.

- Make sure that infinite values are not retrieved when converting
  intensity to log.

## lactater 0.1.3

- Patched a fix to the `Dmax` method that was producing very high
  estimations in a few cases.

## lactater 0.1.2

CRAN release: 2023-02-22

- Fixed missing log in intensity in the `Log-log` method.

## lactater 0.1.1

CRAN release: 2022-09-08

- Removed `align` attribute in `img` tag.

## lactater 0.1.0

CRAN release: 2021-12-03

- Added a `NEWS.md` file to track changes to the package.
