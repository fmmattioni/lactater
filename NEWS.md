# lactater 1.0.0

lactater 1.0.0 brings methods that follow their original papers, exact and
deterministic calculations, plots that explain themselves, tools to compare
tests and to measure training load, and real example data. See "What's new in
lactater 1.0" on the website for how the numbers compare with 0.3.0.

## Before you upgrade

* Some thresholds differ from 0.3.0, because the methods now follow their
  original definitions and are solved exactly (details below).
* lactater uses the native pipe `|>` and no longer re-exports `%>%`; load
  magrittr or dplyr if your scripts use it.
* `loglog_restrainer` is deprecated: Log-log uses all stages, as Beaver et al.
  (1985) did. A value below 1 still works for now, with a warning.
* Log-log needs at least five stages and LTP at least six (four with
  `ltp_breakpoints = 1`).
* `lactate_threshold()` returns a `lactate_thresholds` tibble with
  `method_category` first, and shows intensities as whole watts, km/h to 0.1
  and m/s to 0.01.
* R 4.1 and ggplot2 4.0.0 or later are required.

## Methods that follow the original papers

* Log-log follows Beaver et al. (1985): the stages are split at a measured
  stage that belongs to both segments, each segment gets its own straight line,
  and the threshold is where the two lines cross. The split is the one with
  "the lowest residuals sum of squares" (Jamnick et al., 2018), with at least
  three stages per line.
* The Dmax lines start and end where each paper put them (`dmax_anchor =
  "original"`, the default): on the fitted curve for Dmax and Exp-Dmax ("the two
  end points of the curve", Cheng et al., 1992), from the measured stage to the
  measured last stage for ModDmax ("the LT and the final lactate point", Bishop
  et al., 1998), and to the measured last stage for Log-Poly-ModDmax and
  Log-Exp-ModDmax (Jamnick et al., 2018). `"fitted"` and `"observed"` put every
  end on the curve or at the measured values.
* Every Dmax variant takes the point where the fitted curve lies furthest below
  a line from its start to the last stage, so curves that level off at the end
  (a final plateau, or sweat lactate) get a sensible Dmax at any lactate scale,
  and `NA` when the curve never dips below the line. The Log-log start of the
  Log-*-ModDmax variants is always computed without the resting sample, as
  documented, and ModDmax detects a rise of exactly 0.4 mmol/L reliably.
* LTP1 and LTP2 are the global least-squares optimum of three connected
  straight segments on the measured stages, found by exhaustive search, so they
  never depend on starting values. `ltp_fix_first_slope` keeps the segment
  before LTP1 flat. Like Log-log, LTP fits the exercise stages only: the
  resting sample has no exercise intensity. The original papers fitted each turn point in its own window
  on tests with 1-min or 90-s stages (Hofmann et al., 1997; Pokan et al., 1997);
  those windows hold too few of the 6-8 stages of a typical step test, so the
  segments are fitted together.
* New `IAT`: the individual anaerobic threshold of Dickhuth et al. (1999), the
  intensity 1.5 mmol/L above the lactate at LTratio on the same B-spline curve.
* New `Stegmann`: the individual anaerobic threshold of Stegmann et al. (1981),
  the tangent to the lactate curve from the point in recovery where lactate is
  back at its end-of-exercise value. Give the recovery samples as `recovery`
  (minutes and lactate) and the stage duration as `stage_duration`; the plot
  shows the recovery samples, the return to the end value and the tangent.
* LTratio is the exact minimum of lactate / intensity within the exercise
  stages, on the B-spline it comes from.
* The exponential fit needs no starting values and always converges: for a fixed
  rate the model is linear, so only the rate is searched, in the positive range
  as in Hughson et al. (1987).

## Exact numbers

* OBLA, Bsln+ and IAT solve the fitted curve exactly for the target lactate (#8,
  thanks @dierickxsimon for the report and for #10, which this supersedes).
  When the curve crosses a target more than once, the last crossing where
  lactate rises through it is used.
* Heart rate at each threshold is interpolated between the measured stages, so
  it follows the bend of the heart-rate curve; outside the stages it is `NA`.
  `lactate_curve()$heart_rate_response` holds the measured heart rate.
* Log-log and LTP report the lactate on their own fitted lines, so the table,
  the plot label and the plotted point agree.
* The resting sample is the row with intensity 0, wherever it is, and is placed
  one median step before the first stage; tests without a resting sample keep
  all their stages.
* Results are deterministic, and `lactate_threshold()` is ~18x faster than
  0.3.0 (426 -> 24 ms for all methods on `demo_data` without plots).

## Flexible methods

* `fixed` sets the OBLA concentrations (default `c(2, 2.5, 3, 3.5, 4)`),
  `baseline_plus` the Bsln+ rises (default `c(0.5, 1, 1.5)`) and `iat_rise` the
  IAT rise above LTratio (default 1.5 mmol/L), so any fixed or baseline-relative
  threshold can be computed, e.g. `fixed = 3.2` or `baseline_plus = 0.2`.
* `ltp_breakpoints = 1` fits a single turn point (LTP1) for a submaximal test
  that stopped before LTP2; `lactate_checks()` suggests it when peak lactate
  stays below the ~4 mmol/L where LTP2 usually sits (Hofmann et al., 1997).
* `dmax_methods` chooses the Dmax variants and adds `"LTP1-Poly-ModDmax"` (a
  line from LTP1) on request; `moddmax_rise` sets the ModDmax first rise
  (default 0.4 mmol/L, measured in fingertip plasma by Bishop et al., 1998), for
  example for sweat lactate.
* `max_intensity` leaves out stages above an intensity, for example an
  incomplete last stage.
* New sport `"rowing"` (power in W, with the Concept2 split in s/500 m); running
  and swimming get a `pace` column.
* Column arguments take a bare name, a string, or a variable that holds the
  name; `heart_rate_column` is optional.
* New `rpe_column` and `rpe_scale` (`"6-20"` or `"CR10"`): the rating of
  perceived exertion at each threshold, in the zones, the comparison, the plot
  labels and the report.

## Plots that explain themselves

* Every plot shows how its method works: the method as the title, one line on
  how it works as the subtitle, the fitted curve in blue, the measured stages as
  open dots, and the threshold in green with dotted guides to both axes and a
  label (intensity, then pace, lactate and heart rate) joined to it by a curved
  arrow. The Dmax plots show their line, Bsln+ and IAT
  the rise above the baseline or LTratio, and Log-log and LTP their fitted
  straight lines, dashed beyond the part they fit. Axis lines, no grid, lactate
  axes in whole numbers, and colors checked for color-vision deficiency.
  Subtitles and captions wrap to the width of the plot (ggtext), the arrow
  stops at the edge of the threshold dot whatever the size of the plot, and
  method names carry their subscripts (D<sub>max</sub>, ModD<sub>max</sub>).
* `autoplot(x)` draws an overview, in the style of a forest plot: the fitted
  curve on top and one row per threshold below on the same intensity axis,
  grouped by method family, with confidence intervals when computed, and a
  table beside it with the lactate, heart rate, RPE, % of the peak power or
  speed and % of the highest heart rate (HR<sub>max</sub>) at each threshold. `autoplot(x, method = "Dmax")` draws one method; `autoplot()` is
  re-exported from ggplot2.
* `options(lactater.style = "night")` gives every plot a dark style and
  `"journal"` a quieter print style (an ink curve with one amber accent);
  `options(lactater.font = )` sets the font, annotations included.
  `theme_lactater()` makes your own plots match.
* New `lactate_report()` formats the results as a gt table for reports and
  papers, grouped by method family, with confidence intervals and the checks
  that need attention as notes.
* New `test_report()` writes the whole test as one web page for the athlete or
  the coach, two A4 pages when printed (`pdf = TRUE` prints the PDF with
  pagedown). The first page answers the question: LT1 and LT2, what they mean,
  and the zones they make. The second keeps the record: the predicted
  performance from the chosen thresholds, what to double-check, the protocol and
  stages, and every method.
* Build a report step by step with `report_start()`, `report_header()` (the
  athlete's details, your lab's name and logo), `report_summary()`,
  `report_zones()`, `report_prediction()`, `report_checks()`, `report_test()`,
  `report_methods()`, `report_text()` and `report_page_break()`, then
  `report_save()`; printing a report previews it.
* Give `test_report()` or `report_start()` two to five tests of one athlete and
  their `dates`, and the report shows the progress (`report_progress()`): how
  LT1 and LT2 moved since the first test, every lactate curve above the zones of
  each test, the tests side by side, and whether the latest change is larger
  than the noise of the method. The latest test follows on the second page.
* New `gt_lactater()` gives your own gt tables the look of lactater's.
* New `recording_sheet()` copies a printable sheet, a web form and a CSV
  template into your folder, to record every test the same way (the sheet and
  the form are also on the website). The form saves itself in the browser and
  downloads an .xlsx (stages, recovery samples and the test details) or a CSV;
  new `read_recording()` turns the file into the inputs of `lactate_threshold()`
  and `test_report()`.

## Uncertainty, checks and zones

* `ci = TRUE` adds a bootstrap interval to every threshold for the noise of the
  samples: each stage keeps its own residual with a random sign (a wild
  bootstrap, as lactate error grows with concentration), widened for the few
  degrees of freedom of a step test, and every method is run again. In a
  simulation of 150 tests (`data-raw/bootstrap_coverage.R`) the 95% intervals
  held the true threshold in 84-97% of tests; OBLA falls short by the bias of
  the cubic fit.
* `lactate_checks()` flags what is easy to miss: too few stages, no resting
  sample, thresholds not found or at the edge of the tested range, an early
  lactate dip, no rise in the last stage, a test that may have stopped before
  LTP2, fixed-concentration methods used on a non-blood `sample`, and an LTP
  fit with fewer than two degrees of freedom left (fewer than 8 stages). It
  prints as a checklist, with a tick for each check that passed.
* `lactate_fit_quality()` compares the curves on your data (R-squared, RMSE,
  standard error of the estimate, largest residual, and the leave-one-out
  prediction error, which does not reward overfitting) to help choose `fit`
  (thanks @Victorivus for the idea and #11).
* New `lactate_diagnostics()`: residuals, leverage and Cook's distance of the
  curve (flagged against the median of F, which works with 6 to 10 stages),
  every threshold computed again without each stage (range and jackknife
  standard error), and a six-panel plot with a line under each panel on what
  to look for. A grid shows how far each threshold moves without each stage,
  so a threshold that rests on one stage, or a stage many thresholds rest on,
  stands out; a "rest" column shows how much Bsln+ rests on its baseline
  sample.
* `lactate_zones()` builds three or five training zones from the thresholds
  you choose as LT1 and LT2, with the zone boundaries at the intensity domains
  (moderate below LT1, heavy from LT1 to LT2, severe above LT2). By default LT1
  is Bsln + 0.5 and LT2 Log-Poly-ModDmax, the pair with the best evidence for
  each boundary (Jamnick et al., 2020, 2018) that also moves least with a
  single stage. Five zones split the moderate domain at 80% of LT1 and the
  severe domain halfway between LT2 and the peak, from the oxygen uptake bands
  of Seiler (2010), at Seiler's 1.5 and 6.0 mmol/L, or at your own
  intensities. Each zone comes with intensity,
  pace, lactate, heart-rate and RPE ranges, and the plot colors the zones
  green, yellow and red by domain. When the test has heart rate, the plot shows
  it in a panel under the curve, read at LT1 and LT2; `heart_rate = "axis"`
  puts it on the lactate plot against a second axis instead, as many lab
  reports do (`options(lactater.heart_rate = "axis")` for every plot and
  report).
* `.by` analyses many tests in one call (e.g. `.by = c("athlete", "date")`).
* Rows with a missing intensity or lactate are dropped with a warning, so a
  missed sample does not void a test; impossible inputs (lactate of 0,
  intensities that do not increase, unknown columns) get a clear message.

## Comparing tests

* `compare_tests()` says whether each change is larger than the noise of the
  method: the change comes with an interval from the test-retest variation
  (published values for IAT, OBLA 4.0, Dmax, ModDmax and Bsln+, or your own
  `cv`), and a `verdict` (increase, decrease, unclear) against an optional
  smallest worthwhile change. It lists the heart rate at each threshold in both
  tests, and notes what could make the comparison unfair (a different protocol,
  stage length, sampling site, treadmill grade, sample type or resting sample,
  or a fall in peak lactate).
* New `lactate_reliability()`: the typical error of each threshold, in units
  and as a CV, from your own repeated tests (Hopkins, 2000), for
  `compare_tests(cv = )`.
* New `curve_shift()`: the intensity gained at each lactate and the lactate saved
  at chosen intensities; the comparison plot shows the shift at 2 and 4 mmol/L.
* New `lactate_trend()`: a straight-line trend with its 90% interval for each
  threshold over three or more tests. Its plot draws up to five tests as their
  lactate curves, light to dark by date, then their heart rate in the same
  colors with LT1 and LT2 as filled points, above the five zones of each test
  (`lt1`, `lt2`), and more tests as each threshold over time against a band for
  the noise of the method around the first test (`view = "thresholds"`). `day`
  takes dates, which then name the tests. A list of tests can be spliced in with
  `!!!`.
* New `reference_percentiles()`: the percentile of the power at 3 mmol/L and at
  baseline + 1 mmol/L, in W and W/kg, among people of the same sex and age in
  Germany (14-64 years), from the reference values of Fiedler et al. (2025),
  whose thresholds were computed with lactater. Both thresholds are computed as
  the reference values were, on a 3rd degree polynomial with the resting sample.
  For the general population, not athletes; it warns when the steps, the stage
  length or the sampling site differ from the reference tests. `report_reference()` adds it to a report.
* New `curve_shape()`: did the curve only shift, or change its shape? Both
  tests are fitted with one exponential shape moved by a shift, and with a
  shape each; an F test and AICc compare the two, and the shift comes with a
  confidence interval. The cyclist in `lactate_tests` shifted by +24 W in the
  first 58 days (no evidence of a shape change, p = 0.11) and changed shape
  by day 2046 (p = 0.004).
* New `relative_thresholds()`: thresholds per kg, as % of peak intensity, and
  heart rate as % of peak, to compare athletes.

## Predicting performance

* New `predict_performance()`: race times from a step test with a 95%
  prediction range. Running: 5 km to the marathon from IAT (Roecker et al.,
  1998, 427 runners) and the marathon from OBLA 4.0 (Sjödin & Jacobs, 1981).
  Cycling: 45-min time-trial power from six thresholds, fitted with lactater's
  own methods on the open data of Heuberger et al. (2018). It warns when the
  steps, stage length, sampling site or treadmill grade of the test differ from
  the source protocol, or a threshold lies outside the source sample.
* New `calibrate_performance()`: fit your own equation on your athletes' tests
  and races, for any threshold, sport and protocol, and use it in
  `predict_performance()`.

## Training load

* New `training_load()`: sorts heart-rate, power or speed samples of training
  sessions into the lactate zones and returns the time in each zone, the
  training intensity distribution across the domains with the polarization
  index (Treff et al., 2019), per session or per period with `.by`. It prints
  a one-line summary and plots the share of time in each zone.

## Example data

* New `lactate_tests`: 16 step tests in long format across cycling, running,
  rowing and swimming, from elite to untrained, including one cyclist tested six
  times over five years, an early lactate dip, a test without a resting sample,
  5-min steps and a submaximal test. Twelve are real (Trinity College Dublin
  open data and Heuberger et al. 2018, both CC BY 4.0, credited in
  `?lactate_tests` and `COPYRIGHTS`); four are simulated from published
  protocols and values where no open data exists, with a simulated RPE.
* `demo_data` gains a simulated RPE column, by the same rule.

## Documentation for teaching

* New articles: "Best practices for a step test" (a checklist with sources for
  preparing the athlete, the protocol, sampling, the analyzer and reporting),
  "Checking the analysis" (checks, regression diagnostics explained panel by
  panel, confidence intervals), "Training zones", "Training load", "Comparing
  tests" and "The example data".
* "Get started" opens with a table of what you may want to do, the function
  that does it, and the article to read.
* Every citation in the articles links to its paper, and every reference to its
  DOI. "Best practices" opens with the test in five steps and a sketch of what
  moves the lactate curve, and gives each section as a table with the strength
  of its evidence. The examples use dplyr, so they read like everyday R.
* "The methods and the evidence" and "Training zones" give the studies that
  tested thresholds against the measured maximal lactate steady state, why one
  lactate concentration cannot mark it for everyone, and the debate over where
  the heavy and severe domains meet; "Training zones" says why zones from
  thresholds beat percentages of a maximum.

## Beyond the step test

* New `vlamax()` estimates the maximal lactate accumulation rate from an all-out
  sprint test: (peak post-sprint lactate - pre-sprint lactate) / (sprint duration
  - alactic time), with the alactic time fixed at 3.5 s, the most reliable option
  in Meixner et al. (2024). It also returns vLapeak, the same rise divided by the
  whole sprint (Wackerhage et al., 2025).
* New `lactate_mlss()`: the maximal lactate steady state from constant-load
  bouts, the highest one where lactate rose by no more than 1 mmol/L from
  minute 10 to 30 (Beneke, 2003), and how far each threshold of a step test
  lies from it.
* New `sample` argument (`"blood"`, `"plasma"`, `"sweat"`,
  `"interstitial fluid"`), used by `lactate_checks()`, and `stage_duration`,
  `site` and `grade` to record the protocol, which `predict_performance()`,
  `reference_percentiles()` and `compare_tests()` check against theirs.
  `read_recording()` returns them from the recording form.

## Leaner

* Eleven dependencies were dropped: broom, dplyr, forcats, ggtext, lubridate,
  magrittr, minpack.lm, pracma, segmented, stringr and tidyr.
* A new README and website: a "Get started" vignette and articles on the methods
  and the evidence behind them, on zones, uncertainty and checks, on comparing
  tests and training load, on VLamax, and on what is new.

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
