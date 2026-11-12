# Package index

## Analyze a test

Start here.
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
takes the stages of a step test (intensity, lactate and, optionally,
heart rate) and estimates every threshold method in one call. It returns
a tibble that remembers the data and the fitted curve, so it can be
plotted, checked and turned into zones afterwards.

- [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  : Lactate thresholds from a step test
- [`demo_data`](https://fmmattioni.github.io/lactater/reference/demo_data.md)
  : Demo data
- [`lactate_tests`](https://fmmattioni.github.io/lactater/reference/lactate_tests.md)
  : Lactate step tests from different sports, athletes and scenarios
- [`lactate_curve()`](https://fmmattioni.github.io/lactater/reference/lactate_curve.md)
  : Lactate curve

## Look at the results

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws an overview of every threshold on one intensity axis, or a single
method in detail.
[`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md)
writes a report of the test as one web page (and PDF) for the athlete or
the coach;
[`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
builds one step by step.
[`lactate_report()`](https://fmmattioni.github.io/lactater/reference/lactate_report.md)
formats the results as a table for papers.
[`theme_lactater()`](https://fmmattioni.github.io/lactater/reference/theme_lactater.md)
gives your own plots the same look.

- [`autoplot(`*`<lactate_thresholds>`*`)`](https://fmmattioni.github.io/lactater/reference/autoplot.lactate_thresholds.md)
  [`plot(`*`<lactate_thresholds>`*`)`](https://fmmattioni.github.io/lactater/reference/autoplot.lactate_thresholds.md)
  : Plot lactate thresholds
- [`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md)
  : A report of a step test, as a web page
- [`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_header()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_summary()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_zones()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_progress()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_reference()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_prediction()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_checks()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_test()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_methods()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_text()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_page_break()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  [`report_save()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
  : Build a test report step by step
- [`lactate_report()`](https://fmmattioni.github.io/lactater/reference/lactate_report.md)
  : Lactate threshold report table
- [`gt_lactater()`](https://fmmattioni.github.io/lactater/reference/gt_lactater.md)
  : The look of lactater's tables
- [`plot_methods()`](https://fmmattioni.github.io/lactater/reference/plot_methods.md)
  : Combine lactate threshold methods into one plot
- [`theme_lactater()`](https://fmmattioni.github.io/lactater/reference/theme_lactater.md)
  : lactater ggplot2 theme

## Check the analysis

A threshold is only as good as the test behind it.
[`lactate_checks()`](https://fmmattioni.github.io/lactater/reference/lactate_checks.md)
flags what is easy to miss in a table of numbers,
[`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md)
shows how well each curve follows and predicts the measured lactate, and
[`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md)
plots the regression checks and how much each threshold rests on single
stages.

- [`lactate_checks()`](https://fmmattioni.github.io/lactater/reference/lactate_checks.md)
  : Check a lactate threshold analysis
- [`lactate_fit_quality()`](https://fmmattioni.github.io/lactater/reference/lactate_fit_quality.md)
  : Goodness of fit of the lactate curves
- [`lactate_diagnostics()`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md)
  [`autoplot(`*`<lactate_diagnostics>`*`)`](https://fmmattioni.github.io/lactater/reference/lactate_diagnostics.md)
  : Check the lactate curve and how much each threshold depends on
  single stages

## Use the results

Turn two thresholds of your choice into training zones and count the
training time in them; compare two tests of one athlete (is the change
real, how far did the curve move, did it shift or change shape), follow
a series of tests, measure the test-retest noise of your own protocol,
compare athletes per kg or % of peak or with the population of their age
and sex, and predict race performance, from published equations or one
you calibrate on your own athletes.

- [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md)
  [`autoplot(`*`<lactate_zones>`*`)`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md)
  : Training zones from two lactate thresholds
- [`training_load()`](https://fmmattioni.github.io/lactater/reference/training_load.md)
  [`autoplot(`*`<lactate_load>`*`)`](https://fmmattioni.github.io/lactater/reference/training_load.md)
  : Training load and intensity distribution from the lactate zones
- [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  [`autoplot(`*`<lactate_comparison>`*`)`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
  : Compare two lactate tests
- [`lactate_reliability()`](https://fmmattioni.github.io/lactater/reference/lactate_reliability.md)
  : Test-retest reliability of each threshold, from your own repeated
  tests
- [`curve_shift()`](https://fmmattioni.github.io/lactater/reference/curve_shift.md)
  : Shift of the whole lactate curve between two tests
- [`curve_shape()`](https://fmmattioni.github.io/lactater/reference/curve_shape.md)
  [`autoplot(`*`<lactate_shape>`*`)`](https://fmmattioni.github.io/lactater/reference/curve_shape.md)
  : Did the lactate curve shift, or change its shape?
- [`predict_performance()`](https://fmmattioni.github.io/lactater/reference/predict_performance.md)
  [`autoplot(`*`<lactate_performance>`*`)`](https://fmmattioni.github.io/lactater/reference/predict_performance.md)
  : Predict performance from a step test
- [`calibrate_performance()`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md)
  [`autoplot(`*`<lactate_calibration>`*`)`](https://fmmattioni.github.io/lactater/reference/calibrate_performance.md)
  : Calibrate your own performance equation
- [`lactate_trend()`](https://fmmattioni.github.io/lactater/reference/lactate_trend.md)
  [`autoplot(`*`<lactate_trend>`*`)`](https://fmmattioni.github.io/lactater/reference/lactate_trend.md)
  : Thresholds over a series of tests
- [`relative_thresholds()`](https://fmmattioni.github.io/lactater/reference/relative_thresholds.md)
  : Thresholds relative to body mass, peak intensity and peak heart rate
- [`reference_percentiles()`](https://fmmattioni.github.io/lactater/reference/reference_percentiles.md)
  : Thresholds against the population

## Beyond the step test

The maximal lactate steady state from constant-load bouts, to check
which threshold estimates it in your athletes, and the maximal lactate
accumulation rate from a sprint test.

- [`lactate_mlss()`](https://fmmattioni.github.io/lactater/reference/lactate_mlss.md)
  [`autoplot(`*`<lactate_mlss>`*`)`](https://fmmattioni.github.io/lactater/reference/lactate_mlss.md)
  : Maximal lactate steady state from constant-load bouts
- [`vlamax()`](https://fmmattioni.github.io/lactater/reference/vlamax.md)
  : Maximal lactate accumulation rate (VLamax)

## Record a test

A web form that records a test and downloads it for lactater, with a
printable sheet and a CSV template, to record every test the same way.

- [`recording_sheet()`](https://fmmattioni.github.io/lactater/reference/recording_sheet.md)
  : A sheet to record a step test
- [`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
  : Read a recorded step test

## Re-exported functions

- [`reexports`](https://fmmattioni.github.io/lactater/reference/reexports.md)
  [`autoplot`](https://fmmattioni.github.io/lactater/reference/reexports.md)
  : Objects exported from other packages
