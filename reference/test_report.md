# A report of a step test, as a web page

Writes the default report of a step test in one call: the header with
the athlete's details, LT1 and LT2 with what they mean, the training
zones, the predicted performance, what to double-check, the test, and
every method. Open it in a browser, or print it to PDF from there, or
set `pdf = TRUE`: two A4 pages, the answer and the zones on the first,
the record on the second (a section moves to the next page whole). To
choose the sections, their order, or to add your lab's name and logo,
build the report step by step with
[`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md).
Needs the gt package.

## Usage

``` r
test_report(
  x,
  file = "lactate-test-report.html",
  athlete = list(),
  lt1 = NULL,
  lt2 = NULL,
  protocol = NULL,
  notes = NULL,
  pdf = FALSE,
  open = interactive(),
  dates = NULL
)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md),
  or a list of two to five of them with their `dates`.

- file:

  Where to write the report, an `.html` file.

- athlete:

  A named list for the header, passed to
  [`report_header()`](https://fmmattioni.github.io/lactater/reference/report_start.md),
  e.g.
  `list(name = "Ana Silva", date = "3 November 2026", sex = "female", age = 27, body_mass = 61)`.

- lt1, lt2:

  The thresholds that mark LT1 and LT2, as in
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md).

- protocol:

  One line on how the test was done: ergometer, stages, sampling site,
  analyzer. Not shown for a series.

- notes:

  Free text for the end of the report.

- pdf:

  Whether to print a PDF next to the HTML file as well. Needs the
  pagedown package and Google Chrome or Chromium.

- open:

  Whether to open the report in the browser.

- dates:

  With a list of two to five tests in `x`, the date of each test (dates,
  or days since the first test). The report then starts with the
  progress over the tests, and describes the latest one.

## Value

the path of the report, invisibly.

## See also

[`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md)
to build a report step by step.

## Examples

``` r
results <- lactate_threshold(
  demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
)
report <- test_report(
  results,
  file = tempfile(fileext = ".html"),
  athlete = list(name = "Demo athlete", date = "3 November 2026", body_mass = 72),
  protocol = "Cycle ergometer, 3-min stages of +25 W from 50 W, earlobe capillary blood",
  open = FALSE
)
```
