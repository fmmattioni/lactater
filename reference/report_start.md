# Build a test report step by step

Start a report with `report_start()`, then add the parts you want, in
the order you want them, and write it with `report_save()`. Printing a
report previews it in the viewer.
[`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md)
builds the default report in one call.

## Usage

``` r
report_start(x, lt1 = NULL, lt2 = NULL, dates = NULL)

report_header(report, name = NULL, date = NULL, ..., lab = NULL, logo = NULL)

report_summary(report)

report_zones(report, model = 5, chart = TRUE)

report_progress(report, cv = NULL)

report_reference(report)

report_prediction(report)

report_checks(report)

report_test(report, protocol = NULL)

report_methods(report)

report_text(report, title, text)

report_page_break(report)

report_save(
  report,
  file = "lactate-test-report.html",
  pdf = FALSE,
  open = interactive()
)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md),
  or a list of two to five of them for a series of tests of one athlete.

- lt1, lt2:

  The thresholds that mark LT1 and LT2, as in
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md).

- dates:

  For a series, the date of each test: dates, or days since the first
  test. Without them, the names of `x` are read as dates.

- report:

  A report from `report_start()`.

- name:

  The athlete's name, the heading of the report.

- date:

  The date of the test, as you want it printed.

- ...:

  Other details to list under the name, as `label = value`, e.g.
  `sex = "female"`, `age = 27`, `club = "Tuebingen RC"`. `body_mass`
  (kg, a number) also gives the thresholds per kg.

- lab:

  Your lab, institution or business, printed at the top.

- logo:

  The path to a logo image (PNG or JPEG), printed at the top right.

- model:

  `3` or `5` zones.

- chart:

  Whether to draw the zones chart above the table.

- cv:

  The typical error of the methods, as in
  [`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md);
  the defaults cover only some methods.

- protocol:

  One line on how the test was done: ergometer, stages, sampling site,
  analyzer.

- title:

  The heading of a section of your own.

- text:

  Its text: one string per paragraph.

- file:

  Where to write the report, an `.html` file.

- pdf:

  Whether to print a PDF next to the HTML file as well. Needs the
  pagedown package and Google Chrome or Chromium.

- open:

  Whether to open the report in the browser.

## Value

a `lactate_test_report` to add to, and for `report_save()` the path of
the report, invisibly.

## Details

To report a series of tests of one athlete, give `report_start()` a list
of tests and their `dates`: the sections above then describe the latest
test, and `report_progress()` shows the progress over all of them.

- `report_header()`: who was tested and where. The athlete's name, the
  date, any other details you name (`sex`, `age`, `club`, `tester`...),
  your lab or institution, and its logo.

- `report_summary()`: the answer: what LT1 and LT2 mean, and LT1, LT2
  and the peak with their lactate, heart rate, RPE, percentage of peak
  and, with `body_mass`, per kg.

- `report_progress()`: for a series, how LT1 and LT2 changed since the
  first test, every lactate curve with the zones of each test, test by
  test, and whether the latest change is larger than the noise of the
  method
  ([`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)).

- `report_zones()`: the zones chart and table, three or five zones.

- `report_reference()`: for cycling, the percentile of the power at 3
  mmol/L and at baseline + 1 mmol/L among people of the same age and sex
  in Germany
  ([`reference_percentiles()`](https://fmmattioni.github.io/lactater/reference/reference_percentiles.md)),
  from the `age`, `sex` and `body_mass` given to `report_header()`. For
  the general population, not for athletes.

- `report_prediction()`: the predicted race performance, from the chosen
  thresholds when an equation takes them.

- `report_checks()`: what to double-check in the data.

- `report_test()`: the protocol and the measured stages.

- `report_methods()`: the threshold of every method.

- `report_text()`: a section of your own, such as notes or a training
  plan.

- `report_page_break()`: start a new page when printed.

## Examples

``` r
results <- lactate_threshold(
  demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
)
report <- report_start(results) |>
  report_header(
    name = "Demo athlete", date = "3 November 2026", sex = "male", age = 31, body_mass = 72,
    lab = "Exercise physiology lab"
  ) |>
  report_summary() |>
  report_zones(model = 3) |>
  report_text("Training", "Two sessions a week in zone 2, one in zone 3.")
report_save(report, tempfile(fileext = ".html"), open = FALSE)
```
