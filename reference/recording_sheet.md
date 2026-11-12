# A sheet to record a step test

Copies a printable recording sheet, a web form and a CSV template into a
folder, so every test is recorded the same way and goes straight into
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).
The sheet is an A4 web page (open it in a browser and print it) with the
athlete, the 24 to 48 h before the test, the protocol, one row per stage
for intensity, lactate, heart rate and RPE, and how the test ended, as
[Best practices for a step
test](https://fmmattioni.github.io/lactater/articles/best-practices.html)
suggests. The form has the same fields to fill in during the test, saves
itself in the browser, and downloads an `.xlsx` that
[`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
reads, or a CSV of the stages; it works offline and is also [on the
website](https://fmmattioni.github.io/lactater/step-test-form.html). The
template has the columns lactater reads, with the resting sample as the
first row.

## Usage

``` r
recording_sheet(dir = ".", open = interactive())
```

## Arguments

- dir:

  The folder to copy them into.

- open:

  Whether to open the form in the browser.

## Value

the paths of the sheet, the form and the template, invisibly.

## See also

[`read_recording()`](https://fmmattioni.github.io/lactater/reference/read_recording.md)
to read a test recorded with the form.

## Examples

``` r
files <- recording_sheet(tempfile(), open = FALSE)
read.csv(files[["template"]])
#>   intensity lactate heart_rate rpe
#> 1         0      NA         NA  NA
```
