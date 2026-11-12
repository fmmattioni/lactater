# Read a recorded step test

Reads a test recorded with the web form of
[`recording_sheet()`](https://fmmattioni.github.io/lactater/reference/recording_sheet.md)
(an `.xlsx` with the sheets `stages`, `recovery` and `test`), or a CSV
in the columns of the template, and returns what
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
and
[`test_report()`](https://fmmattioni.github.io/lactater/reference/test_report.md)
need. Needs the readxl package for an `.xlsx`.

## Usage

``` r
read_recording(file)
```

## Arguments

- file:

  The `.xlsx` downloaded from the form, or a `.csv`.

## Value

a list with

- `stages`: one row per stage, the resting sample first, with
  `intensity`, `lactate`, `heart_rate` and `rpe` (and `clock_time` and
  `notes` from the form). When the form has a "time in the last stage"
  shorter than the stage duration, the intensity of the last stage is
  time-weighted, as `lactate_threshold(last_stage_duration = )` does,
  and a message says so. A last step that is already smaller than the
  one before is taken as weighted and kept;

- `recovery`: the recovery samples (`time`, in minutes, and `lactate`)
  for the Stegmann threshold, or `NULL`;

- `athlete`: `name`, `date`, `sex`, `age` and `body_mass`, as recorded,
  for `test_report(athlete = )` or
  [`report_header()`](https://fmmattioni.github.io/lactater/reference/report_start.md);

- `sport`, `sample`, `rpe_scale`, `stage_duration` (min), `site` and
  `grade` (%), for
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md):
  `"cycling"`, `"blood"` and `"6-20"` when not recorded, and `NULL` for
  the others;

- `protocol`: the protocol in one line, for `test_report(protocol = )`,
  or `NULL`;

- `details`: every field of the form, as text.

## See also

[`recording_sheet()`](https://fmmattioni.github.io/lactater/reference/recording_sheet.md)
for the form.

## Examples

``` r
## a test recorded with the CSV template
file <- tempfile(fileext = ".csv")
write.csv(demo_data[c("intensity", "lactate", "heart_rate", "rpe")], file, row.names = FALSE)
test <- read_recording(file)
results <- lactate_threshold(
  test$stages, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
)
```
