# Report internals

`check_report()` stops when `report` is not one; `add_section()` adds a
section that is drawn when the report is saved, so the header can come
in any order; `html_*()` draw the parts. For internal use only.

## Usage

``` r
prepare_series(tests, dates)

check_report(report)

add_section(report, part, draw)

html_page(report)

html_header(report)

html_method(zones, boundary)

html_lede(r)

html_stats(r)

html_figure(zones)

html_plot(plot, caption, height = 3.5)

html_progress(r, cv)

html_reference(r)

html_zones(zones, sport)

html_prediction(r)

html_checks(x)

html_stages(info)

html_methods(x, info)
```

## Arguments

- tests:

  A list of results of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- dates:

  Their dates, or days since the first test.

- report, r:

  A report from
  [`report_start()`](https://fmmattioni.github.io/lactater/reference/report_start.md).

- part:

  The name of the section, for printing.

- draw:

  A function of the report that returns the section's HTML.

- zones:

  The result of
  [`lactate_zones()`](https://fmmattioni.github.io/lactater/reference/lactate_zones.md).

- boundary:

  1 for LT1, 2 for LT2.

- plot:

  A ggplot or a patchwork, drawn without its title and subtitle.

- caption:

  The text under it.

- height:

  Its height, in inches; it is 7 inches wide.

- sport:

  The sport.

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- info:

  The `lactater` attribute of the results.
