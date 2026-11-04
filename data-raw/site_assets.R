## Writes the files the website offers for download, into pkgdown/assets (copied to the root of the site):
## the recording sheet as PDF, the recording form, an example test report and an example report of a series; and the
## first page of the example report as an image for the README. Run from the package root after changing any of them,
## before knitting the README:
##
##   Rscript data-raw/site_assets.R
##
## Needs Google Chrome or Chromium, and the pagedown, pdftools and png packages.

pkgload::load_all(quiet = TRUE)
if(isTRUE(suppressMessages(systemfonts::require_font("IBM Plex Sans", fallback = "sans")))) {
  options(lactater.font = "IBM Plex Sans")
}

pagedown::chrome_print("inst/sheets/step-test-sheet.html", output = "pkgdown/assets/step-test-sheet.pdf", verbose = 0)

## the recording form, with the analytics of the rest of the site (the copy in the package has none)
form <- readLines("inst/sheets/step-test-form.html")
analytics <- yaml::read_yaml("_pkgdown.yml")$template$includes$in_header
writeLines(sub("<!-- analytics -->", analytics, form, fixed = TRUE), "pkgdown/assets/step-test-form.html")

## the default report, built step by step so the example shows a lab name and a logo
results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE)
report_start(results, lt1 = "LTP1", lt2 = "ModDmax") |>
  report_header(
    name = "Demo athlete", date = "3 November 2026", sex = "male", age = 31, body_mass = 72,
    tester = "F. Mattioni Maturana", lab = "Exercise physiology lab (example)", logo = "man/figures/logo.png"
  ) |>
  report_summary() |>
  report_zones() |>
  report_page_break() |>
  report_prediction() |>
  report_checks() |>
  report_test(protocol = "Cycle ergometer, 3-min stages of +25 W from 50 W, earlobe capillary blood (demo_data; RPE
  simulated)") |>
  report_methods() |>
  report_save("pkgdown/assets/report-example.html", open = FALSE)

## its first page as an image for the README gallery (needs pdftools and png)
page <- tempfile(fileext = ".pdf")
pagedown::chrome_print("pkgdown/assets/report-example.html", output = page, verbose = 0)
png::writePNG(pdftools::pdf_render_page(page, 1, dpi = 110), "man/figures/readme/report-page.png")

## five tests of one cyclist; the test dates are not in the data, so they start on an example date
series <- lactate_tests[lactate_tests$athlete == "cyclist_series", ]
days <- c(0, 163, 1107, 1865, 2046)
tests <- lapply(days, function(d) {
  lactate_threshold(series[series$day == d, ], intensity, lactate, heart_rate, plot = FALSE)
})
test_report(
  tests, "pkgdown/assets/report-series-example.html", dates = as.Date("2019-01-14") + days,
  athlete = list(name = "Trained cyclist (example)", sex = "male"), lt1 = "Bsln + 0.5", lt2 = "ModDmax", open = FALSE
)
