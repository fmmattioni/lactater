## Writes the files the website offers for download, into pkgdown/assets (copied to the root of the site):
## the recording sheet as PDF and an example test report. Run from the package root after changing either:
##
##   Rscript data-raw/site_assets.R
##
## Needs Google Chrome or Chromium, and the pagedown package for the PDF.

pkgload::load_all(quiet = TRUE)
if(isTRUE(suppressMessages(systemfonts::require_font("IBM Plex Sans", fallback = "sans")))) {
  options(lactater.font = "IBM Plex Sans")
}

pagedown::chrome_print("inst/sheets/step-test-sheet.html", output = "pkgdown/assets/step-test-sheet.pdf", verbose = 0)

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
