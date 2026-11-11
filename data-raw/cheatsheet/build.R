## Builds the two-page cheatsheet: renders its plots from demo_data, then prints cheatsheet.html to PDF and writes its
## web version (pkgdown/assets/cheatsheet.html, the Cheatsheet of the site menu).
## Run from the package root (needs Chrome and the pagedown package):
##
##   Rscript data-raw/cheatsheet/build.R

pkgload::load_all(quiet = TRUE)
if(isTRUE(suppressMessages(systemfonts::require_font("IBM Plex Sans", fallback = "sans")))) {
  options(lactater.font = "IBM Plex Sans")
}
dir <- "data-raw/cheatsheet"
save <- function(plot, name, width, height) {
  ggplot2::ggsave(file.path(dir, name), plot, width = width, height = height, dpi = 300, bg = "white")
}

results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE)
save(autoplot(results, method = "ModDmax") + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL),
     "method.png", 6, 3.2)
save(leave_one_out_panel(lactate_diagnostics(results), lactater_colours(), "cycling"), "grid.png", 6, 3.6)
zones <- lactate_zones(results, model = 5)
## the curve over the heart rate panel: the titles sit on the whole patchwork
save(autoplot(zones) + patchwork::plot_annotation(title = NULL, subtitle = NULL), "zones.png", 6, 4.4)
series <- lactate_tests[lactate_tests$athlete == "cyclist_series", ]
before <- lactate_threshold(series[series$day == 0, ], intensity, lactate, plot = FALSE)
after <- lactate_threshold(series[series$day == 58, ], intensity, lactate, plot = FALSE)
save(autoplot(compare_tests(before, after)) + ggplot2::labs(title = NULL, subtitle = NULL), "compare.png", 6, 3)

## the first page of the example report (data-raw/site_assets.R writes it)
report <- tempfile(fileext = ".pdf")
pagedown::chrome_print("pkgdown/assets/report-example.html", output = report, verbose = 0)
png::writePNG(pdftools::pdf_render_page(report, page = 1, dpi = 110), file.path(dir, "report.png"))

pagedown::chrome_print(
  file.path(dir, "cheatsheet.html"), output = "pkgdown/assets/lactater-cheatsheet.pdf", verbose = 0
)

## the web version beside it: the same page with the analytics of the rest of the site and the plots inside it
page <- readLines(file.path(dir, "cheatsheet.html"), encoding = "UTF-8")
analytics <- yaml::read_yaml("_pkgdown.yml")$template$includes$in_header
page <- sub("<!-- analytics -->", analytics, page, fixed = TRUE)
for(image in list.files(dir, pattern = "\\.png$")) {
  page <- gsub(sprintf('src="%s"', image), sprintf('src="%s"', xfun::base64_uri(file.path(dir, image))), page,
               fixed = TRUE)
}
writeLines(page, "pkgdown/assets/cheatsheet.html", useBytes = TRUE)
