test_that("lactate_report builds a gt table with notes for flagged checks", {
  skip_if_not_installed("gt")
  results <- lactate_threshold(fixtures$plateau_x3, "intensity", "lactate", sample = "sweat", plot = FALSE)
  report <- lactate_report(results)
  expect_s3_class(report, "gt_tbl")
  notes <- unlist(report[["_source_notes"]])
  expect_true(any(grepl("Sample type", notes)))
  body <- gt::extract_body(report)
  expect_equal(nrow(body), nrow(results))
  expect_false(any(body$lactate == "NA"))
})

test_that("test_report writes one self-contained page with every section", {
  skip_if_not_installed("gt")
  results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE)
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  out <- test_report(
    results, file, athlete = list(name = "Ana <test>", body_mass = 72), lt1 = "LTP1", lt2 = "ModDmax", open = FALSE
  )
  expect_equal(out, file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  sections <- c("Five training zones", "Predicted performance", "How to read it", "What to double-check", "The test",
                "Every method")
  for(section in sections) {
    expect_match(html, section, fixed = TRUE)
  }
  ## one document with its own styles
  expect_equal(lengths(regmatches(html, gregexpr("<html", html))), 1)
  expect_match(html, "<title>Lactate step test, Ana &lt;test&gt;</title>", fixed = TRUE)
  expect_match(html, "the report of test_report()", fixed = TRUE)
  ## the title states the two thresholds
  zones <- attr(lactate_zones(results, lt1 = "LTP1", lt2 = "ModDmax"), "lactater_zones")$lt
  expect_match(html, sprintf("<h2 class=\"claim\">LT1 at %.0f W, LT2 at %.0f W</h2>", zones[1], zones[2]), fixed = TRUE)
  expect_match(html, "<h1>Ana &lt;test&gt;</h1>", fixed = TRUE)
  ## the family of the Dmax methods carries its subscript in the table of every method
  expect_false(grepl(">[[:space:]]*Dmax[[:space:]]*<", html))
  expect_match(html, "D<sub>max</sub>", fixed = TRUE)
  ## what the user types is escaped, the figure is embedded, and LT1 comes per kg
  expect_match(html, "Ana &lt;test&gt;", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  lt1 <- attr(lactate_zones(results, lt1 = "LTP1", lt2 = "ModDmax"), "lactater_zones")$lt[1]
  expect_match(html, sprintf("%.1f W/kg", lt1 / 72), fixed = TRUE)
  ## without a published equation for the sport, the report says how to get one
  rowing <- lactate_threshold(lactate_tests[lactate_tests$test == "rowing", ], intensity, lactate, sport = "rowing",
                              plot = FALSE)
  test_report(rowing, file, open = FALSE)
  expect_match(paste(readLines(file, warn = FALSE), collapse = "\n"), "calibrate_performance", fixed = TRUE)
})

test_that("a report can be built step by step, with the sections in any order", {
  skip_if_not_installed("gt")
  results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
  logo <- tempfile(fileext = ".png")
  grDevices::png(logo, 120, 120)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  report <- report_start(results, lt1 = "LTP1", lt2 = "ModDmax") |>
    report_zones(model = 3, chart = FALSE) |>
    report_text("Training plan", c("Two sessions in zone 2.", "One in zone 3.")) |>
    report_header(name = "Ana", club = "Tuebingen RC", body_mass = 61, lab = "Our lab", logo = logo)
  expect_s3_class(report, "lactate_test_report")
  expect_output(print(report), "zones, text")
  file <- report_save(report, tempfile(fileext = ".html"), open = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  ## the header added last still sits on top, with every detail, the lab and the logo
  expect_match(html, "Tuebingen RC", fixed = TRUE)
  expect_match(html, "Our lab", fixed = TRUE)
  expect_lt(regexpr("Ana", html), regexpr("Three training zones", html))
  expect_equal(lengths(regmatches(html, gregexpr("data:image/png;base64", html))), 1)
  expect_match(html, "One in zone 3.", fixed = TRUE)
  expect_false(grepl("Predicted performance", html, fixed = TRUE))
  expect_error(report_zones(results), "report_start")
  expect_error(report_zones(report, model = 4), "3 or 5")
  expect_error(report_header(report, logo = "nope.png"), "Cannot find the logo")
})
