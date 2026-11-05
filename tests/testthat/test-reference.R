demo <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
## the reference values were computed with the resting sample in the fit
obla3 <- attr(lactate_threshold(demo_data, intensity, lactate, include_baseline = TRUE, plot = FALSE),
              "lactater")$intensity[["OBLA 3.0"]]
scaled <- function(by) {
  lactate_threshold(transform(demo_data, intensity = intensity * by), intensity, lactate, heart_rate, plot = FALSE)
}

test_that("the reference data are the published percentiles", {
  ## Table 2 of Fiedler et al. (2025): the median power at 3 mmol/L of men aged 14-18 is 129.43 W
  medians <- reference_power[reference_power$method == "OBLA 3.0" & reference_power$unit == "W" &
                               reference_power$sex == "male" & reference_power$percentile == 50, ]
  expect_equal(mean(medians$value[medians$age %in% 14:18]), 129.43, tolerance = 0.01)
  expect_setequal(unique(reference_power$age), 14:64)
})

test_that("a power at the median of its age and sex is on the 50th percentile", {
  median_31 <- reference_power$value[reference_power$method == "OBLA 3.0" & reference_power$unit == "W" &
                                       reference_power$sex == "male" & reference_power$age == 31 &
                                       reference_power$percentile == 50]
  ## the power at 3 mmol/L scales with the intensities, so a scaled demo test hits the median (steps stay near 25 W)
  at_median <- scaled(median_31 / obla3)
  out <- reference_percentiles(at_median, age = 31, sex = "male")
  expect_s3_class(out, "lactate_reference")
  expect_equal(out$percentile[out$method == "OBLA 3.0"], 50)
  ## at the median exactly, the value starts the interval up to the 75th
  expect_equal(reference_percentile(median_31, "OBLA 3.0", "W", "male", 31)$standing, "upper half")
  ## one year older, the same power sits a little higher
  expect_gte(reference_percentiles(at_median, age = 50, sex = "M")$percentile[1], 50)
  expect_output(print(out), "men aged 31 in Germany")
})

test_that("power per kg is placed too, and values beyond the published percentiles get a standing only", {
  out <- reference_percentiles(demo, age = 31, sex = "female", body_mass = 72)
  expect_equal(out$unit, c("W", "W", "W/kg", "W/kg"))
  expect_equal(out$value[3], round(obla3 / 72, 2))
  expect_true(all(out$percentile >= 2.5 & out$percentile <= 97.5))
  strong <- suppressWarnings(reference_percentiles(scaled(2), age = 31, sex = "female"))
  expect_true(all(is.na(strong$percentile)))
  expect_equal(unique(strong$standing), "top 2.5%")
})

test_that("the protocol is checked and wrong input stops", {
  expect_warning(reference_percentiles(scaled(2), age = 31, sex = "male"), "steps of this test")
  ## the thresholds are computed the reference way, whatever the test was analyzed with
  expected <- reference_percentiles(demo, 31, "male")
  spline <- lactate_threshold(demo_data, intensity, lactate, fit = "B-spline", include_baseline = TRUE, plot = FALSE)
  expect_equal(reference_percentiles(spline, 31, "male"), expected)
  dmax_only <- lactate_threshold(demo_data, intensity, lactate, method = "Dmax", plot = FALSE)
  expect_equal(reference_percentiles(dmax_only, 31, "male"), expected)
  expect_warning(reference_percentiles(demo, 31, "male"), NA)
  no_rest <- lactate_threshold(demo_data[-1, ], intensity, lactate, plot = FALSE)
  expect_warning(reference_percentiles(no_rest, 31, "male"), "no resting sample")
  runner <- lactate_threshold(lactate_tests[lactate_tests$test == "running", ], intensity, lactate, sport = "running",
                              plot = FALSE)
  expect_error(reference_percentiles(runner, age = 30, sex = "male"), "cycle ergometry")
  expect_error(reference_percentiles(demo, age = 70, sex = "male"), "14 to 64")
  expect_error(reference_percentiles(demo, age = 30, sex = "x"), "female")
  expect_error(reference_percentiles(demo, age = 30, sex = "male", body_mass = -1), "body_mass")
})

test_that("a report can compare the athlete with the population", {
  skip_if_not_installed("gt")
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  report_start(demo) |>
    report_header(name = "Ana", sex = "female", age = 27, body_mass = 61) |>
    report_reference() |>
    report_save(file, open = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Compared with the population", fixed = TRUE)
  expect_match(html, "Percentiles of women aged 27 in Germany", fixed = TRUE)
  expect_match(html, "W/kg", fixed = TRUE)
  expect_error(report_save(report_reference(report_start(demo)), tempfile(fileext = ".html"), open = FALSE), "age")
})
