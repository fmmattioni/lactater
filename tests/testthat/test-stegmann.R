recovery <- data.frame(time = c(1, 3, 5, 7, 10), lactate = c(9.3, 9.6, 9.0, 8.2, 7.1))

test_that("Stegmann draws the tangent from where recovery lactate is back at its end value", {
  results <- lactate_threshold(demo_data, intensity, lactate, recovery = recovery, stage_duration = 3, plot = FALSE)
  info <- attr(results, "lactater")
  tangent <- stegmann_tangent(info$data, info$curve, info$settings)
  ## 8.64 mmol/L at the end, crossed between 9.0 (5 min) and 8.2 (7 min)
  expect_equal(tangent$back, 5 + (9.0 - 8.64) / (9.0 - 8.2) * 2)
  ## 25 W per 3-min stage puts that time at 191 + 5.9 x 25 / 3 W
  expect_equal(tangent$from[["intensity"]], 191 + tangent$back * 25 / 3)
  ## at the contact point the line has the slope of the curve
  h <- 1e-4
  slope_curve <- (info$curve$predict(tangent$intensity + h) - info$curve$predict(tangent$intensity - h)) / (2 * h)
  slope_line <- (8.64 - info$curve$predict(tangent$intensity)) / (tangent$from[["intensity"]] - tangent$intensity)
  expect_equal(slope_curve, slope_line, tolerance = 1e-4)
  expect_equal(info$intensity[["Stegmann"]], tangent$intensity)
  expect_s3_class(autoplot(results, method = "Stegmann"), "ggplot")
})

test_that("Stegmann needs recovery samples that fall back to the end value", {
  expect_false("Stegmann" %in% lactate_threshold(demo_data, intensity, lactate, plot = FALSE)$method)
  expect_error(lactate_threshold(demo_data, intensity, lactate, method = "Stegmann", plot = FALSE), "needs `recovery`")
  expect_error(lactate_threshold(demo_data, intensity, lactate, recovery = recovery, plot = FALSE), "stage_duration")
  expect_error(
    lactate_threshold(demo_data, intensity, lactate, recovery = recovery["time"], stage_duration = 3, plot = FALSE),
    "columns `time`"
  )
  ## sampled too briefly: lactate never falls back below 8.64 mmol/L
  short <- lactate_threshold(demo_data, intensity, lactate, method = "Stegmann", recovery = recovery[1:3, ],
                             stage_duration = 3, plot = FALSE)
  expect_true(is.na(short$intensity))
  expect_error(
    lactate_threshold(lactate_tests, intensity, lactate, recovery = recovery, stage_duration = 3, .by = "test"),
    "one test at a time"
  )
  expect_error(
    lactate_threshold(demo_data, intensity, lactate, recovery = recovery, stage_duration = 3, max_intensity = 175),
    "cannot leave out its last stages"
  )
})
