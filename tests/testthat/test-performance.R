runner <- data.frame(intensity = c(0, 8, 10, 12, 14, 16, 18), lactate = c(1.0, 1.1, 1.2, 1.6, 2.6, 4.6, 8.0))
running <- lactate_threshold(runner, intensity, lactate, sport = "running", plot = FALSE)

test_that("the running equations reproduce their sources", {
  ## Sjödin & Jacobs (1981): their mean v4, 4.2 m/s, gives their mean marathon speed, 3.7 m/s
  sjodin <- running_equations[running_equations$method == "OBLA 4.0", ]
  expect_equal((sjodin$intercept + sjodin$slope * 4.2 * 3.6) / 3.6, 3.68, tolerance = 0.01)
  ## Roecker et al. (1998), 10 km: 0.20 + 1.05 x IAT (m/s); the error from Table 2 (SD of race time) and Table 3 (r)
  ten <- running_equations[running_equations$event == "10 km", ]
  expect_equal((ten$intercept + ten$slope * 4 * 3.6) / 3.6, 0.20 + 1.05 * 4)
  expect_equal(ten$sigma, 3.6 * 10000 * 343.4 / 2280.3^2 * sqrt(1 - 0.92^2) * sqrt(224 / 223))
})

test_that("predict_performance gives race times with their 95% range", {
  prediction <- predict_performance(running)
  iat <- attr(running, "lactater")$intensity[["IAT"]]
  ten <- prediction[prediction$event == "10 km", ]
  speed <- 3.6 * 0.20 + 1.05 * iat
  expect_equal(ten$intensity, round(speed, 1))
  expect_equal(ten$time, format_time(10000 / (speed / 3.6)))
  expect_true(ten$intensity_low < ten$intensity && ten$intensity < ten$intensity_high)
  expect_equal(sort(unique(prediction$method)), c("IAT", "OBLA 4.0"))
  expect_match(pillar::tbl_sum(prediction)[["Roecker et al. 1998"]], "2% grade")
  expect_s3_class(autoplot(prediction), "ggplot")
  expect_equal(format_time(c(59.6, 3600, 11172.8)), c("1:00", "1:00:00", "3:06:13"))
})

test_that("predict_performance warns about a different protocol and extrapolation", {
  ## the TCD runner: +1 km/h steps after 16 km/h, and an IAT above the range of Roecker et al.
  tcd <- lactate_threshold(lactate_tests[lactate_tests$test == "running", ], intensity, lactate, sport = "running",
                           plot = FALSE)
  warnings <- character()
  withCallingHandlers(predict_performance(tcd, method = "IAT"), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_match(warnings, "steps of this test", all = FALSE)
  expect_match(warnings, "outside the range", all = FALSE)
  expect_error(predict_performance(running, method = "LTP2"), "no running equation for LTP2")
  rowing <- lactate_threshold(lactate_tests[lactate_tests$test == "rowing", ], intensity, lactate, sport = "rowing",
                              plot = FALSE)
  expect_error(predict_performance(rowing), "calibrate your own")
})

test_that("the cycling equations widen away from the mean threshold", {
  cyclist <- lactate_threshold(lactate_tests[lactate_tests$test == "cycling_5min_no_rest", ], intensity, lactate,
                               plot = FALSE)
  prediction <- expect_silent(predict_performance(cyclist, method = "LTP2"))
  ltp2 <- cycling_equations[cycling_equations$method == "LTP2", ]
  threshold <- attr(cyclist, "lactater")$intensity[["LTP2"]]
  expect_equal(prediction$intensity, round(ltp2$intercept + ltp2$slope * threshold))
  expect_false("time" %in% names(prediction))
  demo <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  expect_warning(predict_performance(demo, method = "LTP2"), "extrapolation")
  far <- suppressWarnings(predict_performance(demo, method = "LTP2"))
  expect_gt(far$intensity_high - far$intensity_low, prediction$intensity_high - prediction$intensity_low)
})

test_that("calibrate_performance fits your own equation and predict_performance uses it", {
  set.seed(1)
  lab <- data.frame(ltp2 = round(stats::rnorm(30, 15, 1.5), 1))
  lab$time <- 10000 / ((0.5 + 1.02 * lab$ltp2 + stats::rnorm(30, 0, 0.3)) / 3.6)
  own <- calibrate_performance(lab, "ltp2", "time", method = "LTP2", sport = "running", distance = 10000)
  fit <- stats::lm(I(36000 / time) ~ ltp2, lab)
  expect_equal(c(own$intercept, own$slope), unname(stats::coef(fit)))
  expect_equal(own$event, "10 km")
  expect_lt(own$loo_error, 0.5)
  prediction <- predict_performance(running, equation = own)
  expect_equal(prediction$method, "LTP2")
  expect_equal(prediction$source, "Your calibration")
  expect_s3_class(autoplot(own), "ggplot")
  expect_match(pillar::tbl_sum(own)[["Fit"]], "n = 30")
  ## rowing times go through the Concept2 power relation and back
  expect_equal(race_time(race_intensity(400, 2000, "rowing"), 2000, "rowing"), 400)
  expect_error(calibrate_performance(lab, "ltp2", "time", "LTP2", sport = "cycling", distance = 40000), "mean power")
  expect_error(calibrate_performance(lab[1:3, ], "ltp2", "time", "LTP2", distance = 10000), "four athletes")
  expect_error(calibrate_performance(transform(lab, time = 0), "ltp2", "time", "LTP2", distance = 10000), "positive")
  cycling <- calibrate_performance(data.frame(t = 1:5 * 10 + 200, p = 1:5 * 9 + 180), "t", "p", "LTP2", "cycling")
  expect_error(predict_performance(running, equation = cycling), "another sport")
})
