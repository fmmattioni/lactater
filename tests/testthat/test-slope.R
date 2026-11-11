test_that("Keul and Simon sit where the polynomial rises by 1.26 and 1 mmol/L per km/h", {
  running <- lactate_tests[lactate_tests$test == "running_recreational", ]
  results <- lactate_threshold(running, intensity, lactate, sport = "running", plot = FALSE)
  intensity <- attr(results, "lactater")$intensity
  stages <- running[running$intensity > 0, ]
  k <- unname(stats::lm(lactate ~ poly(intensity, 3, raw = TRUE), stages)$coefficients)
  slope <- function(x) k[2] + 2 * k[3] * x + 3 * k[4] * x^2
  expect_equal(slope(intensity[["Keul"]]), 1.26, tolerance = 1e-6)
  expect_equal(slope(intensity[["Simon"]]), 1, tolerance = 1e-6)
  expect_lt(intensity[["Simon"]], intensity[["Keul"]])
})

test_that("Keul and Simon run on running tests only", {
  results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  expect_false(any(c("Keul", "Simon") %in% results$method))
  expect_error(
    lactate_threshold(demo_data, intensity, lactate, method = c("OBLA", "Simon"), plot = FALSE),
    "`Simon` is defined on running speed in km/h"
  )
})

test_that("Keul and Simon are NA when the curve never gets that steep", {
  flat <- data.frame(intensity = c(8, 10, 12, 14, 16, 18), lactate = c(1, 1.1, 1.3, 1.6, 2, 2.5))
  results <- lactate_threshold(flat, intensity, lactate, method = c("Keul", "Simon"), sport = "running", plot = FALSE)
  expect_true(all(is.na(results$intensity)))
})

test_that("compare_tests says the default noise comes from cyclists, and Keul and Simon have none", {
  running <- lactate_tests[lactate_tests$test == "running_recreational", ]
  before <- lactate_threshold(running, intensity, lactate, sport = "running", plot = FALSE)
  comparison <- compare_tests(before, before)
  expect_match(attr(comparison, "notes"), "step tests of cyclists", all = FALSE)
  expect_true(all(is.na(comparison$verdict[comparison$method %in% c("Keul", "Simon")])))
  expect_null(attr(compare_tests(before, before, cv = 3), "notes"))
})
