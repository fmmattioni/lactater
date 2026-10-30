test_that("fit_breakpoints recovers exact breakpoints", {
  x <- seq(50, 300, by = 25)
  y <- 1 + 0.002 * x + 0.03 * pmax(x - 140, 0) + 0.08 * pmax(x - 230, 0)
  fit <- fit_breakpoints(x, y)
  expect_equal(fit$psi, c(140, 230), tolerance = 1e-4)
  expect_equal(predict_breakpoints(fit, x), y, tolerance = 1e-8)
})

test_that("fit_two_lines takes the lowest-RSS division and returns where the lines cross", {
  # the corner sits on a measured point (x = 175), which then belongs to both lines
  x <- seq(50, 300, by = 25)
  y <- ifelse(x <= 175, 1 + 0.002 * x, 1.35 + 0.05 * (x - 175))
  fit <- fit_two_lines(x, y)
  expect_equal(fit$psi, 175, tolerance = 1e-8)
  expect_equal(predict_breakpoints(fit, x), y, tolerance = 1e-8)
  # the division point belongs to both lines, so 3 points per line need 5 points
  expect_error(fit_two_lines(1:4, c(1, 1, 2, 4)), "At least 5 measured points")
  # nearly straight data: the two lines cross outside the stages
  expect_true(is.na(fit_two_lines(1:5, c(1, 2.1, 3.1, 3.8, 5.1))$psi))
})

test_that("flat_first fixes the first slope at zero", {
  x <- seq(50, 300, by = 25)
  y <- 1.2 + 0.02 * pmax(x - 120, 0) + 0.06 * pmax(x - 240, 0)
  fit <- fit_breakpoints(x, y, flat_first = TRUE)
  expect_equal(fit$psi, c(120, 240), tolerance = 1e-4)
  expect_length(fit$coefficients, 3)
})

test_that("fit_breakpoints needs two points per segment", {
  expect_error(fit_breakpoints(1:5, (1:5)^2), "At least 6 measured points")
})

test_that("ltp_fix_first_slope reaches the LTP fit", {
  free <- lactate_threshold(demo_data, "intensity", "lactate", method = "LTP", plot = FALSE)
  flat <- lactate_threshold(demo_data, "intensity", "lactate", method = "LTP", plot = FALSE, ltp_fix_first_slope = TRUE)
  expect_false(isTRUE(all.equal(free$intensity, flat$intensity)))
})
