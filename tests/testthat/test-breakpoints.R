test_that("fit_breakpoints recovers exact breakpoints", {
  x <- seq(50, 300, by = 25)
  y <- 1 + 0.002 * x + 0.03 * pmax(x - 140, 0) + 0.08 * pmax(x - 230, 0)
  fit <- fit_breakpoints(x, y, n_breakpoints = 2)
  expect_equal(fit$psi, c(140, 230), tolerance = 1e-4)
  expect_equal(predict_breakpoints(fit, x), y, tolerance = 1e-8)

  one <- fit_breakpoints(x, 2 + 0.05 * pmax(x - 180, 0), n_breakpoints = 1)
  expect_equal(one$psi, 180, tolerance = 1e-4)
})

test_that("flat_first fixes the first slope at zero", {
  x <- seq(50, 300, by = 25)
  y <- 1.2 + 0.02 * pmax(x - 120, 0) + 0.06 * pmax(x - 240, 0)
  fit <- fit_breakpoints(x, y, n_breakpoints = 2, flat_first = TRUE)
  expect_equal(fit$psi, c(120, 240), tolerance = 1e-4)
  expect_length(fit$coefficients, 3)
})

test_that("fit_breakpoints needs two points per segment", {
  expect_error(fit_breakpoints(1:5, (1:5)^2, n_breakpoints = 2), "At least 6 measured points")
})

test_that("ltp_fix_first_slope reaches the LTP fit", {
  free <- lactate_threshold(demo_data, "intensity", "lactate", method = "LTP", plot = FALSE)
  flat <- lactate_threshold(demo_data, "intensity", "lactate", method = "LTP", plot = FALSE, ltp_fix_first_slope = TRUE)
  expect_false(isTRUE(all.equal(free$intensity, flat$intensity)))
})
