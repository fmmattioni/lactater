test_that("retrieve_intensity solves the fitted curve exactly", {
  x <- seq(50, 250, by = 25)
  curve <- fit_curve(data.frame(intensity = x, lactate = 1 + 1e-6 * (x - 50)^3), "3rd degree polynomial")

  # 1 + 1e-6 * (x - 50)^3 = 4  <=>  x = 50 + (3e6)^(1/3)
  expect_equal(retrieve_intensity(curve, 4), 50 + (3e6)^(1 / 3), tolerance = 1e-6)
  expect_true(is.na(retrieve_intensity(curve, 50)))
})

test_that("retrieve_intensity keeps the highest crossing when the curve dips", {
  x <- seq(0, 10, by = 1)
  curve <- fit_curve(data.frame(intensity = x, lactate = 2 - x + 0.15 * x^2), "3rd degree polynomial")
  # 2 - x + 0.15 x^2 = 1.5 has roots at (1 -/+ sqrt(0.7)) / 0.3
  expect_equal(retrieve_intensity(curve, 1.5), (1 + sqrt(0.7)) / 0.3, tolerance = 1e-6)
})

test_that("the exponential fit recovers known parameters without starting values", {
  x <- seq(100, 300, by = 25)
  curve <- fit_curve(data.frame(intensity = x, lactate = 0.8 + 0.02 * exp(0.02 * x)), "Exponential")
  expect_equal(unname(curve$coefficients), c(0.8, 0.02, 0.02), tolerance = 1e-6)
})

test_that("retrieve_heart_rate interpolates between stages and ignores the resting sample", {
  # 106.8 W lies between 100 W (154 bpm) and 125 W (170 bpm)
  expect_equal(retrieve_heart_rate(demo_data, c(100, 106.8, 191)), c(154, 158, 198))
  expect_true(is.na(retrieve_heart_rate(demo_data, 40)))
  expect_true(all(is.na(retrieve_heart_rate(demo_data[1:2, ], 50))))
})
