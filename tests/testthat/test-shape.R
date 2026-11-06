curve <- function(intensity, a = 0.8, b = 0.05, c = 0.015) a + b * exp(c * intensity)
stages <- seq(100, 300, by = 25)
noise <- c(0.05, -0.04, 0.03, -0.05, 0.04, -0.03, 0.05, -0.04, 0.02)
run <- function(lactate, sport = "cycling", scale = 1) {
  data <- data.frame(intensity = c(0, stages) * scale, lactate = c(1, lactate))
  lactate_threshold(data, intensity, lactate, sport = sport, method = "OBLA", plot = FALSE)
}
before <- run(curve(stages) + noise)

test_that("curve_shape recovers a pure shift", {
  shifted <- curve_shape(before, run(curve(stages - 20) + rev(noise)))
  expect_lt(abs(shifted$shift - 20), 2)
  expect_lt(shifted$shift_low, 20)
  expect_gt(shifted$shift_high, 20)
  expect_identical(shifted$verdict, "shift")
  expect_equal(c(shifted$df1, shifted$df2), c(2, 2 * length(stages) - 6))
  same <- curve_shape(before, before)
  expect_equal(same$shift, 0, tolerance = 1e-6)
  expect_output(print(shifted), "No evidence the shape changed")
  expect_s3_class(autoplot(shifted), "ggplot")
})

test_that("curve_shape flags a change of shape", {
  flatter <- curve_shape(before, run(curve(stages, b = 0.3, c = 0.009) + rev(noise)))
  expect_identical(flatter$verdict, "shape changed")
  expect_lt(flatter$p_value, 0.05)
  expect_output(print(flatter), "The shape changed")
})

test_that("curve_shape needs two tests of the same sport, with five stages each", {
  expect_error(curve_shape(before, run(curve(stages) + noise, sport = "running", scale = 0.05)), "same sport")
  short <- lactate_threshold(
    data.frame(intensity = c(0, stages[1:4]), lactate = c(1, curve(stages[1:4]))), intensity, lactate,
    method = "OBLA", plot = FALSE
  )
  expect_error(curve_shape(before, short), "at least five stages")
})
