test_that("position_rest uses the median step, so missed stages do not shift it", {
  # stage at 125 and 150 W missed: the old intensity[3] - intensity[2] gave a 75 W step
  d <- data.frame(intensity = c(0, 100, 175, 200, 225), lactate = c(1, 1.1, 2, 3, 5))
  expect_equal(position_rest(d)$intensity, c(75, 100, 175, 200, 225))
})

test_that("data without a resting sample keeps every stage", {
  no_rest <- demo_data[-1, ]
  expect_equal(position_rest(no_rest)$intensity, no_rest$intensity)
  fitted <- prepare_fit(prepare_data(no_rest, "intensity", "lactate"), include_baseline = FALSE)
  expect_equal(min(fitted$data_augmented[[1]]$intensity), 50)
})
