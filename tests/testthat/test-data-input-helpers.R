# expected values were captured from the dplyr/lubridate implementation before the base R rewrite
test_that("helper_data_protocol builds the protocol skeleton", {
  cycling <- helper_data_protocol(6, 3, 50, 25, TRUE, FALSE, "2:00", "Cycling")
  expect_equal(cycling$step, 0:5)
  expect_equal(cycling$length, c(0, 3, 3, 3, 3, 2))
  expect_equal(cycling$intensity, c(0, 50, 75, 100, 125, 141.67))
  expect_equal(cycling$heart_rate, rep(0L, 6))

  running <- helper_data_protocol(4, 3, 8, 1, FALSE, TRUE, "", "Running")
  expect_equal(running$intensity, c(0, 8, 9, 10))
  expect_false("heart_rate" %in% names(running))

  swimming <- helper_data_protocol(4, 3, 0, 0, FALSE, TRUE, "", "Swimming")
  expect_equal(names(swimming), c("step", "intensity", "lactate"))
})

test_that("helper_last_length_step reads minutes or min:sec", {
  expect_equal(helper_last_length_step("2:15"), 2.2)
  expect_equal(helper_last_length_step("2.5"), 2.5)
})
