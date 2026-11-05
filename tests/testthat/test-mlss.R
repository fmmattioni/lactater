bouts <- data.frame(
  intensity = rep(c(200, 220, 240), each = 3),
  time = rep(c(10, 20, 30), 3),
  lactate = c(2.0, 2.2, 2.4, 3.0, 3.5, 3.9, 4.0, 4.8, 5.6)
)

test_that("the MLSS is the highest bout where lactate rose by 1 mmol/L or less from minute 10 to 30", {
  mlss <- lactate_mlss(bouts)
  expect_s3_class(mlss, "lactate_mlss")
  expect_equal(mlss$bouts$rise, c(0.4, 0.9, 1.6))
  expect_equal(mlss$bouts$steady, c(TRUE, TRUE, FALSE))
  expect_equal(c(mlss$mlss, mlss$next_bout), c(220, 240))
  ## a rise of exactly the criterion is steady
  expect_equal(lactate_mlss(bouts, rise = 0.9)$mlss, 220)
  expect_equal(lactate_mlss(bouts, rise = 0.3)$mlss, NA_real_)
  expect_output(print(lactate_mlss(bouts, rise = 0.3)), "No bout was steady")
  expect_output(print(lactate_mlss(bouts[bouts$intensity < 240, ])), "or above")
  expect_s3_class(autoplot(mlss), "ggplot")
})

test_that("minutes 10 and 30 are interpolated, and a bout that stopped early is not steady", {
  sparse <- data.frame(intensity = 200, time = c(5, 15, 25, 35), lactate = c(2, 3, 3.4, 3.8))
  out <- lactate_mlss(sparse)$bouts
  expect_equal(c(out$lactate_10, out$lactate_30), c(2.5, 3.6))
  early <- data.frame(intensity = 230, time = c(10, 20, 24), lactate = c(3, 3.2, 3.3))
  stopped <- rbind(bouts[bouts$intensity < 240, ], early)
  mlss <- lactate_mlss(stopped)
  expect_false(mlss$bouts$steady[mlss$bouts$intensity == 230])
  expect_true(is.na(mlss$bouts$lactate_30[mlss$bouts$intensity == 230]))
  expect_equal(c(mlss$mlss, mlss$next_bout), c(220, 230))
})

test_that("each threshold of a step test gets its gap to the MLSS", {
  results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  mlss <- lactate_mlss(transform(bouts, intensity = intensity - 80), results)
  obla <- attr(results, "lactater")$intensity[["OBLA 4.0"]]
  row <- mlss$thresholds[mlss$thresholds$method == "OBLA 4.0", ]
  expect_equal(row$gap, round(obla - 140))
  expect_equal(row$gap_percent, round(100 * (obla / 140 - 1), 1))
  expect_output(print(mlss), "MLSS at 140 W")
})

test_that("wrong input stops, and an unsteady bout below the MLSS warns", {
  expect_error(lactate_mlss(data.frame(intensity = 1, lactate = 2)), "`time`")
  expect_error(lactate_mlss(bouts, rise = 0), "`rise`")
  odd <- transform(bouts, lactate = c(2.0, 2.8, 3.4, 3.0, 3.5, 3.9, 4.0, 4.8, 5.6))
  expect_warning(lactate_mlss(odd), "below the MLSS")
})
