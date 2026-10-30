with_rpe <- transform(demo_data, rpe = c(6, 8, 10, 12, 14, 16, 18, 20), cr10 = c(0, 1, 2, 3, 5, 7, 9, 10))

test_that("RPE is read at each threshold between the stages, on either scale", {
  res <- lactate_threshold(with_rpe, intensity, lactate, heart_rate, rpe, method = "OBLA", plot = FALSE)
  at <- attr(res, "lactater")$intensity[["OBLA 4.0"]]
  ## between 125 W (RPE 14) and 150 W (RPE 16)
  expect_equal(res$rpe[res$method == "OBLA 4.0"], round(14 + 2 * (at - 125) / 25, 1))
  cr10 <- lactate_threshold(with_rpe, intensity, lactate, rpe_column = "cr10", rpe_scale = "CR10",
                            method = "OBLA", plot = FALSE)
  expect_true(all(cr10$rpe >= 0 & cr10$rpe <= 10, na.rm = TRUE))
  zones <- lactate_zones(res, lt1 = "OBLA 2.0", lt2 = "OBLA 4.0")
  expect_equal(zones$rpe_to[1], res$rpe[res$method == "OBLA 2.0"])
  expect_true(any(grepl("RPE", unlist(lapply(autoplot(res, "OBLA 4.0")$layers, function(l) l$aes_params$label)))))
})

test_that("RPE values outside the scale are rejected", {
  expect_error(lactate_threshold(with_rpe, intensity, lactate, rpe_column = "cr10", plot = FALSE), "between 6 and 20")
  expect_error(
    lactate_threshold(with_rpe, intensity, lactate, rpe_column = "rpe", rpe_scale = "CR10", plot = FALSE),
    "between 0 and 12"
  )
  ## above 10 is allowed on CR10 (absolute maximum)
  above_ten <- transform(with_rpe, cr10 = c(0, 1, 2, 3, 5, 7, 9, 11))
  expect_no_error(
    lactate_threshold(above_ten, intensity, lactate, rpe_column = "cr10", rpe_scale = "CR10", plot = FALSE)
  )
})

test_that("RPE works with .by", {
  two <- rbind(transform(with_rpe, athlete = "A"), transform(with_rpe, athlete = "B"))
  by <- lactate_threshold(two, intensity, lactate, rpe_column = "rpe", method = "OBLA", plot = FALSE, .by = "athlete")
  expect_true("rpe" %in% names(by))
})
