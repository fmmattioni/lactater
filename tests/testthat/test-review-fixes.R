results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)

test_that("row-subset results still look up the right threshold", {
  subset <- results[c(10, 2), ]
  dmax <- results$intensity[results$method == "Dmax"]
  expect_equal(lactate_zones(subset, lt1 = "Dmax", lt2 = 200)$intensity_to[1], dmax)
  labels <- unlist(lapply(autoplot(subset, method = "Dmax")$layers, function(l) l$aes_params$label))
  expect_true(paste(dmax, "W") %in% labels)
  expect_equal(lactate_checks(subset)$status[3], "ok")
})

test_that("a custom moddmax_rise is not flagged as a blood value", {
  sweat <- lactate_threshold(fixtures$plateau_x3, intensity, lactate, method = "Dmax", plot = FALSE,
                             sample = "sweat", moddmax_rise = 1.2)
  checks <- lactate_checks(sweat)
  expect_equal(checks$status[checks$check == "Sample type"], "ok")
})

test_that(".by accepts a bare column name", {
  two <- rbind(transform(demo_data, athlete = "A"), transform(demo_data, athlete = "B"))
  by <- lactate_threshold(two, intensity, lactate, method = "OBLA", plot = FALSE, .by = athlete)
  expect_equal(unique(by$athlete), c("A", "B"))
})

test_that("the protocol helper leaves stage 1 alone with two steps", {
  expect_equal(helper_data_protocol(2, 3, 50, 25, FALSE, FALSE, "2:00", "Cycling")$length, c(0, 3))
})
