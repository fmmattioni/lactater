results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
lt1 <- attr(results, "lactater")$intensity[["Log-log"]]
lt2 <- attr(results, "lactater")$intensity[["ModDmax"]]

test_that("five zones split the moderate and severe domains equally by default", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5)
  expect_equal(zones$domain, c("moderate", "moderate", "heavy", "severe", "severe"))
  expect_equal(attr(zones, "lactater_zones")$bounds, c(lt1 / 2, lt1, lt2, (lt2 + 191) / 2))
  expect_s3_class(autoplot(zones), "ggplot")
})

test_that("Seiler's cuts sit at 1.5 and 6.0 mmol/L, and fall back to the equal split outside their domain", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = "seiler")
  curve <- attr(results, "lactater")$curve
  seiler <- c(retrieve_intensity(curve, 1.5), retrieve_intensity(curve, 6))
  expect_equal(attr(zones, "lactater_zones")$bounds[c(1, 4)], seiler)
  ## with LT1 below the 1.5 mmol/L point, that cut is not inside the moderate domain
  expect_warning(
    low <- lactate_zones(results, lt1 = "Bsln + 0.5", lt2 = "ModDmax", model = 5, split = "seiler"),
    "1.5 mmol/L"
  )
  expect_equal(attr(low, "lactater_zones")$bounds[1], attr(results, "lactater")$intensity[["Bsln + 0.5"]] / 2)
})

test_that("own cuts are checked against their domains", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = c(70, 170))
  expect_equal(attr(zones, "lactater_zones")$bounds[c(1, 4)], c(70, 170))
  expect_error(lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = c(120, 170)), "`split`")
  expect_error(lactate_zones(results, model = 7), "`model`")
})
