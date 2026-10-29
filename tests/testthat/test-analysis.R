results <- lactate_threshold(demo_data, "intensity", "lactate", "heart_rate", plot = FALSE)

test_that("ci adds intervals that contain the estimate", {
  set.seed(1)
  with_ci <- lactate_threshold(demo_data, "intensity", "lactate", method = c("OBLA", "Dmax"), plot = FALSE, ci = TRUE, n_boot = 60)
  expect_true(all(c("intensity_low", "intensity_high") %in% names(with_ci)))
  ok <- !is.na(with_ci$intensity_low)
  expect_true(all(with_ci$intensity_low[ok] <= with_ci$intensity[ok] + 0.1))
  expect_true(all(with_ci$intensity_high[ok] >= with_ci$intensity[ok] - 0.1))
  expect_error(lactate_threshold(demo_data, "intensity", "lactate", ci = TRUE, n_boot = 5), "n_boot")
})

test_that(".by analyses each test on its own", {
  two <- rbind(transform(demo_data, athlete = "A"), transform(fixtures$noisy, athlete = "B"))
  by <- lactate_threshold(two, intensity, lactate, method = "OBLA", plot = FALSE, .by = "athlete")
  expect_equal(names(by)[1], "athlete")
  expect_equal(nrow(by), 10)
  one <- lactate_threshold(fixtures$noisy, intensity, lactate, method = "OBLA", plot = FALSE)
  expect_equal(by$intensity[by$athlete == "B"], one$intensity)
  expect_error(lactate_threshold(two, intensity, lactate, .by = "team"), ".by")
})

test_that("pace follows the sport", {
  expect_equal(convert_to_pace(12, "running"), 300)
  expect_equal(convert_to_pace(2.80 / 0.25^3, "rowing"), 125) # Concept2: 179.2 W is a 2:05 split
  expect_equal(format_pace(125, "rowing"), "2:05 /500 m")
  rowing <- lactate_threshold(demo_data, "intensity", "lactate", method = "OBLA", sport = "rowing", plot = FALSE)
  expect_true("pace" %in% names(rowing))
})

test_that("lactate_zones uses the chosen LT1 and LT2", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "Dmax")
  expect_s3_class(zones, "lactate_zones")
  expect_equal(zones$intensity_to[1], results$intensity[results$method == "Log-log"])
  expect_equal(zones$intensity_from[3], results$intensity[results$method == "Dmax"])
  expect_true(is.na(zones$intensity_from[1]) && is.na(zones$intensity_to[3]))
  expect_equal(lactate_zones(results, lt1 = 100, lt2 = 150)$intensity_from[2:3], c(100, 150))
  expect_error(lactate_zones(results, lt1 = "Dmax", lt2 = "Log-log"), "below LT2")
  expect_error(lactate_zones(results, lt1 = "Nope"), "lt1")
  expect_s3_class(autoplot(zones), "ggplot")
})

test_that("lactate_checks flags problems", {
  checks <- lactate_checks(results)
  expect_equal(checks$status[checks$check == "Stages"], "ok")
  sweat <- lactate_threshold(fixtures$plateau_x3, "intensity", "lactate", plot = FALSE, sample = "sweat")
  sweat_checks <- lactate_checks(sweat)
  expect_equal(sweat_checks$status[sweat_checks$check == "Sample type"], "warning")
  expect_equal(sweat_checks$status[sweat_checks$check == "Final stage"], "ok")
  no_rest <- lactate_checks(lactate_threshold(fixtures$no_rest, "intensity", "lactate", method = "OBLA", plot = FALSE))
  expect_equal(no_rest$status[no_rest$check == "Resting sample"], "note")
  expect_match(pillar::tbl_sum(sweat)[["Checks"]], "warning")
})

test_that("lactate_fit_quality compares the curves on the same points", {
  quality <- lactate_fit_quality(results)
  expect_equal(quality$fit[quality$used], "3rd degree polynomial")
  expect_true(all(quality$r_squared > 0.9 & quality$r_squared <= 1))
  expect_equal(unique(quality$n_points), 7)
})

test_that("compare_tests reports the change for shared methods", {
  after <- lactate_threshold(transform(demo_data, intensity = intensity * 1.05), "intensity", "lactate", method = "OBLA", plot = FALSE)
  before <- lactate_threshold(demo_data, "intensity", "lactate", method = c("OBLA", "Dmax"), plot = FALSE)
  comparison <- compare_tests(before, after)
  expect_equal(nrow(comparison), 5)
  expect_true(all(abs(comparison$change_percent - 5) < 0.2))
  expect_s3_class(autoplot(comparison), "ggplot")
})
