test_that("the polarization index follows Treff et al. (2019)", {
  expect_equal(polarization_index(c(0.80, 0.05, 0.15)), log10(0.8 / 0.05 * 0.15 * 100))
  ## an empty zone 2 becomes 0.01, taken from zone 3
  expect_equal(polarization_index(c(0.8, 0, 0.2)), log10(0.8 / 0.01 * 0.19 * 100))
  expect_equal(polarization_index(c(0.9, 0.1, 0)), 0)
  expect_true(is.na(polarization_index(c(0.3, 0.2, 0.5))))
  expect_equal(intensity_distribution(c(0.80, 0.05, 0.15)), "polarized")
  expect_equal(intensity_distribution(c(0.70, 0.20, 0.10)), "pyramidal")
  expect_equal(intensity_distribution(c(0.40, 0.50, 0.10)), "threshold")
  expect_equal(intensity_distribution(c(0.20, 0.20, 0.60)), "high intensity")
})

test_that("five zones add up to the three domains for the distribution", {
  results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5)
  bounds <- attr(zones, "lactater_zones")$bounds
  ## one minute in each of the five zones (power samples every second)
  levels <- c(bounds[1] - 5, mean(bounds[1:2]), mean(bounds[2:3]), mean(bounds[3:4]), bounds[4] + 5)
  load <- training_load(rep(levels, each = 60), zones, measure = "intensity")
  expect_equal(unname(unlist(load[paste0("zone_", 1:5)])), rep(1, 5))
  expect_equal(c(load$moderate_percent, load$heavy_percent, load$severe_percent), c(40, 20, 40))
  expect_equal(load$polarization_index, round(log10(0.4 / 0.2 * 0.4 * 100), 2))
  ## sessions keep the order they come in
  week <- data.frame(day = rep(c("Tue", "Mon"), each = 60), heart_rate = 120)
  expect_equal(training_load(week, zones, .by = "day")$day, c("Tue", "Mon"))
})

test_that("training_load counts time in the lactate zones", {
  results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
  hr_lt <- zones$heart_rate_to[1:2]
  ## 60 s below LT1, 30 s between, 30 s above, sampled every second
  samples <- c(rep(hr_lt[1] - 10, 60), rep(mean(hr_lt), 30), rep(hr_lt[2] + 5, 30), NA)
  load <- training_load(samples, zones)
  expect_equal(c(load$zone_1, load$zone_2, load$zone_3), c(1, 0.5, 0.5))
  expect_false("lucia_trimp" %in% names(load))
  expect_match(pillar::tbl_sum(load)[["Distribution"]], "50% moderate, 25% heavy, 25% severe")
  expect_s3_class(autoplot(load), "ggplot")
  expect_equal(load$zone_1_percent, 50)

  by_session <- training_load(data.frame(s = rep(1:2, each = 60), heart_rate = rep(c(100, 200), each = 60)), zones,
                              .by = "s")
  expect_equal(by_session$zone_1, c(1, 0))
  expect_equal(by_session$zone_3, c(0, 1))

  power <- training_load(rep(attr(zones, "lactater_zones")$bounds[2] + 1, 120), zones, measure = "intensity")
  expect_equal(power$zone_3, 2)
  expect_error(training_load(samples, results), "lactate_zones")
  expect_error(training_load(samples, zones, interval = 0), "`interval`")
})
