run <- function(data, ...) lactate_threshold(data, intensity, lactate, heart_rate, plot = FALSE, ...)
before <- run(demo_data)
shifted <- function(by) transform(demo_data, intensity = intensity * by)

test_that("compare_tests calls a change real only beyond the noise of the method", {
  ## +5% on every threshold, typical error 1% of the intensity: interval about +/- 2.3%
  small_noise <- compare_tests(before, run(shifted(1.05)), cv = 1)
  expect_true(all(small_noise$verdict == "increase"))
  big_noise <- compare_tests(before, run(shifted(1.05)), cv = 5)
  expect_true(all(big_noise$verdict == "unclear"))
  ## a smallest worthwhile change above the change itself
  expect_true(all(compare_tests(before, run(shifted(1.05)), cv = 1, swc = 6)$verdict == "unclear"))
  ## default values only where a study reports them
  default <- compare_tests(before, run(shifted(1.05)))
  expect_false(is.na(default$verdict[default$method == "ModDmax"]))
  expect_true(is.na(default$verdict[default$method == "LTP2"]))
  expect_true(all(c("heart_rate_before", "heart_rate_after") %in% names(default)))
  expect_error(compare_tests(before, before, cv = c(1, 2)), "`cv`")
})

test_that("compare_tests notes a different protocol and a fall in peak lactate", {
  tired <- transform(demo_data, lactate = ifelse(intensity > 150, lactate * 0.8, lactate))
  notes <- attr(compare_tests(before, run(tired)), "notes")
  expect_match(notes, "Peak lactate fell", all = FALSE)
  other_protocol <- transform(demo_data, intensity = ifelse(intensity > 0, intensity + 10, 0))
  expect_match(attr(compare_tests(before, run(other_protocol)), "notes"), "protocol differs", all = FALSE)
  expect_null(attr(compare_tests(before, before), "notes"))
  ## a stage length or site recorded for both tests, and different
  notes <- attr(compare_tests(run(demo_data, stage_duration = 3), run(demo_data, stage_duration = 4)), "notes")
  expect_match(notes, "The second test had 4-min stages, not 3", all = FALSE)
  expect_null(attr(compare_tests(run(demo_data, site = "earlobe"), run(demo_data)), "notes"))
})

test_that("curve_shift finds the intensity gained at each lactate", {
  shift <- curve_shift(before, run(shifted(1.1)), lactate = c(2, 4))
  at_2 <- retrieve_intensity(attr(before, "lactater")$curve, 2)
  expect_equal(shift$shift[1], round(0.1 * at_2), tolerance = 1)
  saved <- curve_shift(before, run(shifted(1.1)), lactate = 2, intensity = 150)
  expect_lt(saved$lactate_change[2], 0)
})

test_that("relative_thresholds expresses thresholds per kg and as % of peak", {
  rel <- relative_thresholds(before, body_mass = 70)
  i <- which(rel$method == "OBLA 4.0")
  expect_equal(rel$intensity_per_kg[i], round(attr(before, "lactater")$intensity[["OBLA 4.0"]] / 70, 2))
  expect_equal(rel$percent_peak[i], round(100 * attr(before, "lactater")$intensity[["OBLA 4.0"]] / 191, 1))
  expect_true("percent_peak_heart_rate" %in% names(rel))
  expect_error(relative_thresholds(before, body_mass = -1), "`body_mass`")
})

test_that("lactate_trend recovers a steady change per 30 days", {
  ## +1% per 30 days, with test-to-test scatter
  days <- c(0, 30, 60, 90)
  tests <- lapply(seq_along(days), function(k) run(shifted(1 + days[k] / 3000 + c(0, 0.004, -0.003, 0.002)[k])))
  trend <- do.call(lactate_trend, c(tests, list(day = days)))
  obla <- trend[trend$method == "OBLA 4.0", ]
  at_start <- attr(tests[[1]], "lactater")$intensity[["OBLA 4.0"]]
  expect_equal(obla$per_30_days, at_start / 100, tolerance = 0.25)
  expect_lt(obla$low, obla$per_30_days)
  expect_s3_class(autoplot(trend), "patchwork")
  expect_error(lactate_trend(tests[[1]], tests[[2]], day = 1:2), "at least three")
  ## a list of tests can be spliced in
  expect_equal(lactate_trend(!!!tests, day = days), trend)
})

test_that("the trend plot draws up to five curves with their zones, and more tests against the noise band", {
  days <- c(0, 30, 60)
  tests <- lapply(c(1, 1.005, 1.1), function(by) run(shifted(by)))
  trend <- do.call(lactate_trend, c(tests, list(day = days)))
  curves <- autoplot(trend, lt1 = "LTP1", lt2 = "ModDmax")
  expect_s3_class(curves, "patchwork")
  grDevices::pdf(NULL)
  expect_silent(patchwork::patchworkGrob(curves))
  grDevices::dev.off()
  ## the curves, the heart rate of every test, the zone bars; without heart rate, no heart rate panel
  expect_length(curves, 3)
  expect_equal(nrow(curves[[2]]$data), sum(sapply(tests, function(t) sum(lactater_info(t)$data$intensity > 0))))
  ## LT1 and LT2 of every test as filled points on its heart rate, and the latest test's labelled
  zones <- lapply(tests, lactate_zones, lt1 = "LTP1", lt2 = "ModDmax", model = 5)
  points <- ggplot2::layer_data(curves[[2]], 2)
  expect_equal(points$y, unlist(lapply(zones, function(z) z$heart_rate_from[3:4])))
  labels <- unname(unlist(lapply(curves[[2]]$layers, function(l) {
    if(inherits(l$geom, "GeomCustomAnn")) l$geom_params$grob$children[[1]]$label
  })))
  expect_equal(labels, sprintf("%.0f bpm", zones[[3]]$heart_rate_from[3:4]))
  no_hr <- lapply(c(1, 1.005, 1.1), function(by) lactate_threshold(shifted(by), intensity, lactate, plot = FALSE))
  expect_length(autoplot(do.call(lactate_trend, c(no_hr, list(day = days))), lt1 = "LTP1", lt2 = "ModDmax"), 2)
  expect_error(autoplot(trend, lt2 = "nope"), "The test of Day 0")
  ## with dates, the tests are named by them, and the trend is the same
  dated <- lactate_trend(!!!tests, day = as.Date("2026-01-10") + days)
  expect_equal(attr(dated, "lactater_trend")$labels, c("10 Jan 2026", "9 Feb 2026", "11 Mar 2026"))
  expect_equal(dated$per_30_days, trend$per_30_days)
  ## +0.5% stays inside the band of a 1% typical error (+/- 2.3%); +10% lies outside
  points <- autoplot(trend, view = "thresholds", method = "OBLA 4.0", cv = 1)$data
  expect_equal(points$outside, c(FALSE, FALSE, TRUE))
  expect_error(autoplot(trend, view = "thresholds", cv = c(1, 2)), "`cv`")
  ## more than five tests: each threshold over time
  six <- lactate_trend(!!!rep(tests, 2), day = 1:6)
  expect_false(inherits(autoplot(six), "patchwork"))
  expect_error(autoplot(six, view = "curves"), "more than five")
})

test_that("the comparison plot draws the shift arrows", {
  expect_s3_class(autoplot(compare_tests(before, run(shifted(1.05)))), "ggplot")
})
