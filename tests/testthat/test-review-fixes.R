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

test_that("a curve with more parameters than points is an error, not a table of NA", {
  four <- demo_data[demo_data$intensity > 0, ][1:4, ]
  expect_error(lactate_threshold(four, intensity, lactate, fit = "B-spline", method = "OBLA", plot = FALSE),
               "B-spline.*5 parameters.*test has 4")
  expect_error(lactate_threshold(demo_data[1:3, ], intensity, lactate, method = "OBLA", plot = FALSE),
               "At least 3 exercise stages")
  ## with four stages the exponential still fits, and lactate_fit_quality() leaves the others NA
  four_exp <- lactate_threshold(four, intensity, lactate, fit = "3rd degree polynomial", method = "OBLA",
                                plot = FALSE)
  quality <- lactate_fit_quality(four_exp)
  expect_true(is.na(quality$r_squared[quality$fit == "B-spline"]))
  expect_false(is.na(quality$r_squared[quality$fit == "Exponential"]))
})

test_that("training_load() reads the column named by measure", {
  zones <- lactate_zones(lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE))
  ride <- data.frame(heart_rate = c(120, 150, 185), intensity = c(80, 120, 170))
  by_intensity <- training_load(ride, zones, measure = "intensity")
  expect_equal(by_intensity$zone_1, training_load(ride, zones, column = "intensity", measure = "intensity")$zone_1)
  expect_equal(unname(unlist(by_intensity[c("zone_1_percent", "zone_2_percent", "zone_3_percent")])), rep(33.3, 3))
})

test_that("series dates in another format are an error, not the year 3", {
  one <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  expect_error(report_start(list(one, one), dates = c("03/11/2026", "10/11/2026")), "YYYY-MM-DD")
  expect_s3_class(report_start(list(one, one), dates = c("2026-11-03", "2026-11-10")), "lactate_test_report")
})

test_that("methods are grouped by what they aim at, as in Faude et al. (2009)", {
  aim <- function(x) as.integer(lactater:::method_aim(x))
  expect_equal(aim(c("Log-log", "LTratio", "LTP1", "Bsln + 0.5", "Bsln + 1.0", "Bsln + 1.4")), rep(1, 6))
  expect_equal(aim(c("Bsln + 1.5", "Dmax", "Log-Poly-ModDmax", "LTP2", "IAT", "Stegmann")), rep(2, 6))
  expect_equal(aim(c("OBLA 2.0", "OBLA 4.0")), c(3, 3))
})

test_that("the checks say when the LT1 or LT2 methods disagree", {
  recreational <- lactate_tests[lactate_tests$test == "running_recreational", ]
  checks <- lactate_checks(lactate_threshold(recreational, intensity, lactate, sport = "running", plot = FALSE))
  expect_equal(checks$status[checks$check == "LT1"], "warning")
  expect_match(checks$details[checks$check == "LT1"], "31% of peak apart (5 methods, Bsln + 0.5 6.8 to Log-log 11.8)",
               fixed = TRUE)
  expect_equal(checks$status[checks$check == "LT2"], "ok")
  ## one method of the set is enough to say nothing
  expect_false("LT1" %in% lactate_checks(lactate_threshold(demo_data, intensity, lactate, method = "Log-log",
                                                            plot = FALSE))$check)
})

test_that("the report says how far the LT1 and LT2 methods lie apart under the chosen ones", {
  skip_if_not_installed("htmltools")
  results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
  html <- as.character(lactater:::html_stats(report_start(results)))
  expect_match(html, "5&nbsp;first-rise&nbsp;methods:&nbsp;77&nbsp;to&nbsp;111&nbsp;W")
  expect_match(html, "8&nbsp;second-rise&nbsp;methods:&nbsp;118&nbsp;to&nbsp;160&nbsp;W")
})
