results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
## the first line of each label the plot draws as a grob (zone names, heart rate at LT1 and LT2)
grob_labels <- function(plot) {
  unname(unlist(lapply(plot$layers, function(l) {
    if(inherits(l$geom, "GeomCustomAnn")) l$geom_params$grob$children[[1]]$label
  })))
}
lt1 <- attr(results, "lactater")$intensity[["Log-log"]]
lt2 <- attr(results, "lactater")$intensity[["ModDmax"]]

test_that("five zones split the moderate domain at 75% of LT1 and the severe domain halfway by default", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5)
  expect_equal(zones$domain, c("moderate", "moderate", "heavy", "severe", "severe"))
  expect_equal(attr(zones, "lactater_zones")$bounds, c(0.75 * lt1, lt1, lt2, (lt2 + 191) / 2))
  expect_s3_class(autoplot(zones), "ggplot")
})

test_that("Seiler's cuts sit at 1.5 and 6.0 mmol/L, and fall back to the relative split outside their domain", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = "seiler")
  curve <- attr(results, "lactater")$curve
  seiler <- c(retrieve_intensity(curve, 1.5), retrieve_intensity(curve, 6))
  expect_equal(attr(zones, "lactater_zones")$bounds[c(1, 4)], seiler)
  ## with LT1 below the 1.5 mmol/L point, that cut is not inside the moderate domain
  expect_warning(
    low <- lactate_zones(results, lt1 = "Bsln + 0.5", lt2 = "ModDmax", model = 5, split = "seiler"),
    "1.5 mmol/L"
  )
  expect_equal(attr(low, "lactater_zones")$bounds[1], attr(results, "lactater")$intensity[["Bsln + 0.5"]] * 0.75)
})

test_that("own cuts are checked against their domains", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = c(70, 170))
  expect_equal(attr(zones, "lactater_zones")$bounds[c(1, 4)], c(70, 170))
  expect_error(lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax", model = 5, split = c(120, 170)), "`split`")
  expect_error(lactate_zones(results, model = 7), "`model`")
})

test_that("a zone below the first stage does not stretch the zones plot", {
  swim <- lactate_threshold(lactate_tests[lactate_tests$test == "swimming", ], intensity, lactate, heart_rate,
                            sport = "swimming", plot = FALSE)
  zones <- lactate_zones(swim, lt1 = "LTP1", lt2 = "ModDmax", model = 5)
  ## zone 1 ends far below the swimmer's first stage, so the plot starts above it, at the test
  plot <- autoplot(zones)
  x_range <- ggplot2::ggplot_build(plot[[1]])$layout$panel_params[[1]]$x.range
  expect_gt(x_range[1], zones$intensity_to[1])
  ## the heart rate panel below shares the intensity axis
  expect_equal(ggplot2::ggplot_build(plot[[2]])$layout$panel_params[[1]]$x.range, x_range)
})

test_that("a runner's zones leave the lactate below the first stage unknown, and label narrow zones by number", {
  run <- lactate_tests[lactate_tests$test == "running", ]
  ## no RPE was recorded: the empty column is dropped, so nothing warns or shows an empty RPE
  expect_silent(results <- lactate_threshold(run, intensity, lactate, heart_rate, rpe_column = rpe, sport = "running",
                                             plot = FALSE))
  expect_false("rpe" %in% names(results))
  zones <- expect_silent(lactate_zones(results, lt1 = "LTP1", lt2 = "LTP2", model = 5, split = c(9.3, 22.1)))
  ## zone 1|2 (9.3 km/h) lies below the first stage (12 km/h), where the fitted curve dips below zero
  expect_lt(zones$intensity_to[1], 12)
  expect_true(is.na(zones$lactate_to[1]) && is.na(zones$lactate_from[2]))
  expect_true(all(zones$lactate_from > 0, na.rm = TRUE))
  ## zones 4 and 5 are under a tenth of the axis wide: their numbers only
  expect_equal(grob_labels(autoplot(zones)[[1]]), c("ZONE 2", "ZONE 3", "Z4", "Z5"))
})

test_that("the zones plot shows the heart rate in a panel below, on a second axis, or not at all", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
  panel <- autoplot(zones)
  expect_s3_class(panel, "patchwork")
  expect_length(panel, 2)
  ## read at LT1 and LT2 as in the table of the zones, on a point where the line crosses each threshold
  expect_equal(grob_labels(panel[[2]]), sprintf("%.0f bpm", zones$heart_rate_from[2:3]))
  marked <- ggplot2::layer_data(panel[[2]], 5)
  expect_equal(marked$x, attr(zones, "lactater_zones")$lt)
  expect_equal(marked$y, zones$heart_rate_from[2:3])
  axis <- autoplot(zones, heart_rate = "axis")
  expect_false(inherits(axis, "patchwork"))
  expect_equal(axis$scales$get_scales("y")$secondary.axis$name, "Heart rate (bpm)")
  grDevices::pdf(NULL)
  expect_silent(patchwork::patchworkGrob(panel))
  expect_silent(ggplot2::ggplotGrob(axis))
  grDevices::dev.off()
  expect_false(inherits(autoplot(zones, heart_rate = "none"), "patchwork"))
  expect_error(autoplot(zones, heart_rate = "twin"), "should be one of")
  old <- options(lactater.heart_rate = "none")
  on.exit(options(old), add = TRUE)
  expect_false(inherits(autoplot(zones), "patchwork"))
  ## a test without heart rate has nothing to show
  options(old)
  no_hr <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  no_hr <- lactate_zones(no_hr, lt1 = "Log-log", lt2 = "ModDmax")
  expect_false(inherits(autoplot(no_hr), "patchwork"))
})

test_that("LT1 and LT2 chosen in lactate_threshold() are what the other functions use", {
  chosen <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE, lt1 = "Log-log", lt2 = "LTP2")
  expect_equal(attr(lactate_zones(chosen), "lactater_zones")$labels, c("Log-log", "LTP2"))
  ## an argument still wins, and a result without the choice (saved by an older version) gets the defaults
  expect_equal(attr(lactate_zones(chosen, lt2 = "Dmax"), "lactater_zones")$labels, c("Log-log", "Dmax"))
  old <- chosen
  attr(old, "lactater")$settings[c("lt1", "lt2")] <- NULL
  expect_equal(attr(lactate_zones(old), "lactater_zones")$labels, c("Bsln + 0.5", "Log-Poly-ModDmax"))
  expect_equal(pillar::tbl_sum(chosen)[c("LT1", "LT2")], c(LT1 = "101 W (Log-log)", LT2 = "160 W (LTP2)"))
  expect_equal(suppressWarnings(predict_performance(chosen))$method[1], "LTP2")
  expect_error(lactate_threshold(demo_data, intensity, lactate, lt1 = c("Log-log", "LTP1")), "one method or one")
})
