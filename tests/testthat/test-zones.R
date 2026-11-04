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

test_that("the zones plot shows the heart rate in a panel below, on a second axis, or not at all", {
  zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
  panel <- autoplot(zones)
  expect_s3_class(panel, "patchwork")
  expect_length(panel, 2)
  ## read at LT1 and LT2 as in the table of the zones
  expect_equal(ggplot2::layer_data(panel[[2]], 5)$label, sprintf("%.0f bpm", zones$heart_rate_from[2:3]))
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
