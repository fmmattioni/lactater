## what the plots say and where they put it, after the design review of 1.0
results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)

test_that("the overview states LT1 and LT2, shades how far each group lies apart, and prints each value", {
  plot <- autoplot(results)
  expect_equal(plot$patches$annotation$title, "LT1 at 83 W, LT2 at 149 W")
  ## the first-rise methods span 77 to 111 W and the second-rise methods 118 to 160 W
  band <- ggplot2::layer_data(plot[[3]], 2)
  expect_equal(round(c(band$xmin, band$xmax)), c(77, 118, 111, 160))
  expect_true(all(c("83", "149") %in% ggplot2::layer_data(plot[[3]], 4)$label))
  ## without one of the two methods of the zones, the title counts the thresholds
  one <- autoplot(lactate_threshold(demo_data, intensity, lactate, method = "OBLA", plot = FALSE))
  expect_equal(one$patches$annotation$title, "5 thresholds from 1 methods, 106 to 146 W")
})

test_that("green marks the answer only: the MLSS bout, not every steady bout or a reference line", {
  colours <- lactater:::lactater_colours()
  bouts <- data.frame(
    intensity = rep(c(130, 140, 150), each = 4), time = rep(c(5, 10, 20, 30), 3),
    lactate = c(2.4, 2.6, 2.7, 2.8, 3.1, 3.4, 3.8, 4.1, 3.9, 4.5, 5.4, 6.3)
  )
  lines <- ggplot2::layer_data(autoplot(lactate_mlss(bouts)), 2)
  expect_equal(unique(lines$colour), unname(colours[c("curve", "accent", "muted")]))
  panels <- autoplot(lactate_diagnostics(results))
  expect_false(colours[["accent"]] %in% unlist(lapply(panels$patches$plots, function(p) {
    lapply(p$layers, function(l) l$aes_params$colour)
  })))
})

test_that("the training load is drawn in minutes, one bar per session", {
  zones <- lactate_zones(results)
  week <- data.frame(session = rep(c("easy", "hard"), c(120, 60)), heart_rate = rep(c(120, 190), c(120, 60)))
  load <- training_load(week, zones, interval = 30, .by = "session")
  bars <- ggplot2::layer_data(autoplot(load), 1)
  expect_equal(sort(unique(bars$y)), c(1, 2))
  expect_equal(max(bars$xmax), 60)
  expect_match(autoplot(load)$labels$title, "of 90 min below LT1")
})

test_that("the race prediction prints each time over its range, and the title gives the first and last event", {
  runner <- data.frame(intensity = c(0, 8, 10, 12, 14, 16, 18), lactate = c(1.0, 1.1, 1.2, 1.6, 2.6, 4.6, 8.0))
  prediction <- predict_performance(lactate_threshold(runner, intensity, lactate, sport = "running", plot = FALSE))
  plot <- autoplot(prediction)
  expect_equal(plot$labels$title, "5 km in 17:44, marathon in 3:08:52")
  expect_equal(ggplot2::layer_data(plot, 5)$label, paste(prediction$time_low, "to", prediction$time_high))
})

test_that("the series plot ends each zone bar at the peak of its test and says how far LT1 and LT2 moved", {
  series <- lactate_tests[lactate_tests$athlete == "cyclist_series", ]
  days <- c(0, 163, 2046)
  tests <- lapply(days, function(d) {
    lactate_threshold(series[series$day == d, ], intensity, lactate, heart_rate, plot = FALSE)
  })
  plot <- autoplot(do.call(lactate_trend, c(tests, list(day = days))), lt1 = "Bsln + 0.5", lt2 = "ModDmax")
  expect_equal(plot$patches$annotation$title, "Since the first test: LT1 +27 W, LT2 +74 W")
  bars <- ggplot2::layer_data(plot[[3]], 1)
  expect_equal(as.numeric(tapply(bars$xmax, bars$ymin, max)), rev(c(360, 400, 440)))
})

test_that("the zones can be drawn as a ruler with the values at each boundary", {
  running <- lactate_tests[lactate_tests$test == "running_recreational", ]
  runner <- lactate_threshold(running, intensity, lactate, heart_rate, sport = "running", plot = FALSE)
  ruler <- autoplot(lactate_zones(runner, model = 5), view = "ruler")
  values <- ggplot2::layer_data(ruler, 7)$label
  ## speed, pace and heart rate at the four boundaries
  expect_equal(values[1:8], c("5.1", "6.8", "14.0", "15.0", "11:50", "8:52", "4:16", "4:00"))
  expect_length(values, 12)
  expect_error(autoplot(lactate_zones(runner), view = "wheel"), "should be one of")
})

test_that("the report opens with one line and flags LT1 beside its number when the methods disagree", {
  skip_if_not_installed("htmltools")
  running <- lactate_tests[lactate_tests$test == "running_recreational", ]
  runner <- lactate_threshold(running, intensity, lactate, heart_rate, sport = "running", plot = FALSE)
  report <- report_start(runner)
  expect_equal(lactater:::html_lede(report), "A running step test of 6 stages, up to 16.0 km/h.")
  html <- as.character(lactater:::html_stats(report))
  expect_match(html, "<span class=\"stat-flag\">!</span> Methods disagree: 6.8&nbsp;to&nbsp;11.8&nbsp;km/h", fixed = TRUE)
  expect_match(html, "8&nbsp;second-rise&nbsp;methods:&nbsp;10.4&nbsp;to&nbsp;14.1&nbsp;km/h", fixed = TRUE)
})

test_that("labels keep clear of the lines beside them, and the zones table leaves no empty or repeated cell", {
  ## the guide of Bsln + 0.5 runs just over the resting sample, so "rest" goes under its dot (and over it for Dmax)
  rest_vjust <- function(method) {
    layers <- autoplot(results, method = method)$layers
    Filter(function(l) identical(l$aes_params$label, "rest"), layers)[[1]]$aes_params$vjust
  }
  expect_equal(rest_vjust("Bsln + 0.5"), 2)
  expect_equal(rest_vjust("Dmax"), -1.2)
  skip_if_not_installed("gt")
  running <- lactate_tests[lactate_tests$test == "running_recreational", ]
  runner <- lactate_threshold(running, intensity, lactate, heart_rate, rpe_column = rpe, sport = "running",
                              plot = FALSE)
  html <- as.character(lactater:::html_zones(lactate_zones(runner, model = 5), "running"))
  ## RPE 9.9 and 10.4 would both print as 10; the lactate below the first stage is a dash
  expect_match(html, "&lt; 9.9", fixed = TRUE)
  expect_match(html, "9.9\u201310.4", fixed = TRUE)
  expect_match(html, ">\u2013</td>")
})
