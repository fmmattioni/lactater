run <- function(...) lactate_threshold(demo_data, "intensity", "lactate", plot = FALSE, ...)

test_that("fixed, baseline_plus and iat_rise set the targets and the method names", {
  res <- run(method = c("OBLA", "Bsln+", "IAT"), fixed = c(4, 2.25), baseline_plus = 2, iat_rise = 1)
  expect_equal(as.character(res$method), c("OBLA 2.25", "OBLA 4.0", "Bsln + 2.0", "IAT"))
  expect_equal(res$lactate[1:2], c(2.25, 4))
  expect_equal(res$lactate[3], demo_data$lactate[1] + 2)
  default_iat <- run(method = "IAT")
  expect_lt(res$intensity[4], default_iat$intensity)
  expect_error(run(fixed = c(2, -1)), "`fixed`")
  expect_error(run(baseline_plus = numeric(0)), "`baseline_plus`")
  expect_error(run(iat_rise = c(1, 2)), "`iat_rise`")
  expect_error(run(ltp_breakpoints = 3), "`ltp_breakpoints`")
})

test_that("ltp_breakpoints = 1 fits one turn point for a submaximal test", {
  submax <- data.frame(
    intensity = c(0, seq(100, 250, by = 25)),
    lactate = c(0.9, 1.0, 1.0, 1.1, 1.4, 1.9, 2.5, 3.2)
  )
  one <- lactate_threshold(submax, intensity, lactate, method = "LTP", plot = FALSE, ltp_breakpoints = 1)
  expect_true(is.na(one$intensity[2]))
  expect_gt(one$intensity[1], 125)
  expect_lt(one$intensity[1], 200)
  two <- lactate_threshold(submax, intensity, lactate, method = "LTP", plot = FALSE)
  peak_check <- function(x) {
    checks <- lactate_checks(x)
    checks$status[checks$check == "Peak lactate"]
  }
  expect_equal(peak_check(two), "warning")
  expect_equal(peak_check(one), "ok")
})

test_that("LTP and Log-log report the lactate on their own fitted lines", {
  res <- run(method = c("Log-log", "LTP"))
  info <- attr(res, "lactater")
  ltp <- info$plot_data[["LTP2"]]$breakpoints
  expect_equal(res$lactate[3], round(predict_breakpoints(ltp, info$intensity[["LTP2"]]), 1))
  loglog <- info$plot_data[["Log-log"]]$breakpoints
  expect_equal(res$lactate[1], round(exp(predict_breakpoints(loglog, log(info$intensity[["Log-log"]]))), 1))
})

test_that("the curve is solved at its last rising crossing", {
  # rises through 4 at x = 1, falls back through it at x = 3
  hill <- list(domain = c(0, 4), predict = function(x) 5 - (x - 2)^2)
  expect_equal(retrieve_intensity(hill, 4), 1, tolerance = 1e-6)
  falling <- list(domain = c(0, 4), predict = function(x) 5 - x)
  expect_true(is.na(retrieve_intensity(falling, 3)))
})

test_that("column arguments accept bare names, strings, and variables that hold a name", {
  bare <- lactate_threshold(demo_data, intensity, lactate, heart_rate, method = "OBLA", plot = FALSE)
  hr <- "heart_rate"
  from_variable <- lactate_threshold(demo_data, "intensity", "lactate", hr, method = "OBLA", plot = FALSE)
  expect_equal(from_variable$heart_rate, bare$heart_rate)
  no_hr <- NULL
  without <- lactate_threshold(demo_data, intensity, lactate, no_hr, method = "OBLA", plot = FALSE)
  expect_false("heart_rate" %in% names(without))
  expect_error(lactate_threshold(demo_data, intensity, lactate, not_a_column, method = "OBLA"), "not found")
})

test_that("loglog_restrainer is deprecated but still applied", {
  expect_warning(res <- run(method = "Log-log", loglog_restrainer = 1), "deprecated")
  expect_equal(res$intensity, run(method = "Log-log")$intensity)
})
