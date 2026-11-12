dmax_run <- function(.data, ...) {
  lactate_threshold(
    .data = .data, intensity_column = "intensity", lactate_column = "lactate",
    method = "Dmax", plot = FALSE, ...
  )
}

test_that("dmax_point finds the analytic maximum distance below the line", {
  # f(x) = x^2 on [0, 1], line y = x: the gap x - x^2 peaks at x = 0.5
  expect_equal(dmax_point(function(x) x^2, 0, 0, 1, 1), 0.5, tolerance = 1e-6)
  # a cubic with two stationary points of the gap: only the one below the line counts
  f <- function(x) x^3 - 1.5 * x^2 + 0.5 * x
  expect_equal(dmax_point(f, 0, 0, 1, 0), 0.5 + sqrt(3) / 6, tolerance = 1e-6)
})

test_that("dmax_point returns NA when the curve never dips below the line", {
  expect_true(is.na(dmax_point(function(x) sqrt(x), 0, 0, 1, 1)))
  expect_true(is.na(dmax_point(function(x) x, NA, 0, 1, 1)))
})

test_that("Dmax intensities do not depend on the lactate scale", {
  # the old code picked the other root once lactate passed 8 mmol/L
  plateau <- dmax_run(fixtures$plateau)
  plateau_x3 <- dmax_run(fixtures$plateau_x3)
  same_shape <- plateau$method != "ModDmax" # ModDmax uses an absolute 0.4 mmol/L rise
  expect_equal(plateau$intensity[same_shape], plateau_x3$intensity[same_shape])
  expect_equal(plateau$intensity[plateau$method == "Dmax"], 176)
})

test_that("Dmax family ignores include_baseline", {
  expect_equal(
    dmax_run(demo_data, include_baseline = TRUE)$intensity,
    dmax_run(demo_data, include_baseline = FALSE)$intensity
  )
})

test_that("dmax_anchor and moddmax_rise are honoured and validated", {
  fitted <- dmax_run(demo_data, dmax_anchor = "fitted")
  observed <- dmax_run(demo_data, dmax_anchor = "observed")
  expect_false(isTRUE(all.equal(fitted$intensity, observed$intensity)))

  # original: Dmax and Exp-Dmax on the curve, ModDmax and Log-*-ModDmax at the measured values
  original <- dmax_run(fixtures$plateau)
  on_curve <- original$method %in% c("Dmax", "Exp-Dmax")
  expect_equal(original$intensity[on_curve], dmax_run(fixtures$plateau, dmax_anchor = "fitted")$intensity[on_curve])
  expect_equal(original$intensity[!on_curve], dmax_run(fixtures$plateau, dmax_anchor = "observed")$intensity[!on_curve])
  expect_true(is.na(dmax_run(demo_data, moddmax_rise = 50)$intensity[2]))
  expect_error(dmax_run(demo_data, moddmax_rise = -1), "moddmax_rise")
  expect_error(dmax_run(demo_data, dmax_anchor = "middle"), "should be one of")
})
