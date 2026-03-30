test_that("fit stats are returned for all methods", {
  results <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio"),
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )

  expect_true("fit_rmse" %in% names(results))
  expect_true("fit_r_squared" %in% names(results))
  expect_true("fit_mae" %in% names(results))
  expect_true("fit_see" %in% names(results))
  expect_true("fit_n_points" %in% names(results))
  expect_true(all(!is.na(results$fit_rmse)))
  expect_true(all(!is.na(results$fit_r_squared)))
})

test_that("fit stats have plausible values", {
  results <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Dmax",
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = "cycling",
    plot = FALSE
  )

  expect_true(all(results$fit_rmse > 0))
  expect_true(all(results$fit_rmse < 5))
  expect_true(all(results$fit_r_squared > 0.5))
  expect_true(all(results$fit_r_squared <= 1.0))
  expect_true(all(results$fit_mae > 0))
  expect_true(all(results$fit_see > 0))
  expect_true(all(results$fit_n_points >= 3))
})

test_that("Dmax methods report correct fit type per method", {
  results <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Dmax",
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = "cycling",
    plot = FALSE
  )

  expect_true("fit_type" %in% names(results))

  poly_methods <- c("Dmax", "ModDmax", "Log-Poly-ModDmax")
  exp_methods <- c("Exp-Dmax", "Log-Exp-ModDmax")

  for (m in poly_methods) {
    row <- results[results$method == m, ]
    if (nrow(row) > 0) {
      expect_equal(row$fit_type, "3rd degree polynomial")
    }
  }

  for (m in exp_methods) {
    row <- results[results$method == m, ]
    if (nrow(row) > 0) {
      expect_equal(row$fit_type, "Exponential")
    }
  }
})

test_that("polynomial and exponential fit stats differ", {
  results <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Dmax",
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = "cycling",
    plot = FALSE
  )

  poly_rmse <- results[results$method == "Dmax", ]$fit_rmse
  exp_rmse <- results[results$method == "Exp-Dmax", ]$fit_rmse

  expect_false(identical(poly_rmse, exp_rmse))
})

test_that("fit stats work with include_baseline TRUE and FALSE", {
  results_bsln <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "OBLA",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )

  results_no_bsln <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "OBLA",
    fit = "3rd degree polynomial",
    include_baseline = FALSE,
    sport = "cycling",
    plot = FALSE
  )

  expect_true(all(!is.na(results_bsln$fit_rmse)))
  expect_true(all(!is.na(results_no_bsln$fit_rmse)))
  expect_true(results_bsln$fit_n_points[1] > results_no_bsln$fit_n_points[1])
})
