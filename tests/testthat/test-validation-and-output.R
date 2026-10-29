test_that("overall output has expected structure", {
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

  expect_s3_class(results, "tbl")
  expect_equal(nrow(results), 17)
  expect_true(all(c("method_category", "method", "fitting", "intensity", "lactate", "heart_rate") %in% names(results)))
  expect_true(all(!is.na(results$intensity)))
  expect_true(all(!is.na(results$lactate)))
})

test_that("individual methods return expected number of rows", {
  common_args <- list(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )

  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "Log-log")))), 1)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "OBLA")))), 5)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "Bsln+")))), 3)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "Dmax")))), 5)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "LTP")))), 2)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "LTratio")))), 1)
  expect_equal(nrow(do.call(lactate_threshold, c(common_args, list(method = "IAT")))), 1)
})

test_that("input validation errors are informative", {
  invalid_data_non_numeric <- demo_data
  invalid_data_non_numeric$intensity <- as.character(invalid_data_non_numeric$intensity)

  expect_error(
    lactate_threshold(
      .data = invalid_data_non_numeric,
      intensity_column = "intensity",
      lactate_column = "lactate",
      heart_rate_column = "heart_rate",
      method = "OBLA",
      fit = "3rd degree polynomial",
      include_baseline = TRUE,
      sport = "cycling",
      plot = FALSE
    ),
    "must be numeric"
  )

  invalid_data_missing <- demo_data
  invalid_data_missing$lactate[3] <- NA_real_

  expect_warning(
    missing_dropped <- lactate_threshold(
      .data = invalid_data_missing,
      intensity_column = "intensity",
      lactate_column = "lactate",
      heart_rate_column = "heart_rate",
      method = "OBLA",
      fit = "3rd degree polynomial",
      include_baseline = TRUE,
      sport = "cycling",
      plot = FALSE
    ),
    "Dropped 1 row"
  )
  expect_equal(nrow(missing_dropped), 5)

  zero_lactate <- demo_data
  zero_lactate$lactate[2] <- 0
  expect_error(
    lactate_threshold(zero_lactate, "intensity", "lactate", method = "OBLA", plot = FALSE),
    "must be > 0"
  )

  unsorted <- demo_data[c(1, 3, 2, 4:8), ]
  expect_error(
    lactate_threshold(unsorted, "intensity", "lactate", method = "OBLA", plot = FALSE),
    "strictly increasing"
  )

  expect_error(
    lactate_threshold(
      .data = demo_data[1:2, ],
      intensity_column = "intensity",
      lactate_column = "lactate",
      heart_rate_column = "heart_rate",
      method = "OBLA",
      fit = "3rd degree polynomial",
      include_baseline = TRUE,
      sport = "cycling",
      plot = FALSE
    ),
    "At least 3 rows"
  )

  expect_error(
    lactate_threshold(
      .data = demo_data,
      intensity_column = "intensity",
      lactate_column = "lactate",
      heart_rate_column = "heart_rate",
      method = "Log-log",
      fit = "3rd degree polynomial",
      include_baseline = TRUE,
      sport = "cycling",
      loglog_restrainer = NA_real_,
      plot = FALSE
    ),
    "single numeric value"
  )
})
