test_that("demo_data golden outputs stay stable", {
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
  ) %>%
    dplyr::select(method_category, method, intensity, lactate, heart_rate)

  expected <- tibble::tribble(
    ~method_category, ~method, ~intensity, ~lactate, ~heart_rate,
    "Log-log", "Log-log", 92.8, 1.6, 145,
    "OBLA", "OBLA 2.0", 107.0, 2.0, 153,
    "OBLA", "OBLA 2.5", 120.0, 2.5, 161,
    "OBLA", "OBLA 3.0", 130.0, 3.0, 167,
    "OBLA", "OBLA 3.5", 138.0, 3.5, 172,
    "OBLA", "OBLA 4.0", 146.0, 4.0, 177,
    "Bsln+", "Bsln + 0.5", 84.5, 1.43, 140,
    "Bsln+", "Bsln + 1.0", 105.0, 1.93, 152,
    "Bsln+", "Bsln + 1.5", 118.0, 2.43, 160,
    "Dmax", "Dmax", 132.0, 3.1, 168,
    "Dmax", "ModDmax", 140.0, 3.6, 173,
    "Dmax", "Exp-Dmax", 135.0, 3.3, 170,
    "Dmax", "Log-Poly-ModDmax", 147.0, 4.0, 177,
    "Dmax", "Log-Exp-ModDmax", 149.0, 4.2, 179,
    "LTP", "LTP1", 100.0, 1.8, 150,
    "LTP", "LTP2", 149.0, 4.3, 179,
    "LTratio", "LTratio", 71.2, 1.2, 132
  )

  joined <- results %>%
    dplyr::mutate(method = as.character(method), method_category = as.character(method_category)) %>%
    dplyr::left_join(expected, by = c("method_category", "method"), suffix = c("_obs", "_exp"))

  expect_equal(nrow(joined), 17)
  expect_true(all(!is.na(joined$intensity_exp)))
  expect_equal(unname(joined$intensity_obs), unname(joined$intensity_exp), tolerance = 0.15)
  expect_equal(unname(joined$lactate_obs), unname(joined$lactate_exp), tolerance = 0.05)
  expect_equal(unname(joined$heart_rate_obs), unname(joined$heart_rate_exp), tolerance = 1)
})

test_that("noisy fixture outputs stay within golden tolerance", {
  noisy_data <- demo_data
  noisy_data$lactate <- c(0.93, 1.02, 1.30, 1.95, 2.72, 4.35, 6.45, 8.55)
  noisy_data$heart_rate <- c(96, 115, 133, 153, 171, 183, 194, 198)

  results <- lactate_threshold(
    .data = noisy_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio"),
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  ) %>%
    dplyr::select(method_category, method, intensity, lactate, heart_rate)

  expected <- tibble::tribble(
    ~method_category, ~method, ~intensity, ~lactate, ~heart_rate,
    "Log-log", "Log-log", 94.9, 1.7, 147,
    "OBLA", "OBLA 2.0", 105.0, 2.0, 153,
    "OBLA", "OBLA 2.5", 119.0, 2.5, 161,
    "OBLA", "OBLA 3.0", 130.0, 3.0, 167,
    "OBLA", "OBLA 3.5", 138.0, 3.5, 173,
    "OBLA", "OBLA 4.0", 146.0, 4.0, 177,
    "Bsln+", "Bsln + 0.5", 81.2, 1.43, 138,
    "Bsln+", "Bsln + 1.0", 103.0, 1.93, 151,
    "Bsln+", "Bsln + 1.5", 117.0, 2.43, 160,
    "Dmax", "Dmax", 132.0, 3.1, 169,
    "Dmax", "ModDmax", 141.0, 3.6, 174,
    "Dmax", "Exp-Dmax", 135.0, 3.3, 170,
    "Dmax", "Log-Poly-ModDmax", 148.0, 4.1, 178,
    "Dmax", "Log-Exp-ModDmax", 150.0, 4.2, 179,
    "LTP", "LTP1", 101.0, 1.9, 150,
    "LTP", "LTP2", 150.0, 4.3, 180,
    "LTratio", "LTratio", 74.4, 1.3, 134
  )

  joined <- results %>%
    dplyr::mutate(method = as.character(method), method_category = as.character(method_category)) %>%
    dplyr::left_join(expected, by = c("method_category", "method"), suffix = c("_obs", "_exp"))

  expect_equal(nrow(joined), 17)
  expect_true(all(!is.na(joined$intensity_exp)))
  expect_equal(unname(joined$intensity_obs), unname(joined$intensity_exp), tolerance = 0.2)
  expect_equal(unname(joined$lactate_obs), unname(joined$lactate_exp), tolerance = 0.06)
  expect_equal(unname(joined$heart_rate_obs), unname(joined$heart_rate_exp), tolerance = 1)
})
