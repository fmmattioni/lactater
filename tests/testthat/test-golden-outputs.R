golden_run <- function(.data) {
  lactate_threshold(
    .data = .data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )
}

test_that("demo_data golden outputs stay stable", {
  expected <- tibble::tribble(
    ~method_category, ~method, ~intensity, ~lactate, ~heart_rate,
    "Log-log", "Log-log", 100.7, 1.8, 154,
    "OBLA", "OBLA 2.0", 106.8, 2.0, 158,
    "OBLA", "OBLA 2.5", 119.7, 2.5, 167,
    "OBLA", "OBLA 3.0", 129.9, 3.0, 172,
    "OBLA", "OBLA 3.5", 138.4, 3.5, 176,
    "OBLA", "OBLA 4.0", 145.8, 4.0, 180,
    "Bsln+", "Bsln + 0.5", 84.5, 1.4, 142,
    "Bsln+", "Bsln + 1.0", 104.6, 1.9, 157,
    "Bsln+", "Bsln + 1.5", 118.1, 2.4, 166,
    "Dmax", "Dmax", 131.7, 3.1, 173,
    "Dmax", "ModDmax", 140.1, 3.6, 177,
    "Dmax", "Exp-Dmax", 134.8, 3.3, 175,
    "Dmax", "Log-Poly-ModDmax", 149.4, 4.2, 182,
    "Dmax", "Log-Exp-ModDmax", 151.5, 4.4, 183,
    "LTP", "LTP1", 111.0, 2.0, 161,
    "LTP", "LTP2", 159.6, 4.7, 186,
    "LTratio", "LTratio", 71.2, 1.2, 131,
    "IAT", "IAT", 122.2, 2.7, 168
  )
  expect_thresholds(golden_run(fixtures$demo), expected)
})

test_that("noisy fixture outputs stay stable", {
  expected <- tibble::tribble(
    ~method_category, ~method, ~intensity, ~lactate, ~heart_rate,
    "Log-log", "Log-log", 120.3, 2.4, 168,
    "OBLA", "OBLA 2.0", 105.3, 2.0, 157,
    "OBLA", "OBLA 2.5", 118.9, 2.5, 167,
    "OBLA", "OBLA 3.0", 129.5, 3.0, 173,
    "OBLA", "OBLA 3.5", 138.3, 3.5, 177,
    "OBLA", "OBLA 4.0", 146.0, 4.0, 181,
    "Bsln+", "Bsln + 0.5", 81.2, 1.4, 138,
    "Bsln+", "Bsln + 1.0", 103.0, 1.9, 155,
    "Bsln+", "Bsln + 1.5", 117.2, 2.4, 165,
    "Dmax", "Dmax", 132.1, 3.1, 174,
    "Dmax", "ModDmax", 140.6, 3.6, 178,
    "Dmax", "Exp-Dmax", 134.8, 3.3, 176,
    "Dmax", "Log-Poly-ModDmax", 158.1, 4.9, 187,
    "Dmax", "Log-Exp-ModDmax", 159.4, 5.0, 187,
    "LTP", "LTP1", 117.1, 2.2, 165,
    "LTP", "LTP2", 167.9, 5.5, 191,
    "LTratio", "LTratio", 74.4, 1.3, 133,
    "IAT", "IAT", 125.0, 2.8, 171
  )
  expect_thresholds(golden_run(fixtures$noisy), expected)
})
