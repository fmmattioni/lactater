test_that("overall function works", {
  results_overall <- lactate_threshold(
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
  expect_s3_class(object = results_overall, class = "tbl")
})
