test_that("loglog works", {
  results_loglog <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Log-log",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )
  expect_s3_class(object = results_loglog, class = "tbl")
})
