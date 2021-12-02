test_that("dmax works", {
  results_dmax <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Dmax",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )
  expect_s3_class(object = results_dmax, class = "tbl")
})
