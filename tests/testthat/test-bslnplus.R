test_that("bslnplus works", {
  results_bsln_plus <- lactate_threshold(
    .data = demo_data,
    intensity_column = "intensity",
    lactate_column = "lactate",
    heart_rate_column = "heart_rate",
    method = "Bsln+",
    fit = "3rd degree polynomial",
    include_baseline = TRUE,
    sport = "cycling",
    plot = FALSE
  )
  expect_s3_class(object = results_bsln_plus, class = "tbl")
})
