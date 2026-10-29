results <- lactate_threshold(demo_data, "intensity", "lactate", "heart_rate", plot = FALSE)

test_that("lactate_threshold returns a lactate_thresholds tibble", {
  expect_s3_class(results, c("lactate_thresholds", "tbl_df"))
  expect_equal(names(results)[1:2], c("method_category", "method"))
  expect_match(pillar::tbl_sum(results), "cycling, 3rd degree polynomial, 7 stages \\+ rest")
})

test_that("autoplot draws the overview and single methods", {
  expect_s3_class(autoplot(results), "patchwork")
  expect_s3_class(autoplot(results, method = "Dmax"), "ggplot")
  expect_error(autoplot(results, method = "Nope"), "must be one of")
})

test_that("autoplot explains when the attributes are gone", {
  stripped <- results
  attr(stripped, "lactater") <- NULL
  expect_error(autoplot(stripped), "lost its lactater attributes")
})

test_that("the plot column still holds one plot per threshold", {
  with_plots <- lactate_threshold(demo_data, "intensity", "lactate", method = "OBLA")
  expect_length(with_plots$plot, 5)
  expect_s3_class(with_plots$plot[[1]], "ggplot")
})
