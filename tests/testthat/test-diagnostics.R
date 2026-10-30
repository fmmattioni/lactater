female <- subset(lactate_tests, test == "cycling_female")
results <- lactate_threshold(female, intensity, lactate, plot = FALSE)
diagnostics <- lactate_diagnostics(results)

test_that("lactate_diagnostics matches lm() for the polynomial curve", {
  stages <- diagnostics$stages
  model <- stats::lm(lactate ~ poly(intensity, 3, raw = TRUE), stages)
  expect_equal(stages$leverage, unname(stats::hatvalues(model)))
  expect_equal(stages$standardized, unname(stats::rstandard(model)))
  expect_equal(stages$cooks_distance, unname(stats::cooks.distance(model)))
  expect_equal(diagnostics$cut, stats::qf(0.5, 4, nrow(stages) - 4))
  ## for a linear fit the leave-one-out error is the PRESS residual e / (1 - h)
  press <- sqrt(mean((stats::residuals(model) / (1 - stats::hatvalues(model)))^2))
  expect_equal(diagnostics$fits$prediction_error[diagnostics$fits$fit == "3rd degree polynomial"], press)
})

test_that("the jackknife summarises leaving each stage out", {
  loo <- diagnostics$leave_one_out
  expect_equal(nrow(loo), length(unique(results$method)) * sum(female$intensity > 0))
  obla <- loo$intensity[loo$method == "OBLA 4.0"]
  row <- diagnostics$jackknife[diagnostics$jackknife$method == "OBLA 4.0", ]
  expect_equal(row$jackknife_se, sqrt((length(obla) - 1) / length(obla) * sum((obla - mean(obla))^2)))
  expect_equal(c(row$lowest, row$highest), range(obla))
  expect_output(print(diagnostics), "Best-predicting curve")
  expect_s3_class(autoplot(diagnostics), "patchwork")
})
