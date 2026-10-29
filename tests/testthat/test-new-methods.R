run <- function(...) lactate_threshold(demo_data, "intensity", "lactate", plot = FALSE, ...)

test_that("IAT is 1.5 mmol/L above the LTratio lactate on the same curve", {
  res <- run(method = c("LTratio", "IAT"))
  expect_equal(as.character(res$method), c("LTratio", "IAT"))
  spline <- fit_curve(modeling_data(prepare_data(demo_data, "intensity", "lactate"), FALSE), "B-spline")
  ltratio <- attr(res, "lactater")$intensity[1]
  iat <- attr(res, "lactater")$intensity[2]
  expect_equal(spline$predict(iat) - spline$predict(ltratio), 1.5, tolerance = 1e-6)
  expect_gt(iat, ltratio)
})

test_that("dmax_methods selects variants and adds LTP1-Poly-ModDmax on request", {
  res <- run(method = "Dmax", dmax_methods = c("LTP1-Poly-ModDmax", "Dmax"))
  expect_equal(as.character(res$method), c("Dmax", "LTP1-Poly-ModDmax"))
  ltp1 <- run(method = "LTP", include_baseline = FALSE)$intensity[1]
  expect_gt(res$intensity[2], ltp1)
  expect_error(run(method = "Dmax", dmax_methods = "Dmin"), "dmax_methods")
})

test_that("max_intensity drops the stages above it from every method", {
  full <- run(method = "Dmax", dmax_methods = "Dmax")
  cut <- run(method = "Dmax", dmax_methods = "Dmax", max_intensity = 175)
  expect_false(isTRUE(all.equal(full$intensity, cut$intensity)))
  expect_equal(sum(attr(cut, "lactater")$data$intensity > 0), 6)
  expect_error(run(method = "OBLA", max_intensity = 60), "Fewer than 3 stages")
})

test_that("vlamax follows (peak - pre) / (duration - alactic time)", {
  res <- vlamax(lactate_pre = 1.1, lactate_post = c(8.9, 10.4, 10.9, 10.6))
  expect_equal(res$vlamax, (10.9 - 1.1) / (15 - 3.5))
  expect_equal(res$glycolytic_time, 11.5)
  expect_error(vlamax(1, 8, test_duration = 3, alactic_time = 3.5), "longer than")
  expect_error(vlamax(0, 8), "lactate_pre")
})
