## three athletes tested twice: known intensities, so the typical error can be computed by hand
repeats <- data.frame(
  athlete = rep(c("a", "b", "c"), each = 2), test = rep(1:2, 3),
  method = "OBLA 4.0", intensity = c(200, 206, 250, 247, 180, 189)
)

test_that("the typical error is the residual of athletes and tests (Hopkins, 2000)", {
  out <- lactate_reliability(repeats, "athlete", "test")
  ## with two tests it is the SD of the differences over sqrt(2), on the log scale for the CV
  change <- function(v) v[c(FALSE, TRUE)] - v[c(TRUE, FALSE)]
  expect_equal(out$typical_error, round(stats::sd(change(repeats$intensity)) / sqrt(2), 2))
  expect_equal(out$cv, round(100 * (exp(stats::sd(change(log(repeats$intensity))) / sqrt(2)) - 1), 2))
  expect_equal(c(out$n_athletes, out$n_tests), c(3, 6))
})

test_that("athletes with one test are left out, and too few athletes give NA", {
  one_test <- rbind(repeats, data.frame(athlete = "d", test = 1, method = "OBLA 4.0", intensity = 300))
  expect_equal(lactate_reliability(one_test, "athlete", "test"), lactate_reliability(repeats, "athlete", "test"))
  expect_true(is.na(lactate_reliability(repeats[1:2, ], "athlete", "test")$cv))
  expect_error(lactate_reliability(repeats, "athlete", "trial"), "`athlete` and `test`")
})

test_that("compare_tests() takes the result as its cv", {
  before <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
  after <- lactate_threshold(transform(demo_data, intensity = intensity * 1.05), intensity, lactate, plot = FALSE)
  own <- lactate_reliability(repeats, "athlete", "test")
  by_table <- compare_tests(before, after, cv = own)
  by_vector <- compare_tests(before, after, cv = c("OBLA 4.0" = own$cv))
  expect_equal(by_table, by_vector)
})

test_that("it runs on lactate_threshold(.by = ) results", {
  set.seed(3)
  tests <- do.call(rbind, lapply(1:3, function(a) do.call(rbind, lapply(1:2, function(t) {
    transform(demo_data, athlete = a, test = t, lactate = lactate * exp(rnorm(nrow(demo_data), 0, 0.04)))
  }))))
  results <- lactate_threshold(tests, intensity, lactate, method = "OBLA", plot = FALSE, .by = c("athlete", "test"))
  out <- lactate_reliability(results, "athlete", "test")
  expect_equal(out$method, paste("OBLA", c("2.0", "2.5", "3.0", "3.5", "4.0")))
  expect_true(all(out$cv > 0 & out$cv < 10))
})

test_that("tests are matched by their order within each athlete, so dates work", {
  dated <- transform(repeats, test = as.Date("2026-01-01") + c(0, 7, 30, 37, 60, 64))
  expect_equal(lactate_reliability(dated, "athlete", "test"), lactate_reliability(repeats, "athlete", "test"))
})
