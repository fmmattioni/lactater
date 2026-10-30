test_that("every test in lactate_tests can be analysed", {
  expect_equal(length(unique(lactate_tests$test)), 15)
  expect_false(any(c("name", "DOB", "test_date") %in% names(lactate_tests)))
  for(name in unique(lactate_tests$test)) {
    one <- lactate_tests[lactate_tests$test == name, ]
    heart_rate <- if(!anyNA(one$heart_rate)) "heart_rate"
    res <- lactate_threshold(
      one, intensity, lactate, heart_rate,
      sport = one$sport[1], plot = FALSE, ltp_breakpoints = if(name == "submaximal") 1 else 2
    )
    expect_true(all(c("OBLA 2.0", "Dmax", "LTP1") %in% res$method[!is.na(res$intensity)]), info = name)
  }
})

test_that("the breakpoint search survives out-of-order proposals (real running test, uneven steps)", {
  running <- lactate_tests[lactate_tests$test == "running" & lactate_tests$intensity > 0, ]
  fit <- fit_breakpoints(running$intensity, running$lactate)
  expect_false(is.unsorted(fit$psi))
  expect_true(all(fit$psi > min(running$intensity) & fit$psi < max(running$intensity)))
})
