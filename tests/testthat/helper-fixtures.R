# Edge-case tests that exercise the corners of the methods. Intensity 0 marks the resting sample.
fixtures <- list(
  demo = demo_data,
  noisy = transform(
    demo_data,
    lactate = c(0.93, 1.02, 1.30, 1.95, 2.72, 4.35, 6.45, 8.55),
    heart_rate = c(96, 115, 133, 153, 171, 183, 194, 198)
  ),
  # lactate levels off in the last stages, so the fitted cubic has a negative leading coefficient
  plateau = data.frame(
    intensity = c(0, 100, 150, 200, 250, 300, 350, 400),
    lactate = c(1.0, 1.1, 1.3, 2.0, 3.4, 5.2, 6.4, 6.9)
  ),
  # same shape at sweat-lactate magnitudes
  plateau_x3 = data.frame(
    intensity = c(0, 100, 150, 200, 250, 300, 350, 400),
    lactate = 3 * c(1.0, 1.1, 1.3, 2.0, 3.4, 5.2, 6.4, 6.9)
  ),
  # first stage higher than the next ones (the early "lactate dip")
  early_dip = data.frame(
    intensity = c(0, 100, 150, 200, 250, 300, 350),
    lactate = c(1.0, 1.6, 1.2, 1.4, 2.2, 4.0, 7.5)
  ),
  # no resting sample
  no_rest = demo_data[-1, ],
  # the made-up resting intensity falls below zero (40 W start, 50 W steps)
  negative_rest = data.frame(
    intensity = c(0, 40, 90, 140, 190, 240, 290),
    lactate = c(1.2, 1.1, 1.2, 1.6, 2.5, 4.2, 7.0)
  ),
  running = data.frame(
    intensity = c(0, 8, 10, 12, 14, 16, 18),
    lactate = c(1.1, 1.0, 1.3, 1.9, 3.1, 5.4, 8.9),
    heart_rate = c(70, 128, 142, 156, 170, 182, 191)
  ),
  swimming = data.frame(
    intensity = c(0, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5),
    lactate = c(1.0, 1.2, 1.4, 2.0, 3.2, 5.5, 8.8)
  )
)

fixture_sport <- function(name) {
  switch(name, running = "running", swimming = "swimming", "cycling")
}

# Absolute tolerances: expect_equal(tolerance=) is relative, which hid 15% drifts before.
expect_thresholds <- function(results, expected, tol_intensity = 0.11, tol_lactate = 0.051, tol_hr = 1) {
  ## the table rounds intensities to the sport's decimals (whole watts); compare the unrounded values
  unrounded <- as.data.frame(lapply(results, function(x) if (is.factor(x)) as.character(x) else x))
  unrounded$intensity <- unname(attr(results, "lactater")$intensity[unrounded$method])
  joined <- merge(
    unrounded,
    expected,
    by = c("method_category", "method"),
    suffixes = c("_obs", "_exp")
  )
  testthat::expect_equal(nrow(joined), nrow(expected))
  testthat::expect_lte(max(abs(joined$intensity_obs - joined$intensity_exp)), tol_intensity)
  testthat::expect_lte(max(abs(joined$lactate_obs - joined$lactate_exp)), tol_lactate)
  testthat::expect_lte(max(abs(joined$heart_rate_obs - joined$heart_rate_exp)), tol_hr)
}
