## method categories in output order
method_categories <- c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio", "IAT")

#' Compute thresholds
#'
#' Fits the user-selected curve and runs the requested methods. For internal use only.
#'
#' @param data The data retrieved from `prepare_data()`.
#' @param settings The analysis settings.
#' @param method The method categories to run.
#'
#' @return a list with the user `curve`, the result `rows` (with `method_category`) and the
#' `plot_data` for each row.
#' @keywords internal
compute_thresholds <- function(data, settings, method) {
  curve <- fit_curve(modeling_data(data, settings$include_baseline), settings$fit)
  results <- lapply(method_categories[method_categories %in% method], function(category) {
    method_function <- switch(
      category,
      "Log-log" = method_loglog,
      "OBLA" = method_obla,
      "Bsln+" = method_bsln_plus,
      "Dmax" = method_dmax,
      "LTP" = method_ltp,
      "LTratio" = method_ltratio,
      "IAT" = method_iat
    )
    result <- method_function(data, curve, settings)
    result$rows$method_category <- category
    result
  })
  list(
    curve = curve,
    rows = do.call(rbind, lapply(results, `[[`, "rows")),
    plot_data = do.call(c, lapply(results, `[[`, "plot_data"))
  )
}

#' Bootstrap confidence intervals
#'
#' Residual bootstrap around the user-selected curve: each replicate keeps the fitted values and
#' adds residuals resampled with replacement, after scaling each residual by `1 / sqrt(1 - leverage)`
#' and centering them (Davison & Hinkley, 1997, Example 6.8). Every method is then run again on the
#' replicate. The interval is the percentile interval of the replicate intensities. For internal use only.
#'
#' @inheritParams compute_thresholds
#' @param curve The user-selected curve fitted to the original data.
#' @param n_rows Number of thresholds the methods return.
#' @param n_boot Number of replicates.
#' @param level Confidence level.
#'
#' @return a matrix with one row per threshold and the columns `low` and `high`.
#' @keywords internal
bootstrap_intervals <- function(data, settings, method, curve, n_rows, n_boot, level) {
  in_fit <- if(settings$include_baseline) rep(TRUE, nrow(data)) else data$intensity > 0
  fitted <- curve$predict(curve$points$intensity)
  if(any(curve$leverage > 1 - 1e-8)) {
    warning("Too few stages for the chosen fit to estimate confidence intervals.", call. = FALSE)
    return(NULL)
  }
  residuals <- (curve$points$lactate - fitted) / sqrt(1 - curve$leverage)
  residuals <- residuals - mean(residuals)

  replicates <- vapply(seq_len(n_boot), function(b) {
    replicate_data <- data
    replicate_data$lactate[in_fit] <- fitted + sample(residuals, sum(in_fit), replace = TRUE)
    replicate_data$lactate[!in_fit] <- data$lactate[!in_fit] + sample(residuals, sum(!in_fit), replace = TRUE)
    ## a lactate value cannot be negative
    replicate_data$lactate <- pmax(replicate_data$lactate, 0.05)
    tryCatch(
      compute_thresholds(replicate_data, settings, method)$rows$intensity,
      error = function(e) rep(NA_real_, n_rows)
    )
  }, numeric(n_rows))

  alpha <- (1 - level) / 2
  replicates <- matrix(replicates, nrow = n_rows)
  t(apply(replicates, 1, function(r) {
    stats::setNames(stats::quantile(r, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE), c("low", "high"))
  }))
}
