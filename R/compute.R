## method categories in output order
method_categories <- c("Log-log", "OBLA", "Bsln+", "Dmax", "LTP", "LTratio", "IAT", "Stegmann")

## what each method aims at, in the three categories of Faude et al. (2009): the first rise above baseline (LT1),
## the MLSS or the second, abrupt rise (LT2), and fixed lactate values. Bsln + 1.5 goes with LT2 as the rise that
## estimated the MLSS (Jamnick et al., 2020).
## shortcut: any other Bsln+ rise is split at 1.5 mmol/L by that rule, not by a published cut-off
method_aims <- c("First rise (LT1)", "Second rise (LT2)", "Fixed lactate (OBLA)")
method_aim <- function(method) {
  method <- as.character(method)
  rise <- suppressWarnings(as.numeric(sub("^Bsln \\+ ", "", method)))
  first <- method %in% c("Log-log", "LTratio", "LTP1") | (!is.na(rise) & rise < 1.5)
  factor(ifelse(grepl("^OBLA", method), method_aims[3], ifelse(first, method_aims[1], method_aims[2])), method_aims)
}

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
  ## LTratio and IAT share the minimum ratio, found here once (a local copy of the settings: never kept in a result)
  if(any(c("LTratio", "IAT") %in% method)) settings$ltratio <- ltratio_minimum(data, curve, settings)
  results <- lapply(method_categories[method_categories %in% method], function(category) {
    method_function <- switch(
      category,
      "Log-log" = method_loglog,
      "OBLA" = method_obla,
      "Bsln+" = method_bsln_plus,
      "Dmax" = method_dmax,
      "LTP" = method_ltp,
      "LTratio" = method_ltratio,
      "IAT" = method_iat,
      "Stegmann" = method_stegmann
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

#' Bootstrap intervals
#'
#' Wild bootstrap around the user-selected curve: each replicate keeps the fitted values and adds to each stage its
#' own residual, scaled by `1 / sqrt(1 - leverage)` and widened by `qt(level) / qnorm(level)` for the n - p degrees of
#' freedom of the curve, with a random sign (Rademacher weights; Wu, 1986; Davidson & Flachaire, 2008). Residuals never
#' move between stages, because the error of a lactate sample grows with its concentration. The widening brought the
#' coverage of 95% intervals from 75-93% to 84-97% in `data-raw/bootstrap_coverage.R`. Davidson & Flachaire (2008) used
#' residuals divided by `1 - leverage` (HC3), which they found made little difference; in that simulation it gave
#' intervals two to three times too wide for Bsln+ (coverage 95-99%). A resting sample outside the
#' fit gets the residual size of the first stage. Every method is then run again on the replicate. The interval is the
#' percentile interval of the replicate intensities. For internal use only.
#'
#' @inheritParams compute_thresholds
#' @param curve The user-selected curve fitted to the original data.
#' @param n_rows Number of thresholds the methods return.
#' @param n_boot Number of replicates.
#' @param level Confidence level.
#' @param seed A seed for the replicates, or `NULL` to draw from the current random stream.
#'
#' @return a matrix with one row per threshold and the columns `low` and `high`.
#' @keywords internal
bootstrap_intervals <- function(data, settings, method, curve, n_rows, n_boot, level, seed = NULL) {
  if(!is.null(seed)) {
    ## seeded locally: the user's random stream continues afterwards as if nothing was drawn
    saved <- get0(".Random.seed", globalenv(), inherits = FALSE)
    on.exit(if(is.null(saved)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", saved, globalenv()))
    set.seed(seed)
  }
  in_fit <- if(settings$include_baseline) rep(TRUE, nrow(data)) else data$intensity > 0
  fitted <- curve$predict(curve$points$intensity)
  if(any(curve$leverage > 1 - 1e-8)) {
    warning("Too few stages for the chosen fit to estimate confidence intervals.", call. = FALSE)
    return(NULL)
  }
  residuals <- (curve$points$lactate - fitted) / sqrt(1 - curve$leverage)
  ## the noise is estimated from n - p residuals: widen it as a t quantile would a normal one
  df <- length(residuals) - length(curve$coefficients)
  residuals <- residuals * stats::qt(1 - (1 - level) / 2, df) / stats::qnorm(1 - (1 - level) / 2)
  signs <- function(n) sample(c(-1, 1), n, replace = TRUE)
  failed <- 0L

  replicates <- vapply(seq_len(n_boot), function(b) {
    replicate_data <- data
    replicate_data$lactate[in_fit] <- fitted + residuals * signs(sum(in_fit))
    ## shortcut: the resting sample has no residual of its own; the first stage's is the nearest in concentration
    replicate_data$lactate[!in_fit] <- data$lactate[!in_fit] + abs(residuals[1]) * signs(sum(!in_fit))
    ## a lactate value cannot be negative
    replicate_data$lactate <- pmax(replicate_data$lactate, 0.05)
    tryCatch(
      compute_thresholds(replicate_data, settings, method)$rows$intensity,
      error = function(e) {
        failed <<- failed + 1L
        rep(NA_real_, n_rows)
      }
    )
  }, numeric(n_rows))
  if(failed > 0) {
    warning(sprintf("%d of %d bootstrap replicates could not be computed and are left out of the intervals.",
                    failed, n_boot), call. = FALSE)
  }

  alpha <- (1 - level) / 2
  replicates <- matrix(replicates, nrow = n_rows)
  t(apply(replicates, 1, function(r) {
    stats::setNames(stats::quantile(r, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE), c("low", "high"))
  }))
}
