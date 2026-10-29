#' Dmax
#'
#' It applies the `Dmax` methods: `"Dmax"`, `"ModDmax"`, `"Exp-Dmax"`, `"Log-Poly-ModDmax"`, and `"Log-Exp-ModDmax"`.
#'
#' Every variant looks for the point on the fitted curve that lies furthest below a straight
#' line between two anchors. The variants differ in the fitted curve and in where the line starts:
#'
#' * Dmax: 3rd degree polynomial, from the first stage.
#' * ModDmax: 3rd degree polynomial, from the stage preceding the first rise of at least
#'   `moddmax_rise` mmol/L.
#' * Exp-Dmax: exponential plus constant, from the first stage.
#' * Log-Poly-ModDmax: 3rd degree polynomial, from the Log-log threshold.
#' * Log-Exp-ModDmax: exponential plus constant, from the Log-log threshold.
#'
#' The line always ends at the last stage. The resting sample is never used.
#'
#' @param data_prepared The data retrieved from `prepare_fit()`.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#' @param loglog_restrainer A scalar from 0 to 1 indicating the percentage of the data that you would like to
#' restrain for fitting the Log-Log method - This is going to be used in the `Log-Poly-ModDmax` and `Log-Exp-ModDmax` methods only.
#' For example, `1` means no restriction (fits using the whole data), and `0.5` means that
#' only the first 50% of the data will be used. Default to `1`.
#' @param plot A boolean to indicate whether to generate a plot from each one of the methods.
#' @param dmax_anchor Where the line's ends sit: `"fitted"` (on the fitted curve) or `"observed"`
#' (at the measured lactate of the start and end stages). The Log-log start has no measured value,
#' so it always sits on the curve.
#' @param moddmax_rise The rise in lactate (mmol/L) between two consecutive stages that marks the
#' first rise in ModDmax. Default to `0.4`.
#'
#' @return a tibble with the following columns:
#' \item{method}{the method used to estimate the lactate threshold}
#' \item{fitting}{the fitting method used to predict the lactate curve}
#' \item{intensity}{the intensity associated with the estimated lactate threshold}
#' \item{lactate}{the lactate concentration associated with the estimated lactate threshold}
#' \item{plot}{the plot produced to display the lactate threshold}
#'
#' @keywords internal
method_dmax <- function(data_prepared, sport, loglog_restrainer = 1, plot, dmax_anchor = "fitted", moddmax_rise = 0.4) {
  data_raw <- data_prepared$data[[1]]
  fit_poly <- prepare_fit(.data = data_raw, fit = "3rd degree polynomial", include_baseline = FALSE, sport = sport)
  fit_exp <- prepare_fit(.data = data_raw, fit = "Exponential", include_baseline = FALSE, sport = sport)

  stages <- data_raw[data_raw$intensity > 0, ]
  n <- nrow(stages)
  ## tolerance: 1.4 - 1.0 is 0.3999... in floating point
  first_rise <- which(diff(stages$lactate) >= moddmax_rise - 1e-9)[1]
  loglog_intensity <- loglog_breakpoint(stages, loglog_restrainer)$intensity

  variants <- list(
    "Dmax" = list(fit = fit_poly, x0 = stages$intensity[1], y0 = stages$lactate[1]),
    "ModDmax" = list(fit = fit_poly, x0 = stages$intensity[first_rise], y0 = stages$lactate[first_rise]),
    "Exp-Dmax" = list(fit = fit_exp, x0 = stages$intensity[1], y0 = stages$lactate[1]),
    "Log-Poly-ModDmax" = list(fit = fit_poly, x0 = loglog_intensity, y0 = NA_real_),
    "Log-Exp-ModDmax" = list(fit = fit_exp, x0 = loglog_intensity, y0 = NA_real_)
  )

  out <- lapply(names(variants), function(method) {
    v <- variants[[method]]
    model <- v$fit$model[[1]]
    curve <- function(x) unname(stats::predict(model, newdata = data.frame(intensity = x)))
    x0 <- v$x0
    x1 <- stages$intensity[n]
    observed <- dmax_anchor == "observed" && !is.na(v$y0)
    y0 <- if (is.na(x0)) NA_real_ else if (observed) v$y0 else curve(x0)
    y1 <- if (dmax_anchor == "observed") stages$lactate[n] else curve(x1)
    intensity <- dmax_point(curve, x0, y0, x1, y1)

    v$fit %>%
      dplyr::mutate(
        data_augmented = list(dplyr::filter(data_augmented[[1]], is.na(x0) | intensity >= x0)),
        method = method,
        fitting = if (inherits(model, "nls")) "Exponential (default)" else "3rd degree polynomial (default)",
        intensity = intensity,
        lactate = if (is.na(intensity)) NA_real_ else round(curve(intensity), 1),
        data_plot_line = list(data.frame(intensity = c(x0, x1), lactate = c(y0, y1)))
      )
  }) %>%
    dplyr::bind_rows()

  if(plot) {
    out$plot <- lapply(seq_len(nrow(out)), function(i) suppressWarnings(plot_lactate(data_processed = out[i, ], method = out$method[i])))
  }

  if(sport == "swimming") {
    out <- out %>%
      dplyr::mutate(pace = convert_to_pace(speed = intensity))
  }

  out <- out %>%
    dplyr::select(dplyr::any_of(c("method", "fitting", "intensity", "pace", "lactate", "heart_rate", "plot"))) %>%
    dplyr::mutate(
      method = factor(method, levels = names(variants)),
      intensity = round(intensity, digits = switch(sport, cycling = 1, running = 2, swimming = 3))
    )

  out
}

#' Dmax point
#'
#' Finds where the curve lies furthest below the straight line from `(x0, y0)` to `(x1, y1)`.
#' The perpendicular distance is the vertical distance times a constant, so both peak at the
#' same intensity. For internal use only.
#'
#' @param curve A function returning the fitted lactate at given intensities.
#' @param x0,y0 The start of the line.
#' @param x1,y1 The end of the line.
#'
#' @return the intensity, or `NA` when the curve never dips below the line.
#' @keywords internal
dmax_point <- function(curve, x0, y0, x1, y1) {
  if (anyNA(c(x0, y0, x1, y1)) || x1 <= x0) return(NA_real_)
  below_line <- function(x) y0 + (y1 - y0) / (x1 - x0) * (x - x0) - curve(x)
  grid <- seq(x0, x1, length.out = 1001)
  best <- which.max(below_line(grid))
  if (below_line(grid[best]) <= 0) return(NA_real_)
  stats::optimize(
    below_line,
    interval = grid[c(max(best - 1, 1), min(best + 1, length(grid)))],
    maximum = TRUE,
    tol = 1e-8
  )$maximum
}
