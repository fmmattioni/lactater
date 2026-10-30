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
#' * LTP1-Poly-ModDmax: 3rd degree polynomial, from LTP1 (only when requested in `dmax_methods`).
#' * Log-Poly-ModDmax: 3rd degree polynomial, from the Log-log threshold.
#' * Log-Exp-ModDmax: exponential plus constant, from the Log-log threshold.
#'
#' The line always ends at the last stage. The resting sample is never used. With
#' `settings$dmax_anchor = "observed"` the line's ends sit at the measured lactate of the start
#' and end stages instead of on the fitted curve; the Log-log and LTP1 starts have no measured
#' value, so they always sit on the curve. `"original"` follows each paper: on the curve for Dmax
#' and Exp-Dmax, at the measured values for the ModDmax variants.
#'
#' @inheritParams method_obla
#'
#' @return a list with the result `rows` (method, fitting, intensity, lactate) and the `plot_data`
#' for each row.
#' @keywords internal
method_dmax <- function(data, curve, settings) {
  stages <- data[data$intensity > 0, ]
  n <- nrow(stages)
  poly <- fit_curve(stages, "3rd degree polynomial")
  expo <- fit_curve(stages, "Exponential")

  ## tolerance: 1.4 - 1.0 is 0.3999... in floating point
  first_rise <- which(diff(stages$lactate) >= settings$moddmax_rise - 1e-9)[1]
  wanted <- settings$dmax_methods
  loglog_intensity <- if(any(grepl("^Log-", wanted))) loglog_breakpoint(stages, settings$loglog_restrainer)$intensity
  ltp1_intensity <- if("LTP1-Poly-ModDmax" %in% wanted) {
    fit_breakpoints(
      stages$intensity, stages$lactate,
      n_breakpoints = settings$ltp_breakpoints, flat_first = settings$ltp_fix_first_slope
    )$psi[1]
  }

  ## original: "the two end points of the curve" (Cheng et al., 1992); ModDmax from "the LT and the final
  ## lactate point" (Bishop et al., 1998); Log-*-ModDmax to "the final lactate point" (Jamnick et al., 2018)
  variants <- list(
    "Dmax" = list(curve = poly, x0 = stages$intensity[1], y0 = stages$lactate[1], original = "fitted"),
    "ModDmax" = list(
      curve = poly, x0 = stages$intensity[first_rise], y0 = stages$lactate[first_rise], original = "observed"
    ),
    "Exp-Dmax" = list(curve = expo, x0 = stages$intensity[1], y0 = stages$lactate[1], original = "fitted"),
    "LTP1-Poly-ModDmax" = list(curve = poly, x0 = ltp1_intensity, y0 = NA_real_, original = "observed"),
    "Log-Poly-ModDmax" = list(curve = poly, x0 = loglog_intensity, y0 = NA_real_, original = "observed"),
    "Log-Exp-ModDmax" = list(curve = expo, x0 = loglog_intensity, y0 = NA_real_, original = "observed")
  )[wanted]

  results <- lapply(variants, function(v) {
    x0 <- v$x0
    x1 <- stages$intensity[n]
    anchor <- if(settings$dmax_anchor == "original") v$original else settings$dmax_anchor
    observed <- anchor == "observed"
    y0 <- if(is.na(x0)) NA_real_ else if(observed && !is.na(v$y0)) v$y0 else v$curve$predict(x0)
    y1 <- if(observed) stages$lactate[n] else v$curve$predict(x1)
    intensity <- dmax_point(v$curve$predict, x0, y0, x1, y1)
    list(
      intensity = intensity,
      lactate = if(is.na(intensity)) NA_real_ else retrieve_lactate(v$curve, intensity),
      plot_data = list(
        curve = curve_points(v$curve, from = if(is.na(x0)) v$curve$domain[1] else x0),
        line = data.frame(intensity = c(x0, x1), lactate = c(y0, y1)),
        fitting = v$curve$fit
      )
    )
  })

  method_result(
    method = names(variants),
    fitting = paste(vapply(variants, function(v) v$curve$fit, ""), "(default)"),
    intensity = vapply(results, `[[`, numeric(1), "intensity"),
    lactate = vapply(results, `[[`, numeric(1), "lactate"),
    plot_data = unname(lapply(results, `[[`, "plot_data"))
  )
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
