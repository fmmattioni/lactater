#' Check the lactate curve and how much each threshold depends on single stages
#'
#' Regression diagnostics for the fitted lactate curve, adapted to tests of 6 to 10 stages, and a leave-one-out check
#' of every threshold. Print it for a short summary, and plot it with [autoplot()] for six panels, each with a line on
#' what to look for.
#'
#' * **Residuals**: the measured lactate minus the curve. A run of residuals of the same sign, or an arch against the
#'   fitted values, means the shape of the curve is wrong for this test.
#' * **Standardized residuals**: residuals divided by their standard error. With so few stages, normality and
#'   equal-variance tests have almost no power, so a check that "passes" means "could not tell", not "fine"; analyser
#'   error also grows with lactate (Bonaventura et al., 2015).
#' * **Leverage and Cook's distance**: how much one stage pulls the curve (Cook, 1977). The first and last stages always
#'   have high leverage, by design. A stage is flagged when its Cook's distance is above the median of the
#'   F(p, n - p) distribution, where p is the number of curve terms; rules such as 2p/n for leverage cannot flag
#'   anything when p is close to n.
#' * **Leave one stage out**: every threshold is computed again without each stage in turn (a jackknife; Efron, 1982).
#'   The range shows how much a threshold rests on one stage, and the jackknife standard error is
#'   `sqrt((n - 1) / n * sum((theta_i - mean(theta_i))^2))`. Dropping the first or last stage changes the Dmax line
#'   itself, so large changes there are expected for the Dmax family.
#' * **Which curve predicts best**: the leave-one-out prediction error of every curve type, from
#'   [lactate_fit_quality()]. It rewards curves that follow the data without overfitting it, which the RMSE of the fit
#'   alone cannot do.
#'
#' @param x The result of [lactate_threshold()].
#'
#' @return a `lactate_diagnostics` object: a list with `stages` (one row per stage used in the fit: `intensity`,
#' `lactate`, `fitted`, `residual`, `standardized`, `leverage`, `cooks_distance`, `influential`), `leave_one_out`
#' (`method`, `stage` left out, `intensity` without it, `change`), `jackknife` (`method`, `intensity`, `lowest`,
#' `highest`, `jackknife_se`), and `fits` ([lactate_fit_quality()], best prediction error first).
#'
#' @references
#' Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ. Reliability and accuracy of six hand-held blood
#' lactate analysers. Journal of Sports Science and Medicine. 2015;14:203-214.
#'
#' Cook RD. Detection of influential observation in linear regression. Technometrics. 1977;19(1):15-18.
#'
#' Efron B. The jackknife, the bootstrap and other resampling plans. Philadelphia: SIAM; 1982.
#'
#' @export
#'
#' @examples
#' female <- subset(lactate_tests, test == "cycling_female")
#' results <- lactate_threshold(female, intensity, lactate, heart_rate, plot = FALSE)
#' diagnostics <- lactate_diagnostics(results)
#' diagnostics
#' autoplot(diagnostics)
lactate_diagnostics <- function(x) {
  info <- lactater_info(x)
  curve <- info$curve
  points <- curve$points

  fitted <- curve$predict(points$intensity)
  residual <- points$lactate - fitted
  n <- length(residual)
  p <- length(curve$coefficients)
  sigma <- sqrt(sum(residual^2) / (n - p))
  leverage <- curve$leverage
  standardized <- residual / (sigma * sqrt(1 - leverage))
  cooks <- standardized^2 / p * leverage / (1 - leverage)
  cut <- stats::qf(0.5, p, n - p)
  stages <- tibble::tibble(
    intensity = points$intensity, lactate = points$lactate, fitted = fitted, residual = residual,
    standardized = standardized, leverage = leverage, cooks_distance = cooks, influential = cooks > cut
  )

  ## leave each exercise stage out and run every method again
  methods <- as.character(x$method)
  categories <- unique(as.character(x$method_category))
  exercise <- which(info$data$intensity > 0)
  loo <- do.call(rbind, lapply(exercise, function(k) {
    found <- tryCatch(
      compute_thresholds(info$data[-k, ], info$settings, categories)$rows,
      error = function(e) NULL
    )
    without <- if(is.null(found)) rep(NA_real_, length(methods)) else found$intensity[match(methods, found$method)]
    data.frame(method = methods, stage = info$data$intensity[k], intensity = without)
  }))
  loo$change <- loo$intensity - unname(info$intensity[loo$method])
  jackknife <- do.call(rbind, lapply(methods, function(m) {
    values <- loo$intensity[loo$method == m]
    values <- values[!is.na(values)]
    data.frame(
      method = m, intensity = unname(info$intensity[[m]]),
      lowest = if(length(values)) min(values) else NA_real_, highest = if(length(values)) max(values) else NA_real_,
      jackknife_se = if(length(values) > 1) {
        sqrt((length(values) - 1) / length(values) * sum((values - mean(values))^2))
      } else {
        NA_real_
      }
    )
  }))

  fits <- lactate_fit_quality(x)

  structure(
    list(
      stages = stages, leave_one_out = tibble::as_tibble(loo), jackknife = tibble::as_tibble(jackknife),
      fits = fits[order(fits$prediction_error), ],
      cut = cut, terms = p, info = info
    ),
    class = "lactate_diagnostics"
  )
}

#' @export
print.lactate_diagnostics <- function(x, ...) {
  sport <- x$info$settings$sport
  unit <- intensity_unit(sport)
  cat(sprintf(
    "Lactate curve: %s on %d points; residual SD %.2f mmol/L.\n", x$info$settings$fit, nrow(x$stages),
    sqrt(sum(x$stages$residual^2) / (nrow(x$stages) - x$terms))
  ))
  influential <- x$stages[x$stages$influential, ]
  cat(if(nrow(influential) == 0) {
    sprintf("No stage has a Cook's distance above %.2f (the median of F(%d, %d)).\n", x$cut, x$terms,
            nrow(x$stages) - x$terms)
  } else {
    sprintf("Influential stage(s): %s.\n", paste0(
      format_intensity(influential$intensity, sport), " ", unit,
      " (Cook's distance ", sprintf("%.2f", influential$cooks_distance), ")", collapse = ", "
    ))
  })
  spread <- x$jackknife[!is.na(x$jackknife$jackknife_se), ]
  spread <- spread[order(-spread$jackknife_se), ]
  cat(sprintf(
    "Most sensitive to single stages: %s.\n",
    paste0(utils_head(spread$method, 3), " (jackknife SE ", format_intensity(utils_head(spread$jackknife_se, 3), sport),
           " ", unit, ")", collapse = ", ")
  ))
  cat(sprintf(
    "Best-predicting curve (leave one out): %s, %.2f mmol/L.\n", x$fits$fit[1], x$fits$prediction_error[1]
  ))
  invisible(x)
}

#' @rdname lactate_diagnostics
#' @param object The result of [lactate_diagnostics()].
#' @param ... Not used.
#' @export
autoplot.lactate_diagnostics <- function(object, ...) {
  colours <- lactater_colours()
  info <- object$info
  sport <- info$settings$sport
  stages <- as.data.frame(object$stages)
  dot <- function(...) {
    ggplot2::geom_point(
      ..., shape = 21, size = 2.6, stroke = 0.7, fill = colours[["paper"]], colour = colours[["curve"]]
    )
  }
  reference <- function(...) ggplot2::geom_hline(..., colour = colours[["accent"]], linewidth = 0.6, linetype = "22")
  panel <- function(title, subtitle) {
    list(ggplot2::labs(title = title, subtitle = subtitle), theme_lactater(),
         ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.05))))
  }

  fit <- ggplot2::ggplot(stages, ggplot2::aes(intensity, lactate)) +
    ggplot2::geom_line(ggplot2::aes(intensity, lactate), curve_points(info$curve), colour = colours[["curve"]],
                       linewidth = 0.8) +
    ggplot2::geom_segment(ggplot2::aes(xend = intensity, yend = fitted), colour = colours[["muted"]], linewidth = 0.3,
                          linetype = "22") +
    dot() +
    ggplot2::labs(x = intensity_label(sport), y = "Lactate (mmol/L)") +
    panel("Fit to the stages", "Dots should sit close to the curve,\nwith no run of dots on one side")
  residuals <- ggplot2::ggplot(stages, ggplot2::aes(fitted, residual)) +
    reference(yintercept = 0) +
    dot() +
    ggplot2::labs(x = "Fitted lactate (mmol/L)", y = "Residual (mmol/L)") +
    panel("Residuals vs fitted", "Dots should scatter around zero;\nan arch means the curve has the wrong shape")
  quantiles <- stats::qqnorm(stages$standardized, plot.it = FALSE)
  normal <- ggplot2::ggplot(data.frame(x = quantiles$x, y = quantiles$y), ggplot2::aes(x, y)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = colours[["accent"]], linewidth = 0.6, linetype = "22") +
    dot() +
    ggplot2::labs(x = "Theoretical quantiles", y = "Standardized residuals") +
    panel("Normality of residuals", "Dots should follow the line; with so few\nstages this check has little power")
  influence <- ggplot2::ggplot(stages, ggplot2::aes(intensity, cooks_distance)) +
    reference(yintercept = object$cut) +
    ggplot2::geom_segment(ggplot2::aes(xend = intensity, y = 0, yend = cooks_distance), colour = colours[["muted"]],
                          linewidth = 0.5) +
    dot() +
    ggplot2::labs(x = intensity_label(sport), y = "Cook's distance") +
    panel("Influential stages", "Stages above the line pull the curve;\ncheck them for sampling errors")

  loo <- as.data.frame(object$leave_one_out)
  loo <- loo[!is.na(loo$change), ]
  loo$method <- factor(loo$method, levels = rev(unique(as.character(object$jackknife$method))))
  ## only the five largest changes are named, on the inner side of the dot, so the panel stays readable
  largest <- loo[utils_head(order(-abs(loo$change)), 5), ]
  sensitivity <- ggplot2::ggplot(loo, ggplot2::aes(change, method)) +
    ggplot2::geom_vline(xintercept = 0, colour = colours[["accent"]], linewidth = 0.6, linetype = "22") +
    dot() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste("without", format_intensity(stage, sport)), hjust = ifelse(change < 0, -0.2, 1.2)),
      largest, size = 2.6, colour = colours[["muted"]]
    ) +
    ggplot2::labs(x = paste0("Change without one stage (", intensity_unit(sport), ")"), y = NULL) +
    panel("Leave one stage out",
          "Each dot: the threshold without one stage;\na wide spread means it rests on one stage")
  fits <- as.data.frame(object$fits)
  fits$fit <- factor(fits$fit, levels = rev(fits$fit))
  prediction <- ggplot2::ggplot(fits, ggplot2::aes(prediction_error, fit)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = prediction_error, yend = fit), colour = colours[["muted"]],
                          linewidth = 0.5) +
    dot() +
    ggplot2::labs(x = "Prediction error (mmol/L)", y = NULL) +
    panel("Which curve predicts best", "Each stage predicted by a curve fitted\nwithout it; lower is better")

  patchwork::wrap_plots(fit, residuals, normal, influence, sensitivity, prediction, ncol = 2,
                        heights = c(1, 1, max(1.2, length(unique(loo$method)) / 10))) +
    patchwork::plot_annotation(
      title = "Checking the lactate curve",
      subtitle = sprintf("%s on %d points", info$settings$fit, nrow(stages)),
      theme = theme_lactater()
    )
}
