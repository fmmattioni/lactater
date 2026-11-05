#' Maximal lactate steady state from constant-load bouts
#'
#' Finds the maximal lactate steady state (MLSS) from two or more constant-load bouts of at least 30 min on separate
#' days: a bout is steady when blood lactate rises by no more than 1.0 mmol/L from minute 10 to minute 30, and the MLSS
#' is the highest steady bout. This is the criterion of Heck et al. (1985), who introduced the test and the term, and of
#' Beneke (2003). Against the highest steady oxygen uptake, a window from minute 15 or 20 to minute 30 was more accurate
#' (Iannetta et al., 2022); set it with `from`. With the result of [lactate_threshold()] for a step test of the same
#' athlete, it also gives how far each threshold lies from the MLSS, your own check of which threshold estimates it.
#'
#' The lactate at minute `from` and at minute 30 is interpolated linearly between the samples, so sample at or around
#' both. A bout
#' that ended before minute 30 (exhaustion) is not steady. The true MLSS lies between the highest steady bout and the
#' next bout up, so bouts closer together pin it down better. Thresholds against a measured MLSS land from about 40 W
#' below to 40 W above it on average, depending on the method and the protocol (Hauser et al., 2014; Ploszczyca et al.,
#' 2020). Whether the MLSS or the critical power marks the boundary between the heavy and severe domains is debated
#' (Keir et al., 2015; Mattioni Maturana et al., 2016; Jones et al., 2019).
#'
#' @param bouts A data frame with one row per sample: `intensity` (the load of the bout, in the unit of the step
#'   test), `time` (minutes from the start of the bout) and `lactate` (mmol/L).
#' @param x The result of [lactate_threshold()] for a step test of the same athlete, or `NULL`.
#' @param rise The largest rise in lactate (mmol/L) from minute `from` to minute 30 of a steady bout. Default to `1`.
#' @param from The minute the rise is measured from, to minute 30. Default to `10` (Heck et al., 1985; Beneke, 2003);
#'   `15` or `20` agreed best with the highest steady oxygen uptake in Iannetta et al. (2022).
#'
#' @return a `lactate_mlss` list with `mlss` (the highest steady bout, `NA` when none was steady), `next_bout` (the
#'   lowest bout above it, which was not steady, or `NA`), `bouts` (a tibble with one row per bout: `intensity`,
#'   `lactate_from` (at minute `from`), `lactate_30`, `rise`, `minutes` (the last sample), and `steady`), the `samples`,
#'   `rise` and `from`, and, with `x`,
#'   `thresholds` (a tibble with `method`, `intensity`, and the `gap` to the MLSS in intensity units and in %, positive
#'   when the threshold lies above it). Plot it with [autoplot()]; the article [Beyond the step
#'   test](https://fmmattioni.github.io/lactater/articles/beyond-the-step-test.html) shows it.
#'
#' @references
#' Beneke R. Methodological aspects of maximal lactate steady state - implications for performance testing. European
#' Journal of Applied Physiology. 2003;89(1):95-99. doi:10.1007/s00421-002-0783-1
#'
#' Hauser T, Adam J, Schulz H. Comparison of selected lactate threshold parameters with maximal lactate steady state in
#' cycling. International Journal of Sports Medicine. 2014;35(6):517-521.
#'
#' Heck H, Mader A, Hess G, Mücke S, Müller R, Hollmann W. Justification of the 4-mmol/l lactate threshold.
#' International Journal of Sports Medicine. 1985;6(3):117-130. doi:10.1055/s-2008-1025824
#'
#' Iannetta D, Ingram CP, Keir DA, Murias JM. Methodological reconciliation of CP and MLSS and their agreement with the
#' maximal metabolic steady state. Medicine & Science in Sports & Exercise. 2022;54(4):622-632.
#' doi:10.1249/MSS.0000000000002831
#'
#' Jones AM, Burnley M, Black MI, Poole DC, Vanhatalo A. The maximal metabolic steady state: redefining the 'gold
#' standard'. Physiological Reports. 2019;7(10):e14098.
#'
#' Keir DA, Fontana FY, Robertson TC, Murias JM, Paterson DH, Kowalchuk JM, Pogliaghi S. Exercise intensity thresholds:
#' identifying the boundaries of sustainable performance. Medicine & Science in Sports & Exercise. 2015;47(9):1932-1940.
#' doi:10.1249/MSS.0000000000000613
#'
#' Mattioni Maturana F, Keir DA, McLay KM, Murias JM. Can measures of critical power precisely estimate the maximal
#' metabolic steady-state? Applied Physiology, Nutrition, and Metabolism. 2016;41(11):1197-1203.
#' doi:10.1139/apnm-2016-0248
#'
#' Ploszczyca K, Jazic D, Piotrowicz Z, Chalimoniuk M, Langfort J, Czuba M. Comparison of maximal lactate steady state
#' with anaerobic threshold determined by various methods based on graded exercise test with 3-minute stages in elite
#' cyclists. BMC Sports Science, Medicine and Rehabilitation. 2020;12:70.
#'
#' @export
#'
#' @examples
#' bouts <- data.frame(
#'   intensity = rep(c(130, 140, 150), each = 4),
#'   time = rep(c(5, 10, 20, 30), 3),
#'   lactate = c(2.4, 2.6, 2.7, 2.8, 3.1, 3.4, 3.8, 4.1, 3.9, 4.5, 5.4, 6.3)
#' )
#' results <- lactate_threshold(demo_data, intensity, lactate, plot = FALSE)
#' mlss <- lactate_mlss(bouts, results)
#' mlss
#' mlss$thresholds
#' ## the plot is short and wide: see it in the article Beyond the step test
#' if(interactive()) autoplot(mlss)
lactate_mlss <- function(bouts, x = NULL, rise = 1, from = 10) {
  if(!is.data.frame(bouts) || !all(c("intensity", "time", "lactate") %in% names(bouts)))
    stop("`bouts` must be a data frame with the columns `intensity`, `time` (min) and `lactate`.", call. = FALSE)
  if(!is.numeric(rise) || length(rise) != 1 || is.na(rise) || rise <= 0)
    stop("`rise` must be a single positive number (mmol/L).", call. = FALSE)
  if(!is.numeric(from) || length(from) != 1 || is.na(from) || from < 0 || from >= 30)
    stop("`from` must be a single minute from 0 to below 30.", call. = FALSE)
  bouts <- data.frame(
    intensity = as.numeric(bouts[["intensity"]]), time = as.numeric(bouts[["time"]]),
    lactate = as.numeric(bouts[["lactate"]])
  )
  bouts <- bouts[stats::complete.cases(bouts), ]
  if(nrow(bouts) == 0) stop("`bouts` has no complete samples.", call. = FALSE)

  rows <- lapply(split(bouts, bouts$intensity), function(bout) {
    bout <- bout[order(bout$time), ]
    ## approx() gives NA outside the samples, so a bout without a sample at or after minute 30 has no lactate_30
    at <- function(minute) {
      if(nrow(bout) < 2) return(if(any(bout$time == minute)) bout$lactate[bout$time == minute][1] else NA_real_)
      stats::approx(bout$time, bout$lactate, minute, ties = mean)$y
    }
    data.frame(
      intensity = bout$intensity[1], lactate_from = at(from), lactate_30 = at(30), minutes = max(bout$time)
    )
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  out$rise <- out$lactate_30 - out$lactate_from
  out$steady <- !is.na(out$rise) & out$rise <= rise + 1e-9 & out$minutes >= 30
  ## rounded for show only, after the verdict
  out[c("lactate_from", "lactate_30", "rise")] <- lapply(out[c("lactate_from", "lactate_30", "rise")], round, 2)
  out <- out[order(out$intensity), c("intensity", "lactate_from", "lactate_30", "rise", "minutes", "steady")]

  mlss <- if(any(out$steady)) max(out$intensity[out$steady]) else NA_real_
  above <- out$intensity[out$intensity > if(is.na(mlss)) -Inf else mlss]
  if(any(!out$steady & out$intensity < if(is.na(mlss)) -Inf else mlss)) {
    warning("A bout below the MLSS was not steady: check its samples.", call. = FALSE)
  }

  result <- list(
    mlss = mlss, next_bout = if(length(above) > 0) min(above) else NA_real_, bouts = out, samples = bouts, rise = rise,
    from = from, sport = NULL
  )
  if(!is.null(x)) {
    info <- lactater_info(x)
    result$sport <- info$settings$sport
    ## the MLSS lies inside a step test's range; bouts far outside it are most likely in another unit (m/s vs km/h)
    stages <- range(info$data$intensity[info$data$intensity > 0])
    if(any(out$intensity < stages[1] / 2 | out$intensity > stages[2] * 1.5)) {
      warning(sprintf(
        "The bouts (%g to %g) lie far outside the step test (%g to %g %s): are they in the same unit?",
        min(out$intensity), max(out$intensity), stages[1], stages[2], intensity_unit(result$sport)
      ), call. = FALSE)
    }
    ## named apart from the column: tibble() would compute the gaps from the rounded intensity
    value <- unname(info$intensity[as.character(x$method)])
    result$thresholds <- tibble::tibble(
      method = as.character(x$method), intensity = round(value, intensity_digits(result$sport)),
      gap = round(value - mlss, intensity_digits(result$sport)), gap_percent = round(100 * (value / mlss - 1), 1)
    )
  }
  class(result) <- "lactate_mlss"
  result
}

#' @export
print.lactate_mlss <- function(x, ...) {
  unit <- if(is.null(x$sport)) "" else paste0(" ", intensity_unit(x$sport))
  cat(sprintf("%d constant-load bout%s, steady when lactate rose by %g mmol/L or less from minute %g to 30.\n",
              nrow(x$bouts), if(nrow(x$bouts) == 1) "" else "s", x$rise, x$from))
  cat(if(is.na(x$mlss)) {
    "No bout was steady: the MLSS lies below the lowest bout.\n"
  } else if(is.na(x$next_bout)) {
    sprintf("MLSS at %g%s or above: no bout above it was tested.\n", x$mlss, unit)
  } else {
    sprintf("MLSS at %g%s: the next bout (%g%s) was not steady.\n", x$mlss, unit, x$next_bout, unit)
  })
  if(!is.null(x$thresholds) && !is.na(x$mlss)) {
    closest <- x$thresholds[order(abs(x$thresholds$gap)), ][seq_len(min(3, nrow(x$thresholds))), ]
    cat(sprintf("Closest thresholds: %s.\n", paste0(
      closest$method, " (", sprintf("%+g", closest$gap), unit, ")", collapse = ", "
    )))
  }
  invisible(x)
}

#' @rdname lactate_mlss
#' @param object The result of [lactate_mlss()].
#' @param ... Not used.
#' @export
autoplot.lactate_mlss <- function(object, ...) {
  colours <- lactater_colours()
  out <- object$bouts
  unit <- if(is.null(object$sport)) "" else paste0(" ", intensity_unit(object$sport))
  samples <- object$samples
  samples$steady <- out$steady[match(samples$intensity, out$intensity)]
  ends <- do.call(rbind, lapply(split(samples, samples$intensity), function(d) d[which.max(d$time), ]))
  ends$label <- ifelse(
    is.na(out$rise[match(ends$intensity, out$intensity)]), sprintf("%g%s", ends$intensity, unit),
    sprintf("%g%s  %+.1f", ends$intensity, unit, out$rise[match(ends$intensity, out$intensity)])
  )
  ink <- function(steady) ifelse(steady, colours[["accent"]], colours[["muted"]])

  ggplot2::ggplot(samples, ggplot2::aes(time, lactate, group = intensity)) +
    ## the window that decides the bout
    ggplot2::annotate("rect", xmin = object$from, xmax = 30, ymin = -Inf, ymax = Inf, fill = colours[["faint"]],
                      alpha = 0.2) +
    ggplot2::geom_line(colour = ink(samples$steady), linewidth = 0.8) +
    ggplot2::geom_point(shape = 21, size = 2.2, stroke = 0.7, fill = colours[["paper"]], colour = ink(samples$steady)) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), ends, hjust = -0.12, size = 3, colour = ink(ends$steady)
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.3))) +
    ggplot2::labs(
      title = if(is.na(object$mlss)) "No steady bout" else sprintf("MLSS at %g%s", object$mlss, unit),
      subtitle = sprintf(
        "Steady bouts in green: lactate rose by %g mmol/L or less from minute %g to 30 (shaded)",
        object$rise, object$from
      ),
      x = "Minute of the bout", y = "Blood lactate (mmol/L)"
    ) +
    theme_lactater()
}
