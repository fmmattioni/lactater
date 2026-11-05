## How often the bootstrap intervals of lactate_threshold(ci = TRUE) hold the true threshold.
##
## A known lactate curve (exponential plus constant, a trained cyclist: 8 stages of 25 W from 125 W, 6.8 mmol/L at the
## end) is sampled with analyzer-like noise, the test is analyzed with ci = TRUE, and the interval is checked against
## the threshold of the true curve. Two noise models: 0.1 mmol/L plus 4% of the concentration (error that grows with
## lactate, as in hand-held analyzers; Bonaventura et al., 2015), and a constant 0.25 mmol/L.
##
## Run from the package root: Rscript data-raw/bootstrap_coverage.R (about 5 minutes). The numbers are quoted in the
## Details of lactate_threshold() and in the checking article.
pkgload::load_all(".", quiet = TRUE)

x <- seq(125, 300, by = 25)
truth <- function(v) 0.9 + 0.12 * exp((v - 125) / 45)
chord <- function(v) truth(125) + (truth(300) - truth(125)) / 175 * (v - 125)
true <- c(
  "OBLA 4.0" = uniroot(function(v) truth(v) - 4, c(100, 400))$root,
  "Bsln + 1.0" = uniroot(function(v) truth(v) - (truth(125) + 1), c(100, 400))$root,
  ## Exp-Dmax of the true curve: the line from the first to the last stage
  "Exp-Dmax" = optimize(function(v) chord(v) - truth(v), c(125, 300), maximum = TRUE)$maximum
)

one_test <- function(noise) {
  y <- truth(x)
  y <- if(noise == "grows") y + rnorm(length(y), 0, sqrt(0.1^2 + (0.04 * y)^2)) else y + rnorm(length(y), 0, 0.25)
  test <- data.frame(intensity = x, lactate = pmax(y, 0.3))
  r <- suppressWarnings(lactate_threshold(
    test, intensity, lactate, method = c("OBLA", "Bsln+", "Dmax"), dmax_methods = "Exp-Dmax", fixed = 4,
    baseline_plus = 1, plot = FALSE, ci = TRUE, n_boot = 200
  ))
  i <- match(names(true), as.character(r$method))
  cbind(low = r$intensity_low[i], high = r$intensity_high[i], estimate = attr(r, "lactater")$intensity[names(true)])
}

set.seed(42)
coverage <- do.call(rbind, lapply(c("grows", "constant"), function(noise) {
  runs <- replicate(150, one_test(noise), simplify = FALSE)
  do.call(rbind, lapply(seq_along(true), function(k) {
    low <- vapply(runs, function(r) r[k, "low"], numeric(1))
    high <- vapply(runs, function(r) r[k, "high"], numeric(1))
    estimate <- vapply(runs, function(r) r[k, "estimate"], numeric(1))
    data.frame(
      noise = noise, method = names(true)[k], coverage = mean(low <= true[k] & high >= true[k], na.rm = TRUE),
      median_width = stats::median(high - low, na.rm = TRUE), bias = mean(estimate - true[k], na.rm = TRUE),
      sd = stats::sd(estimate, na.rm = TRUE)
    )
  }))
}))
print(coverage, digits = 3, row.names = FALSE)
