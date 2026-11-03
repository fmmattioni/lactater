## Fits the cycling equations of `predict_performance()`: 45-min time-trial power from lactater's own thresholds,
## in the open data of Heuberger et al. (2018). Run from the package root:
##
##   Rscript data-raw/performance_cycling.R <heuberger_xlsx>
##
## * Heuberger et al. (2018) PLOS ONE doi:10.1371/journal.pone.0206846; data on figshare,
##   doi:10.6084/m9.figshare.7240571, CC BY 4.0. <heuberger_xlsx> is its data.xlsx.
## * 48 well-trained men; step test on a Monark LC4r from 175 W, +25 W every 5 min, venous blood, Lactate Pro 2.
## * Only the visits before study day 0 are used (step test on day -14, 45-min time trial on day -7): the
##   original study was a trial of erythropoietin, and later visits may be affected by it.
##
## It prints the `cycling_equations` table to paste into R/performance.R, and the peak power of the step test as
## a comparison for the article.

pkgload::load_all(quiet = TRUE)
heuberger <- as.data.frame(readxl::read_excel(commandArgs(trailingOnly = TRUE)[1]))

stages <- heuberger[
  grepl("^-14d", heuberger$ProtTime) & grepl("^LactMax[0-9]+$", heuberger$Parameter) &
    is.na(heuberger$Parameter_Notes_N),
]
stages$lactate <- suppressWarnings(as.numeric(stages$Value))
stages <- stages[!is.na(stages$lactate), ]
stages$step <- as.integer(sub("LactMax", "", stages$Parameter))
stages$intensity <- 175 + 25 * (stages$step - 1)
trial <- heuberger[grepl("^-7d", heuberger$ProtTime) & heuberger$Parameter == "Pout_av_tot", ]
trial <- data.frame(subject = trial$SubjectNr, power = suppressWarnings(as.numeric(trial$Value)))

methods <- c("LTP2", "IAT", "ModDmax", "OBLA 4.0", "Dmax", "Log-Poly-ModDmax")
thresholds <- do.call(rbind, lapply(split(stages, stages$SubjectNr), function(s) {
  s <- s[order(s$intensity), c("SubjectNr", "intensity", "lactate")]
  results <- tryCatch(
    lactate_threshold(s, intensity, lactate, plot = FALSE),
    error = function(e) NULL
  )
  value <- rep(NA_real_, length(methods))
  if (!is.null(results)) value <- unname(attr(results, "lactater")$intensity[methods])
  data.frame(subject = s$SubjectNr[1], method = c(methods, "peak"), threshold = c(value, max(s$intensity)))
}))
data <- merge(thresholds, trial, by = "subject")
data <- data[!is.na(data$threshold) & !is.na(data$power), ]

## peak power on the riders with an LTP2, whose tests ran to their last stages with lactate samples
fits <- do.call(rbind, lapply(c(methods, "peak"), function(m) {
  d <- data[data$method == m, ]
  if (m == "peak") d <- d[d$subject %in% data$subject[data$method == "LTP2"], ]
  fit <- stats::lm(power ~ threshold, d)
  press <- stats::residuals(fit) / (1 - stats::hatvalues(fit))
  data.frame(
    method = m, n = nrow(d), intercept = unname(stats::coef(fit)[1]), slope = unname(stats::coef(fit)[2]),
    sigma = summary(fit)$sigma, mean = mean(d$threshold), sd = stats::sd(d$threshold),
    r = stats::cor(d$power, d$threshold), loo = sqrt(mean(press^2)),
    percent = 100 * mean(d$power / d$threshold), percent_sd = 100 * stats::sd(d$power / d$threshold),
    low = min(d$threshold), high = max(d$threshold)
  )
}))
print(fits, digits = 4)
equations <- fits[fits$method != "peak", ]
columns <- vapply(c("method", "n", "intercept", "slope", "sigma", "mean", "sd", "low", "high"), function(column) {
  values <- equations[[column]]
  text <- if (is.character(values)) paste0('"', values, '"') else format(signif(values, 6), trim = TRUE)
  sprintf("  %s = c(%s)", column, paste(text, collapse = ", "))
}, character(1))
cat("\ncycling_equations <- data.frame(\n", paste(columns, collapse = ",\n"), "\n)\n", sep = "")
