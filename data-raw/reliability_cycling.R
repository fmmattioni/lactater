## Test-retest coefficient of variation of every threshold, for `compare_tests()`: lactater's own thresholds on the
## five step tests of each rider in the open data of Heuberger et al. (2018). Run from the package root:
##
##   Rscript data-raw/reliability_cycling.R <heuberger_xlsx>
##
## * Heuberger et al. (2018) PLOS ONE doi:10.1371/journal.pone.0206846; data on figshare,
##   doi:10.6084/m9.figshare.7240571, CC BY 4.0. <heuberger_xlsx> is its "CHDR1514 - Data Repository.xlsx".
## * 48 well-trained men; step test on a Monark LC4r from 175 W, +25 W every 5 min, venous blood, Lactate Pro 2;
##   five tests about two weeks apart (study days -14, 11, 25, 39 and 53).
## * The study was a trial of erythropoietin: half of the riders received it from day 0. Heuberger et al. computed
##   their repeatability on the placebo riders only, but the open data do not say who they are, so every rider is
##   used here. `lactate_reliability()` takes out the mean change from test to test, not a change that differs
##   between the two groups, so these values can only be too high. The script prints them beside the placebo-only
##   values of Heuberger et al. (their Table 2) for the methods both have, as the check of how much.
##
## It prints the `method_cv` vector to paste into R/compare.R.

pkgload::load_all(quiet = TRUE)
heuberger <- as.data.frame(readxl::read_excel(commandArgs(trailingOnly = TRUE)[1]))

stages <- heuberger[grepl("^LactMax[0-9]+$", heuberger$Parameter) & is.na(heuberger$Parameter_Notes_N), ]
stages$lactate <- suppressWarnings(as.numeric(stages$Value))
stages <- stages[!is.na(stages$lactate), ]
stages$intensity <- 175 + 25 * (as.integer(sub("LactMax", "", stages$Parameter)) - 1)
stages$day <- as.integer(sub("d.*", "", stages$ProtTime))

thresholds <- do.call(rbind, lapply(split(stages, list(stages$SubjectNr, stages$day), drop = TRUE), function(s) {
  s <- s[order(s$intensity), ]
  results <- tryCatch(lactate_threshold(s, intensity, lactate, plot = FALSE), error = function(e) NULL)
  if (is.null(results)) return(NULL)
  data.frame(subject = s$SubjectNr[1], day = s$day[1], method = as.character(results$method),
             intensity = unname(attr(results, "lactater")$intensity[as.character(results$method)]))
}))
cat(sprintf("%d tests of %d riders analyzed\n", nrow(unique(thresholds[c("subject", "day")])),
            length(unique(thresholds$subject))))

reliability <- as.data.frame(lactate_reliability(thresholds, "subject", "day"))
## Heuberger et al. (2018), Table 2: placebo riders only, their own implementation of each concept
reliability$heuberger <- c("IAT" = 3.6, "OBLA 4.0" = 3.8, "Dmax" = 3.4, "ModDmax" = 4.3)[reliability$method]
print(reliability, row.names = FALSE)

cat("\nmethod_cv <- c(\n", paste(sprintf('  "%s" = %.1f', reliability$method, reliability$cv), collapse = ",\n"),
    "\n)\n", sep = "")
