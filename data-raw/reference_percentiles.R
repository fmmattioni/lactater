## Builds `reference_power` in R/sysdata.rda: the percentiles of power at 3 mmol/L (LT2fix) and at baseline + 1 mmol/L
## (LT2ind) on the cycle ergometer, by sex and age (14-64), from Fiedler et al. (2025), J Sports Sci 43(16):1654-64,
## doi:10.1080/02640414.2025.2517913, CC BY 4.0. Run from the package root:
##
##   Rscript data-raw/reference_percentiles.R <folder with the supplementary files rjsp_a_2517913_sm*.xlsx>
##
## The eight supplementary files carry no titles. Which curve each one holds was found by matching their 25th, 50th
## and 75th percentiles, averaged over ages 14-18 and 59-64, against Tables 2 and 3 of the paper (best match within
## 1.5%, the next one 7% or more away); the check below repeats that match for the medians.

args <- commandArgs(trailingOnly = TRUE)
files <- c(
  sm4859 = "OBLA 3.0|W|male", sm4856 = "OBLA 3.0|W|female",
  sm4858 = "OBLA 3.0|W/kg|male", sm4861 = "OBLA 3.0|W/kg|female",
  sm4862 = "Bsln + 1.0|W|male", sm4863 = "Bsln + 1.0|W|female",
  sm4864 = "Bsln + 1.0|W/kg|male", sm4857 = "Bsln + 1.0|W/kg|female"
)
percentiles <- c(2.5, 5, 10, 25, 50, 75, 90, 95, 97.5)

reference_power <- do.call(rbind, lapply(names(files), function(id) {
  path <- file.path(args[1], sprintf("rjsp_a_2517913_%s.xlsx", id))
  ## one file also holds the paper's title block in an extra column: keep age and the nine percentiles only
  sheet <- as.data.frame(suppressMessages(readxl::read_excel(path)))[, c("age", as.character(percentiles))]
  curve <- strsplit(files[[id]], "|", fixed = TRUE)[[1]]
  data.frame(
    method = curve[1], unit = curve[2], sex = curve[3],
    age = rep(sheet$age, each = length(percentiles)), percentile = percentiles,
    value = as.vector(t(as.matrix(sheet[, -1])))
  )
}))

## the medians of Tables 2 and 3 for ages 14-18 and 59-64, in the order of `files`
published <- rbind(
  c(129.43, 112.01), c(98.50, 83.36), c(1.907, 1.364), c(1.645, 1.186),
  c(112.05, 102.47), c(84.43, 77.88), c(1.654, 1.245), c(1.409, 1.102)
)
for(k in seq_along(files)) {
  curve <- strsplit(files[[k]], "|", fixed = TRUE)[[1]]
  rows <- reference_power$method == curve[1] & reference_power$unit == curve[2] & reference_power$sex == curve[3] &
    reference_power$percentile == 50
  median_at <- function(ages) mean(reference_power$value[rows & reference_power$age %in% ages])
  stopifnot(abs(c(median_at(14:18), median_at(59:64)) / published[k, ] - 1) < 0.02)
}
stopifnot(nrow(reference_power) == 8 * 51 * 9, !anyNA(reference_power$value))

save(reference_power, file = "R/sysdata.rda", compress = "xz", version = 2)
