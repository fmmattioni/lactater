## code to prepare `demo_data` dataset goes here

demo_data <- dplyr::tribble(
  ~step, ~length, ~intensity, ~lactate, ~heart_rate,
  0L,       0L,          0L,     0.93,  96L,
  1L,       3L,         50L,     0.98, 114L,
  2L,       3L,         75L,     1.23, 134L,
  3L,       3L,        100L,     1.88, 154L,
  4L,       3L,        125L,      2.8, 170L,
  5L,       3L,        150L,     4.21, 182L,
  6L,       3L,        175L,     6.66, 193L,
  7L,       2L,        191L,     8.64, 198L
)

## simulated RPE (Borg 6-20), by the rule of the simulated tests in data-raw/lactate_tests.R: 6 at rest, about 11 at
## 2 mmol/L and 14 at 4 mmol/L, near the cohort means at the first rise in lactate (10.8) and at 4 mmol/L (14.1) of
## Scherr et al. (2013), and 19 at the last stage
rest <- demo_data$lactate[1]
peak <- max(demo_data$lactate)
demo_data$rpe <- c(6L, as.integer(round(stats::approx(c(rest, 2, 4, peak), c(7, 11, 14, 19), demo_data$lactate[-1])$y)))

usethis::use_data(demo_data, overwrite = TRUE)
