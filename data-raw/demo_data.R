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

usethis::use_data(demo_data, overwrite = TRUE)
