## Builds `lactate_tests`: real step tests from two openly licensed datasets, plus three simulated tests
## for scenarios no open dataset covers. Run from the package root:
##
##   Rscript data-raw/lactate_tests.R <tcd_data_dir> <heuberger_xlsx>
##
## * Trinity College Dublin GXT data: Donne et al., Zenodo, doi:10.5281/zenodo.10841412, CC BY 4.0.
##   <tcd_data_dir> is the `data` folder of the unzipped data_v2.zip (one workbook per test: sheet 1 holds
##   the athlete details, sheet 2 the stages).
## * Heuberger et al. (2018) PLOS ONE doi:10.1371/journal.pone.0206846; data on figshare,
##   doi:10.6084/m9.figshare.7240571, CC BY 4.0. <heuberger_xlsx> is its data.xlsx.
##
## No names, birth dates or test dates are kept. Repeat tests of one athlete are matched on birth date and
## sex only to build `day` (days since that athlete's first test).

args <- commandArgs(trailingOnly = TRUE)
tcd_dir <- args[1]
heuberger_file <- args[2]

read_tcd <- function(file) {
  path <- file.path(tcd_dir, file)
  details <- as.data.frame(suppressMessages(readxl::read_excel(path, sheet = 1, col_names = FALSE)))
  detail <- function(label) details[[2]][match(label, details[[1]])]
  stages <- as.data.frame(suppressMessages(readxl::read_excel(path, sheet = 2)))
  stages <- as.data.frame(lapply(stages[, c(1, 2, 4)], function(v) suppressWarnings(as.numeric(v))))
  names(stages) <- c("intensity", "heart_rate", "lactate")
  stages <- stages[!is.na(stages$intensity) & !is.na(stages$lactate), ]
  list(
    stages = stages,
    sex = trimws(detail("Gender:")),
    body_mass = as.numeric(detail("Weight")),
    born = as.numeric(detail("DOB:")),
    tested = as.numeric(detail("Test Date:"))
  )
}

tcd_test <- function(file, test, sport, description, athlete = test, day = 0) {
  t <- read_tcd(file)
  data.frame(
    test = test, athlete = athlete, sport = sport, sex = t$sex, body_mass = t$body_mass, day = day,
    description = description, source = "Trinity College Dublin (CC BY 4.0)",
    step = seq_len(nrow(t$stages)) - 1L, intensity = t$stages$intensity,
    lactate = t$stages$lactate, heart_rate = t$stages$heart_rate, rpe = NA_real_
  )
}

## one cyclist tested six times over 5.6 years; the first two tests are 58 days apart
series <- c("A224.xlsx", "A222.xlsx", "A223.xlsx", "A225.xlsx", "A221.xlsx", "A220.xlsx")
series_dates <- vapply(series, function(f) read_tcd(f)$tested, numeric(1))
series_tests <- do.call(rbind, Map(function(file, k) {
  tcd_test(
    file, sprintf("cycling_series_%d", k), "cycling",
    sprintf("Trained male cyclist, test %d of 6 (3-min stages)", k),
    athlete = "cyclist_series", day = as.integer(series_dates[[k]] - series_dates[[1]])
  )
}, series, seq_along(series)))

real <- rbind(
  tcd_test("A554.xlsx", "cycling_elite", "cycling", "Elite male cyclist, 6.6 W/kg at peak (3-min stages)"),
  tcd_test("A508.xlsx", "cycling_female", "cycling", "Trained female cyclist, 5.0 W/kg at peak (3-min stages)"),
  tcd_test("A290.xlsx", "cycling_early_dip", "cycling",
           "Trained male cyclist whose lactate dips after the first stage (3-min stages)"),
  series_tests,
  tcd_test("A1042.xlsx", "running", "running", "Trained male runner, treadmill at 0% grade (3-min stages)"),
  tcd_test("A1167.xlsx", "rowing", "rowing", "Trained male rower, 3 min work + 1 min rest per stage")
)

## Heuberger et al. (2018): the baseline test (-14 d) of subject 1, a complete curve with no sampling notes;
## 5-min steps from 175 W in 25 W steps, no resting sample and no heart rate
heuberger <- as.data.frame(readxl::read_excel(heuberger_file))
baseline <- heuberger[
  grepl("^-14d", heuberger$ProtTime) & grepl("^LactMax[0-9]+$", heuberger$Parameter) &
    is.na(heuberger$Parameter_Notes_N),
]
baseline$lactate <- suppressWarnings(as.numeric(baseline$Value))
baseline <- baseline[!is.na(baseline$lactate), ]
baseline$k <- as.integer(sub("LactMax", "", baseline$Parameter))
chosen <- baseline[baseline$SubjectNr == 1, ]
chosen <- chosen[order(chosen$k), ]
heuberger_test <- data.frame(
  test = "cycling_5min_no_rest", athlete = "cycling_5min_no_rest", sport = "cycling", sex = "M",
  body_mass = NA_real_, day = 0L, description = "Well-trained male cyclist, 5-min steps, no resting sample",
  source = "Heuberger et al. 2018 (CC BY 4.0)", step = chosen$k, intensity = 175 + 25 * (chosen$k - 1),
  lactate = chosen$lactate, heart_rate = NA_real_, rpe = NA_real_
)

## simulated tests, from published protocols and typical values (see ?lactate_tests)
simulate <- function(test, sport, sex, body_mass, description, intensity, rest, at_lt, lt_share, peak_lactate,
                     hr_rest, hr_peak, seed) {
  set.seed(seed)
  peak <- max(intensity)
  ## lactate = rest + a * (exp(k * x / peak) - 1), through `at_lt` at `lt_share` of peak and `peak_lactate` at peak
  gap <- function(k) (at_lt - rest) / (exp(k * lt_share) - 1) - (peak_lactate - rest) / (exp(k) - 1)
  k <- stats::uniroot(gap, c(0.5, 30))$root
  a <- (peak_lactate - rest) / (exp(k) - 1)
  true <- rest + a * (exp(k * intensity / peak) - 1)
  ## analyser scatter: CV about 2-4% from 2 mmol/L and more below (Bonaventura et al., 2015)
  cv <- ifelse(true < 2, 0.05, 0.03)
  lactate <- round(true * exp(stats::rnorm(length(true), 0, cv)), 1)
  hr <- round(hr_rest + (hr_peak - hr_rest) * (intensity / peak)^0.9 + stats::rnorm(length(true), 0, 1.5))
  ## RPE (Borg 6-20) rising with lactate: about 11 at 2 mmol/L and 14 at 4 mmol/L, near the cohort means at the
  ## first rise in lactate (10.8) and at 4 mmol/L (14.1) of Scherr et al. (2013), and 19 at the last stage; whole
  ## numbers that never fall from one stage to the next
  rpe <- stats::approx(c(rest, 2, 4, peak_lactate), c(7, 11, 14, 19), true, rule = 2)$y
  rpe <- c(6, cummax(round(rpe[-1] + stats::rnorm(length(true) - 1, 0, 0.7))))
  data.frame(
    test = test, athlete = test, sport = sport, sex = sex, body_mass = body_mass, day = 0L,
    description = description, source = "simulated", step = c(0L, seq_along(intensity[-1])),
    intensity = c(0, intensity[-1]), lactate = c(round(rest, 1), lactate[-1]), heart_rate = c(70, hr[-1]),
    rpe = pmin(rpe, 20)
  )
}

simulated <- rbind(
  ## 7 x 200 m, +0.05 m/s per step (Pelarigo et al., 2017)
  simulate("swimming", "swimming", "F", 62, "Simulated elite female swimmer, 7 x 200 m step test",
           intensity = c(0, seq(1.20, 1.50, by = 0.05)), rest = 1.0, at_lt = 2.5, lt_share = 0.9,
           peak_lactate = 8.5, hr_rest = 100, hr_peak = 188, seed = 1),
  ## 25 W every 3 min; untrained men peaked near 250 W (Messonnier et al., 2013)
  simulate("untrained", "cycling", "M", 82, "Simulated untrained man, fitness client (3-min stages)",
           intensity = c(0, seq(50, 250, by = 25)), rest = 0.9, at_lt = 2.5, lt_share = 0.65,
           peak_lactate = 9.0, hr_rest = 98, hr_peak = 186, seed = 2),
  ## the same client stopped early, before the second turn point
  simulate("submaximal", "cycling", "M", 82, "Simulated submaximal test, stopped at about 3 mmol/L",
           intensity = c(0, seq(50, 250, by = 25)), rest = 0.9, at_lt = 2.5, lt_share = 0.65,
           peak_lactate = 9.0, hr_rest = 98, hr_peak = 186, seed = 2)
)
simulated <- simulated[!(simulated$test == "submaximal" & simulated$intensity > 175), ]

lactate_tests <- tibble::as_tibble(rbind(real, heuberger_test, simulated))
lactate_tests$step <- as.integer(lactate_tests$step)
lactate_tests$day <- as.integer(lactate_tests$day)

stopifnot(
  !anyNA(lactate_tests$intensity), !anyNA(lactate_tests$lactate),
  all(tapply(lactate_tests$intensity, lactate_tests$test, function(x) all(diff(x) > 0)))
)
usethis::use_data(lactate_tests, overwrite = TRUE)
