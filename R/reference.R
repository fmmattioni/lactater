#' Thresholds against the population
#'
#' Places the power of a cycling test at 3 mmol/L (`"OBLA 3.0"`) and at 1 mmol/L above the resting lactate
#' (`"Bsln + 1.0"`) among people of the same sex and age, on the reference percentiles of the German population aged 14
#' to 64 (Fiedler et al., 2025): in watts and, with `body_mass`, in watts per kg.
#'
#' The percentiles come from 2,389 women and 2,801 men of two national health surveys (KiGGS wave 2 and DEGS1), tested
#' on a cycle ergometer from 25 W with 25 W more every 2 min, at 60-80 rpm, with earlobe blood analyzed on a Biosen
#' C-Line, until 85% of the age-predicted maximal heart rate. Their thresholds were computed with lactater "on a 3rd
#' degree polynomial with the baseline (step 0) data" included, so the two thresholds are computed here in the same way,
#' whatever `fit` and `include_baseline` the test was analyzed with: they can differ by a few watts from its table. Read
#' the result with three limits in mind:
#'
#' * **The protocol.** Percentiles transfer only between tests run the same way. Shorter stages put the thresholds at
#'   higher intensities (Bentley et al., 2007), so a test with 3-min stages looks a little weaker against these 2-min
#'   norms. A warning is given when the steps of the test are not 25 W or it has no resting sample; the stage length
#'   is yours to check.
#' * **The population.** These are values of the general population, not of athletes: most trained cyclists land
#'   above the 97.5th percentile.
#' * **The precision.** The percentile is interpolated between the nine published percentiles, from the 2.5th to the
#'   97.5th, on the normal scale and at the exact age. Beyond the 2.5th or the 97.5th it is `NA`, and `standing`
#'   says "top 2.5%" or "bottom 2.5%".
#'
#' @param x The result of [lactate_threshold()] for a cycling test.
#' @param age Age in years, from 14 to 64.
#' @param sex `"female"` or `"male"` (or `"F"`, `"M"`).
#' @param body_mass Body mass (kg), for the percentiles of power per kg, or `NULL`.
#'
#' @return a `lactate_reference` tibble with one row per threshold and unit: `method`, `value`, `unit`, the
#'   `percentile`, and the `standing` in plain words, from the published percentiles it falls between: "top 5%",
#'   "top 10%", "top 25%", "upper half", "lower half", "bottom 25%", "bottom 10%", "bottom 5%", or beyond the
#'   2.5th and 97.5th, "top 2.5%" and "bottom 2.5%".
#'
#' @references
#' Fiedler J, Thron M, Altmann S, Haertel S, Barsch F, Schlenker L, Krug S, Boes K, Woll A. Reference standards for
#' power at the lactate threshold 2 for cycle ergometry throughout the lifespan in Germany. Journal of Sports
#' Sciences. 2025;43(16):1654-1664. doi:10.1080/02640414.2025.2517913
#'
#' Bentley DJ, Newell J, Bishop D. Incremental exercise test design and analysis: implications for performance
#' diagnostics in endurance athletes. Sports Medicine. 2007;37(7):575-586.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' reference_percentiles(results, age = 31, sex = "male", body_mass = 72)
reference_percentiles <- function(x, age, sex, body_mass = NULL) {
  info <- lactater_info(x)
  if(info$settings$sport != "cycling") stop("The reference values are for cycle ergometry.", call. = FALSE)
  if(!is.numeric(age) || length(age) != 1 || is.na(age) || age < 14 || age > 64)
    stop("`age` must be one number from 14 to 64, the ages of the reference values.", call. = FALSE)
  key <- if(is.character(sex) && length(sex) == 1) tolower(sex) else NA_character_
  sex <- unname(c(f = "female", female = "female", m = "male", male = "male")[key])
  if(is.na(sex)) stop("`sex` must be \"female\" or \"male\".", call. = FALSE)
  if(!is.null(body_mass) && (!is.numeric(body_mass) || length(body_mass) != 1 || is.na(body_mass) || body_mass <= 0))
    stop("`body_mass` must be a single positive number (kg).", call. = FALSE)
  methods <- c("OBLA 3.0", "Bsln + 1.0")

  ## the reference tests: 25 W steps, a resting sample
  step <- stats::median(diff(sort(info$data$intensity[info$data$intensity > 0])))
  if(!is.na(step) && abs(step / 25 - 1) > 0.05) {
    warning(sprintf(
      "The steps of this test (+%s W) differ from those of the reference tests (+25 W every 2 min): %s",
      format(step), "the thresholds, and so the percentiles, may shift."
    ), call. = FALSE)
  }
  if(!any(info$data$intensity == 0)) {
    warning(
      "This test has no resting sample, so Bsln + 1.0 is 1 mmol/L above the first stage, not above rest.",
      call. = FALSE
    )
  }

  ## computed as in the reference: a 3rd degree polynomial with the resting sample (Fiedler et al., 2025)
  settings <- utils::modifyList(info$settings, list(
    fit = "3rd degree polynomial", include_baseline = TRUE, fixed = 3, baseline_plus = 1
  ))
  found <- compute_thresholds(info$data, settings, c("OBLA", "Bsln+"))$rows
  power <- found$intensity[match(methods, found$method)]
  rows <- data.frame(method = methods, value = power, unit = "W")
  if(!is.null(body_mass)) rows <- rbind(rows, data.frame(method = methods, value = power / body_mass, unit = "W/kg"))
  ## placed unrounded, shown rounded
  placed <- Map(reference_percentile, rows$value, rows$method, rows$unit, sex, age)
  out <- tibble::tibble(
    method = rows$method, value = ifelse(rows$unit == "W", round(rows$value), round(rows$value, 2)), unit = rows$unit,
    percentile = round(vapply(placed, `[[`, numeric(1), "percentile")),
    standing = vapply(placed, `[[`, character(1), "standing")
  )
  attr(out, "lactater_reference") <- list(sex = sex, age = age)
  class(out) <- c("lactate_reference", class(out))
  out
}

#' The percentile of one value among the reference values
#'
#' Interpolates the nine published percentiles to the age, then the value between them on the normal scale.
#'
#' @param value The power (W) or power per kg.
#' @param method `"OBLA 3.0"` or `"Bsln + 1.0"`.
#' @param unit `"W"` or `"W/kg"`.
#' @param sex `"female"` or `"male"`.
#' @param age Age in years.
#' @return a list with the `percentile` (`NA` beyond the published ones) and the `standing`.
#' @keywords internal
reference_percentile <- function(value, method, unit, sex, age) {
  ref <- reference_power[reference_power$method == method & reference_power$unit == unit &
                           reference_power$sex == sex, ]
  at_age <- vapply(split(ref, ref$percentile), function(d) stats::approx(d$age, d$value, age)$y, numeric(1))
  percentiles <- as.numeric(names(at_age))
  if(is.na(value)) return(list(percentile = NA_real_, standing = NA_character_))
  if(value < at_age[1]) return(list(percentile = NA_real_, standing = "bottom 2.5%"))
  if(value > at_age[length(at_age)]) return(list(percentile = NA_real_, standing = "top 2.5%"))
  k <- findInterval(value, at_age, rightmost.closed = TRUE)
  z <- stats::approx(at_age, stats::qnorm(percentiles / 100), value)$y
  ## between the 2.5th and 5th, the 5th and 10th, ..., the 95th and 97.5th published percentiles
  standing <- c("bottom 5%", "bottom 10%", "bottom 25%", "lower half", "upper half", "top 25%", "top 10%", "top 5%")
  list(percentile = 100 * stats::pnorm(z), standing = standing[k])
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_reference <- function(x, ...) {
  reference <- attr(x, "lactater_reference")
  if(is.null(reference)) return(NextMethod())
  c(
    "Reference percentiles" = sprintf(
      "%s aged %s in Germany (Fiedler et al., 2025)", if(reference$sex == "female") "women" else "men",
      format(reference$age)
    ),
    "Reference test" = "cycle ergometer, +25 W every 2 min, earlobe blood; general population, not athletes"
  )
}
