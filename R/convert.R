#' Convert intensity to pace
#'
#' * swimming: speed in m/s to pace in s/100 m;
#' * running: speed in km/h to pace in s/km;
#' * rowing: power in W to split in s/500 m, with the Concept2 relation `watts = 2.80 / pace^3`
#'   (pace in s/m).
#'
#' @param intensity The intensity to convert.
#' @param sport The sport.
#'
#' @return the pace, rounded to 0.1 s.
#' @keywords internal
convert_to_pace <- function(intensity, sport = "swimming") {
  pace <- switch(
    sport,
    swimming = 100 / intensity,
    running = 3600 / intensity,
    rowing = 500 * (2.80 / intensity)^(1 / 3)
  )
  round(pace, digits = 1)
}

#' Format a pace as minutes and seconds
#'
#' @param seconds The pace in seconds.
#' @param sport The sport, which sets the distance unit.
#'
#' @return e.g. `"4:30 /km"`.
#' @keywords internal
format_pace <- function(seconds, sport) {
  seconds <- round(seconds)
  sprintf("%d:%02d %s", seconds %/% 60, seconds %% 60, switch(sport, swimming = "/100 m", running = "/km", rowing = "/500 m"))
}

#' Decimals shown for the intensity of each sport
#'
#' @param sport The sport.
#' @keywords internal
intensity_digits <- function(sport) {
  switch(sport, cycling = 1, rowing = 1, running = 2, swimming = 3)
}
