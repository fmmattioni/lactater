#' Convert m/s to s/100m
#'
#' It converts speed, in m/s, to pace, in s/100m.
#'
#' @param speed The speed to convert to pace.
#'
#' @return the converted pace
#' @keywords internal
convert_to_pace <- function(speed) {
  out <- 100 / speed
  out <- round(x = out, digits = 1)

  out
}
