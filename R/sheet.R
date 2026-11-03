#' A sheet to record a step test
#'
#' Copies a printable recording sheet and a CSV template into a folder, so every test is recorded the same way and
#' goes straight into [lactate_threshold()]. The sheet is an A4 web page (open it in a browser and print it) with the
#' athlete, the 24 to 48 h before the test, the protocol, one row per stage for intensity, lactate, heart rate and RPE,
#' and how the test ended, as [Best practices for a step
#' test](https://fmmattioni.github.io/lactater/articles/best-practices.html) suggests. The template has the columns
#' lactater reads, with the resting sample as the first row.
#'
#' @param dir The folder to copy them into.
#' @param open Whether to open the sheet in the browser.
#'
#' @return the paths of the sheet and the template, invisibly.
#' @export
#'
#' @examples
#' files <- recording_sheet(tempfile(), open = FALSE)
#' read.csv(files[["template"]])
recording_sheet <- function(dir = ".", open = interactive()) {
  from <- system.file("sheets", c("step-test-sheet.html", "step-test-template.csv"), package = "lactater")
  to <- file.path(dir, basename(from))
  if(any(file.exists(to))) stop(sprintf("`%s` already exists.", to[file.exists(to)][1]), call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(from, to)
  if(open) utils::browseURL(to[1])
  invisible(c(sheet = to[1], template = to[2]))
}
