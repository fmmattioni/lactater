#' A sheet to record a step test
#'
#' Copies a printable recording sheet, a web form and a CSV template into a folder, so every test is recorded the same
#' way and goes straight into [lactate_threshold()]. The sheet is an A4 web page (open it in a browser and print it)
#' with the athlete, the 24 to 48 h before the test, the protocol, one row per stage for intensity, lactate, heart rate
#' and RPE, and how the test ended, as [Best practices for a step
#' test](https://fmmattioni.github.io/lactater/articles/best-practices.html) suggests. The form has the same fields to
#' fill in during the test, saves itself in the browser, and downloads an `.xlsx` that [read_recording()] reads, or a
#' CSV of the stages; it works offline and is also [on the
#' website](https://fmmattioni.github.io/lactater/step-test-form.html). The template has the columns lactater reads,
#' with the resting sample as the first row.
#'
#' @param dir The folder to copy them into.
#' @param open Whether to open the form in the browser.
#'
#' @return the paths of the sheet, the form and the template, invisibly.
#' @seealso [read_recording()] to read a test recorded with the form.
#' @export
#'
#' @examples
#' files <- recording_sheet(tempfile(), open = FALSE)
#' read.csv(files[["template"]])
recording_sheet <- function(dir = ".", open = interactive()) {
  from <- system.file(
    "sheets", c("step-test-sheet.html", "step-test-form.html", "step-test-template.csv"), package = "lactater"
  )
  to <- file.path(dir, basename(from))
  if(any(file.exists(to))) stop(sprintf("`%s` already exists.", to[file.exists(to)][1]), call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(from, to)
  if(open) utils::browseURL(to[2])
  invisible(c(sheet = to[1], form = to[2], template = to[3]))
}

#' Read a recorded step test
#'
#' Reads a test recorded with the web form of [recording_sheet()] (an `.xlsx` with the sheets `stages`, `recovery` and
#' `test`), or a CSV in the columns of the template, and returns what [lactate_threshold()] and [test_report()] need.
#' Needs the readxl package for an `.xlsx`.
#'
#' @param file The `.xlsx` downloaded from the form, or a `.csv`.
#'
#' @return a list with
#' * `stages`: one row per stage, the resting sample first, with `intensity`, `lactate`, `heart_rate` and `rpe` (and
#'   `clock_time` and `notes` from the form);
#' * `recovery`: the recovery samples (`time`, in minutes, and `lactate`) for the Stegmann threshold, or `NULL`;
#' * `athlete`: `name`, `date`, `sex`, `age` and `body_mass`, as recorded, for `test_report(athlete = )` or
#'   [report_header()];
#' * `sport`, `sample`, `rpe_scale`, `stage_duration` (min), `site` and `grade` (%), for [lactate_threshold()]:
#'   `"cycling"`, `"blood"` and `"6-20"` when not recorded, and `NULL` for the others;
#' * `protocol`: the protocol in one line, for `test_report(protocol = )`, or `NULL`;
#' * `details`: every field of the form, as text.
#' @seealso [recording_sheet()] for the form.
#' @export
#'
#' @examples
#' ## a test recorded with the CSV template
#' file <- tempfile(fileext = ".csv")
#' write.csv(demo_data[c("intensity", "lactate", "heart_rate", "rpe")], file, row.names = FALSE)
#' test <- read_recording(file)
#' results <- lactate_threshold(
#'   test$stages, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
#' )
read_recording <- function(file) {
  if(!is.character(file) || length(file) != 1 || !file.exists(file)) stop("Cannot find `file`.", call. = FALSE)
  extension <- tolower(tools::file_ext(file))
  if(!extension %in% c("xlsx", "csv")) {
    stop("`file` must be the .xlsx of the recording form, or a .csv in the columns of the template.", call. = FALSE)
  }
  recovery <- NULL
  fields <- list()
  if(extension == "csv") {
    stages <- utils::read.csv(file)
  } else {
    rlang::check_installed("readxl", reason = "to read the .xlsx of the recording form.")
    sheets <- readxl::excel_sheets(file)
    if(!"stages" %in% sheets) {
      stop("`file` has no `stages` sheet: is it the .xlsx of the recording form?", call. = FALSE)
    }
    stages <- as.data.frame(readxl::read_excel(file, sheet = "stages"))
    if("recovery" %in% sheets) recovery <- as.data.frame(readxl::read_excel(file, sheet = "recovery"))
    if(!is.null(recovery) && nrow(recovery) == 0) recovery <- NULL
    if("test" %in% sheets) {
      test <- readxl::read_excel(file, sheet = "test", col_types = "text")
      fields <- stats::setNames(as.list(test$value), test$field)
      fields <- fields[!vapply(fields, is.na, logical(1))]
    }
  }
  if(!all(c("intensity", "lactate") %in% names(stages))) {
    stop("The stages need the columns `intensity` and `lactate`.", call. = FALSE)
  }
  number <- function(x) if(is.null(x)) NULL else suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
  athlete <- list(name = fields$name, date = fields$date, sex = fields$sex, age = number(fields$age),
                  body_mass = number(fields$body_mass))
  sport <- if(!is.null(fields$sport)) fields$sport else "cycling"
  list(
    stages = stages, recovery = recovery, athlete = athlete[!vapply(athlete, is.null, logical(1))], sport = sport,
    sample = if(!is.null(fields$sample)) fields$sample else "blood",
    rpe_scale = if(!is.null(fields$rpe_scale)) fields$rpe_scale else "6-20",
    stage_duration = number(fields$stage_duration), site = if(!is.null(fields$site) && nzchar(fields$site)) fields$site, grade = number(fields$treadmill_grade),
    protocol = recording_protocol(fields, sport), details = fields
  )
}

#' The protocol of a recorded test in one line
#'
#' @param fields The fields of the recording form.
#' @param sport The sport.
#' @return a string such as "Lode Excalibur, 3-min stages of +25 W from 50 W, earlobe whole blood, Biosen C-Line", or
#'   `NULL` when nothing was recorded.
#' @keywords internal
recording_protocol <- function(fields, sport) {
  unit <- intensity_unit(sport)
  stages <- if(!is.null(fields$stage_duration) && !is.null(fields$increment)) {
    paste0(
      fields$stage_duration, "-min stages of +", fields$increment, " ", unit,
      if(!is.null(fields$start)) paste0(" from ", fields$start, " ", unit)
    )
  }
  sample <- if(!is.null(fields$sample)) unname(c(blood = "whole blood")[fields$sample]) else NULL
  if(!is.null(sample) && is.na(sample)) sample <- fields$sample
  sampling <- paste(c(fields$site, sample), collapse = " ")
  parts <- c(fields$ergometer, stages, if(nzchar(sampling)) sampling, fields$analyzer)
  if(length(parts)) paste(parts, collapse = ", ")
}
