##########
# This file contains only internal functions
##########

#' Check if all suggested packages are installed
#'
#' @return a boolean
#' @keywords internal
is_installed <- function() {
  extra_packages <- c("bsplus", "datapasta", "glue", "miniUI", "rhandsontable", "shinyWidgets", "shinyjs")
  length(
    find.package(
      package = extra_packages,
      quiet = TRUE
    )
  ) == length(extra_packages)
}

#' Add external resources
#'
#' This function was copied from golem
#'
#' @keywords internal
add_external_resources <- function(){

  shiny::addResourcePath(
    'extra', system.file('extra', package = 'lactater')
  )

  shiny::tags$head(
    shiny::tags$link(rel="stylesheet", type="text/css", href="extra/style.css")
  )
}

#' Produce data protocol skeleton
#'
#' This is an internal function used in `run_data_input()`.
#'
#' @param input_steps The total number of steps from the incremental test.
#' @param input_length_steps The length of each step, in minutes.
#' @param input_starting_load The starting load.
#' @param input_step_increase The step increase.
#' @param input_heart_rate_data A boolean indicating whether heart rate data was collected.
#' @param input_completed A boolean indicating whether the last step was fully completed.
#' @param input_last_length_step If the last step was not fully completed, then indicate how long it lasted.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @keywords internal
helper_data_protocol <- function(
  input_steps,
  input_length_steps,
  input_starting_load,
  input_step_increase,
  input_heart_rate_data,
  input_completed,
  input_last_length_step,
  sport
) {
  step <- seq(0, input_steps - 1)
  if(sport == "Swimming") {
    df <- data.frame(step = step, intensity = 0, lactate = 0)
  } else {
    length <- c(0, rep(input_length_steps, input_steps - 1))
    if(!input_completed) length[input_steps] <- helper_last_length_step(input_last_length_step)
    ## stage 1 is at the starting load; later stages add the increase pro rata to their length
    increase <- cumsum(c(0, input_step_increase * length[-(1:2)] / input_length_steps))
    df <- data.frame(step = step, length = length, intensity = c(0, round(input_starting_load + increase, 2)), lactate = 0)
  }

  if(input_heart_rate_data) df$heart_rate <- 0L

  df
}

#' Calculate length of last step
#'
#' For internal use only.
#'
#' @param last_length_step A string indicating the length of last step.
#'
#' @keywords internal
helper_last_length_step <- function(last_length_step) {
  if(!grepl(":", last_length_step, fixed = TRUE)) return(as.numeric(last_length_step))
  min_sec <- as.numeric(strsplit(last_length_step, ":", fixed = TRUE)[[1]])
  round(min_sec[1] + min_sec[2] / 60, digits = 1)
}

#' Render table for data input
#'
#' For internal use only.
#'
#' @param .data The raw data.
#' @param sport The sport at which the incremental test was performed. One of `cycling`, `running`, or `swimming`.
#'
#' @keywords internal
helper_render_table <- function(.data, sport) {
  if(sport == "Swimming") {
    rhandsontable::rhandsontable(.data) %>%
      rhandsontable::hot_cols(colWidths = 80, halign = 'htCenter',
                              renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col > 0) {
              td.style.background = 'lightgreen';
             }
           }") %>%
      rhandsontable::hot_col("intensity", format = "0") %>%
      rhandsontable::hot_col("step", format = "0", readOnly = TRUE) %>%
      rhandsontable::hot_cell(row = 1, col = "intensity", readOnly = TRUE)
  } else {
    rhandsontable::rhandsontable(.data) %>%
      rhandsontable::hot_cols(colWidths = 80, halign = 'htCenter',
                              renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col > 2) {
              td.style.background = 'lightgreen';
             }
           }") %>%
      rhandsontable::hot_col("step", format = "0", readOnly = TRUE) %>%
      rhandsontable::hot_col("length", format = "0", readOnly = TRUE) %>%
      rhandsontable::hot_col("intensity", format = "0", readOnly = TRUE)
  }
}
