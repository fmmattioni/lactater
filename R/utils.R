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

#' Interpolate intensity
#'
#' @param .data The raw data.
#' @param interpolation_factor The interpolation factor to be used. This will depend on the sport chosen.
#'
#' @keywords internal
#' @importFrom stats approx
interpolate_intensity <- function(.data, interpolation_factor) {
  out <- lapply(.data, function (i) approx(
    x = .data[[1]],
    y = i,
    xout = seq(min(.data[[1]]), max(.data[[1]], na.rm = TRUE), interpolation_factor)
  )$y
  ) %>%
    dplyr::as_tibble()

  out
}

#' Add external resources
#'
#' This function was copied from {golem}
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
  if(sport == "Swimming") {
    steps <- input_steps

    df <- dplyr::tibble(
      step = seq(0, steps - 1, 1),
      intensity = 0,
      lactate = 0
    )

  } else {
    steps <- input_steps
    length_steps <- input_length_steps

    tmp1 <- dplyr::tibble(
      step = c(0, 1),
      length = c(0, length_steps),
      intensity = c(0, input_starting_load)
    )

    tmp2 <- dplyr::tibble(
      step = seq(0, input_steps - 1, 1)[-1:-2],
      length = c(0, rep(input_length_steps, input_steps - 1))[-1:-2]
    )

    if(!input_completed) {
      input_last_length_step <- helper_last_length_step(input_last_length_step)

      tmp2[nrow(tmp2), ]$length <- input_last_length_step
    }

    tmp2 <- tmp2 %>%
      dplyr::mutate(intensity = cumsum((input_step_increase * length) / input_length_steps) + input_starting_load,
                    length = length) %>%
      dplyr::mutate(intensity = round(intensity, 2))

    df <- dplyr::bind_rows(tmp1, tmp2) %>%
      dplyr::mutate(lactate = 0)
  }

  if(input_heart_rate_data)
    df <- df %>%
      dplyr::mutate(heart_rate = as.integer(0))

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
  if(length(last_length_step > 0) & !stringr::str_detect(last_length_step, ":")) {
    out <- as.numeric(last_length_step)
  } else if (length(last_length_step > 0) & stringr::str_detect(last_length_step, ":")){
    out <- last_length_step %>%
      lubridate::ms() %>%
      lubridate::period_to_seconds() %>%
      magrittr::divide_by(60) %>%
      round(digits = 1)
  }

  out
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
