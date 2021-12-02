#' Data input widget
#'
#' Widget to help with data input.
#'
#' @param width The width, in pixels.
#' @param height The height, in pixels.
#'
#' @return The code to reproduce the manual data input.
#' @export
run_data_input <- function(width = 1200, height = 900) {

  # check additional needed packages
  if(!is_installed())
    stop(
      "
      It looks like you don't have all the packages needed for this function to run. Either install all of them with:
      install.packages(c('bsplus', 'datapasta', 'glue', 'miniUI', 'rhandsontable', 'shinyWidgets', 'shinyjs'))
      or install the ones you know you do not have.
      ",
      call. = FALSE
    )

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      title = "Data input helper",
      right = miniUI::miniTitleBarButton(inputId = "btn_reset", label = "Reset all")
    ),
    miniUI::miniContentPanel(

      shinyjs::useShinyjs(),
      add_external_resources(),

      shiny::column(
        width = 6,

        shiny::strong("Please, choose the sport", shiny::icon("info-circle"), ":") %>%
          bsplus::bs_embed_tooltip(title = "Select the sport that the incremental test was performed in.", placement = "right"),

        shinyWidgets::pickerInput(
          inputId = "choose_sport",
          label = NULL,
          choices = c("Cycling", "Running", "Swimming"),
          multiple = FALSE,
          selected = "Cycling",
          choicesOpt = list(
            content = sprintf("<i class='fas fa-%s'>&nbsp;%s</i>",
                              c("biking", "running", "swimmer"),
                              c("Cycling", "Running", "Swimming")))
        ),

        shiny::strong("Would you like to include heart rate (HR) data", shiny::icon("info-circle"), "?") %>%
          bsplus::bs_embed_tooltip(title = "Select this option in case you also have HR data for each step.", placement = "right"),

        shinyWidgets::switchInput(
          inputId = "heart_rate_data",
          size = "mini",
          onStatus = "success",
          offStatus = "danger",
          onLabel = "Yes, sir!",
          offLabel = "Nah, I'm good!"
        ),

        shiny::strong("Please, indicate below how many steps were done (including rest)", shiny::icon("info-circle"), ":") %>%
          bsplus::bs_embed_tooltip(title = "For example, if you performed 9 steps plus rest, input 10 here.", placement = "right"),

        shiny::numericInput(inputId = "steps",
                     label = NULL,
                     value = 5,
                     min = 1,
                     max = Inf,
                     step = 1),

        shiny::div(
          id = "options_if_no_swimming",

          shiny::strong("Please, indicate below the length (in minutes) of each step", shiny::icon("info-circle"), ":") %>%
            bsplus::bs_embed_tooltip(title = "This is the time of each step (usually 3 or 4 minutes).", placement = "right"),

          shiny::numericInput(inputId = "length_steps",
                       label = NULL,
                       value = 3,
                       min = 1,
                       max = 5,
                       step = 1),

          shiny::strong("Please, indicate the starting load", shiny::icon("info-circle"), ":") %>%
            bsplus::bs_embed_tooltip(title = "Indicate here the load (in power output or speed) after rest (e.g., 50 W).", placement = "right"),

          shiny::numericInput(inputId = "starting_load",
                       label = NULL,
                       value = 0,
                       min = 1,
                       max = Inf,
                       step = 1),

          shiny::strong("Please, indicate the step increase", shiny::icon("info-circle"), ":") %>%
            bsplus::bs_embed_tooltip(title = "Indicate by how many watts, km/h, or m/s the load was increased on each step (e.g., 25 W).", placement = "right"),

          shiny::numericInput(inputId = "step_increase",
                       label = NULL,
                       value = 0,
                       min = 1,
                       max = Inf,
                       step = 1),

          shiny::strong("Was the last step fully completed", shiny::icon("info-circle"), "?") %>%
            bsplus::bs_embed_tooltip(title = "Select this option in case the last step was not fully completed.", placement = "right"),

          shinyWidgets::switchInput(
            inputId = "completed",
            size = "mini",
            onStatus = "success",
            offStatus = "danger",
            onLabel = "Yes",
            offLabel = "No",
            value = TRUE
          )
        ),

        shiny::div(
          id = "hid_options",

          shiny::strong("Then, please indicate how long it was (in the min:sec format)", shiny::icon("info-circle"), ":") %>%
            bsplus::bs_embed_tooltip(title = "Indicate length of last step (e.g., 2:15).", placement = "right"),

          shiny::textInput(
            inputId = "last_length_step",
            label = NULL
          )
        ) %>%
          shinyjs::hidden(),


        shinyWidgets::actionBttn(
          inputId = "btn_ready",
          label = "OK! I'm ready!",
          style = "jelly",
          color = "danger",
          size = "sm"
        )
      ),

      shiny::column(
        width = 6,

        shiny::p("Please, note that in Swimming you should input the intensity as m/s.\nAdditionally, make sure you include a baseline value (first row).") %>%
          shiny::div(id = "info_swimming") %>%
          shinyjs::hidden(),

        rhandsontable::rHandsontableOutput(outputId = "data_input"),

        shiny::br(),shiny::br(),

        shinyWidgets::actionBttn(
          inputId = "btn_done",
          label = "I'm done!",
          style = "jelly",
          color = "danger",
          size = "sm"
        ) %>%
          shinyjs::hidden()
      )

    )
  )

  server <- function(input, output, session) {

    r <- shiny::reactiveValues(
      data_protocol = NULL,
      data_out = NULL,
      last_length_step = NULL
    )

    # show option to input length of last step -------------------
    shiny::observe({
      shinyjs::toggle(id = "hid_options", anim = TRUE, asis = TRUE, condition = input$completed == FALSE)
    })

    # hide protocol options if swimming -------------------
    shiny::observe({
      shinyjs::toggle(id = "options_if_no_swimming", anim = TRUE, asis = TRUE, condition = input$choose_sport != "Swimming")
    })

    # show important info if swimming -------------------
    shiny::observe({
      shiny::req(input$choose_sport == "Swimming", input$btn_ready)

      shinyjs::show(id = "info_swimming", anim = TRUE, asis = TRUE)
    })

    # data for data input ------------------------
    data_protocol <- shiny::reactive({
      helper_data_protocol(
        input_steps = input$steps,
        input_length_steps = input$length_steps,
        input_starting_load = input$starting_load,
        input_step_increase = input$step_increase,
        input_heart_rate_data = input$heart_rate_data,
        input_completed = input$completed,
        input_last_length_step = input$last_length_step,
        sport = input$choose_sport
      )
    })

    # render table for data input -------------------
    output$data_input <- rhandsontable::renderRHandsontable({
      shiny::req(input$btn_ready)

      helper_render_table(.data = data_protocol(), sport = input$choose_sport)
    })


    # show done button once ready button is clicked -----------
    shiny::observeEvent(input$btn_ready, {
      shinyjs::show(id = "btn_done", anim = TRUE, asis = TRUE)
    })

    ## cancel button
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    ## reset button
    shiny::observeEvent(input$btn_reset,  {
      session$reload()
    })

    ## done button
    shiny::observeEvent(input$btn_done, {
      datapasta::tribble_paste(rhandsontable::hot_to_r(input$data_input))
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    app = ui,
    server = server,
    viewer = shiny::dialogViewer(dialogName = "Data Input", width = width, height = height),
    stopOnCancel = FALSE
  )

}
