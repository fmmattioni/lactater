#' A report of a step test, as a web page
#'
#' Writes the default report of a step test in one call: the header with the athlete's details, LT1 and LT2 with what
#' they mean, the training zones, then on a second page the predicted performance, what to double-check, the test,
#' and every method. Open it in a browser, or print it to PDF from there (two A4 pages for a typical test), or set
#' `pdf = TRUE`. To choose the sections, their order, or to add your lab's name and logo, build the report step by
#' step with [report_start()]. Needs the gt package.
#'
#' @param x The result of [lactate_threshold()].
#' @param file Where to write the report, an `.html` file.
#' @param athlete A named list for the header, passed to [report_header()], e.g. `list(name = "Ana Silva", date = "3
#'   November 2026", sex = "female", age = 27, body_mass = 61)`.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, as in [lactate_zones()].
#' @param protocol One line on how the test was done: ergometer, stages, sampling site, analyzer.
#' @param notes Free text for the end of the report.
#' @param pdf Whether to print a PDF next to the HTML file as well. Needs the pagedown package and Google Chrome or
#'   Chromium.
#' @param open Whether to open the report in the browser.
#'
#' @return the path of the report, invisibly.
#' @seealso [report_start()] to build a report step by step.
#' @export
#'
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' results <- lactate_threshold(
#'   demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
#' )
#' report <- test_report(
#'   results,
#'   file = tempfile(fileext = ".html"),
#'   athlete = list(name = "Demo athlete", date = "3 November 2026", body_mass = 72),
#'   lt1 = "LTP1", lt2 = "ModDmax",
#'   protocol = "Cycle ergometer, 3-min stages of +25 W from 50 W, earlobe capillary blood",
#'   open = FALSE
#' )
test_report <- function(x, file = "lactate-test-report.html", athlete = list(), lt1 = "LTP1", lt2 = "LTP2",
                        protocol = NULL, notes = NULL, pdf = FALSE, open = interactive()) {
  report <- do.call(report_header, c(list(report_start(x, lt1 = lt1, lt2 = lt2)), athlete)) |>
    report_summary() |>
    report_zones() |>
    report_page_break() |>
    report_prediction() |>
    report_checks() |>
    report_test(protocol = protocol) |>
    report_methods()
  if(!is.null(notes)) report <- report_text(report, "Notes", notes)
  report_save(report, file = file, pdf = pdf, open = open)
}

#' Build a test report step by step
#'
#' Start a report with `report_start()`, then add the parts you want, in the order you want them, and write it with
#' `report_save()`. Printing a report previews it in the viewer. [test_report()] builds the default report in one call.
#'
#' * `report_header()`: who was tested and where. The athlete's name, the date, any other details you name (`sex`,
#'   `age`, `club`, `tester`...), your lab or institution, and its logo.
#' * `report_summary()`: the answer: LT1 and LT2 as the title, what they mean, and LT1, LT2 and the peak with their
#'   lactate, heart rate, RPE, percentage of peak and, with `body_mass`, per kg.
#' * `report_zones()`: the zones chart and table, three or five zones.
#' * `report_prediction()`: the predicted race performance, from the chosen thresholds when an equation takes them.
#' * `report_checks()`: what to double-check in the data.
#' * `report_test()`: the protocol and the measured stages.
#' * `report_methods()`: the threshold of every method.
#' * `report_text()`: a section of your own, such as notes or a training plan.
#' * `report_page_break()`: start a new page when printed.
#'
#' @param x The result of [lactate_threshold()].
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, as in [lactate_zones()].
#' @param report A report from `report_start()`.
#' @param name The athlete's name, the heading of the report.
#' @param date The date of the test, as you want it printed.
#' @param ... Other details to list under the name, as `label = value`, e.g. `sex = "female"`, `age = 27`, `club =
#'   "Tuebingen RC"`. `body_mass` (kg, a number) also gives the thresholds per kg.
#' @param lab Your lab, institution or business, printed at the top.
#' @param logo The path to a logo image (PNG or JPEG), printed at the top right.
#' @param model `3` or `5` zones.
#' @param chart Whether to draw the zones chart above the table.
#' @param protocol One line on how the test was done: ergometer, stages, sampling site, analyzer.
#' @param title The heading of a section of your own.
#' @param text Its text: one string per paragraph.
#' @param file Where to write the report, an `.html` file.
#' @param pdf Whether to print a PDF next to the HTML file as well. Needs the pagedown package and Google Chrome or
#'   Chromium.
#' @param open Whether to open the report in the browser.
#'
#' @return a `lactate_test_report` to add to, and for `report_save()` the path of the report, invisibly.
#' @name report_start
#' @export
#'
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' results <- lactate_threshold(
#'   demo_data, intensity, lactate, heart_rate, rpe_column = rpe, plot = FALSE
#' )
#' report <- report_start(results, lt1 = "LTP1", lt2 = "ModDmax") |>
#'   report_header(
#'     name = "Demo athlete", date = "3 November 2026", sex = "male", age = 31, body_mass = 72,
#'     lab = "Exercise physiology lab"
#'   ) |>
#'   report_summary() |>
#'   report_zones(model = 3) |>
#'   report_text("Training", "Two sessions a week in zone 2, one in zone 3.")
#' report_save(report, tempfile(fileext = ".html"), open = FALSE)
report_start <- function(x, lt1 = "LTP1", lt2 = "LTP2") {
  rlang::check_installed("gt", reason = "to build the report.")
  info <- lactater_info(x)
  structure(
    list(
      x = x, info = info, three = lactate_zones(x, lt1 = lt1, lt2 = lt2),
      five = lactate_zones(x, lt1 = lt1, lt2 = lt2, model = 5), header = list(), sections = list()
    ),
    class = "lactate_test_report"
  )
}

#' @rdname report_start
#' @export
report_header <- function(report, name = NULL, date = NULL, ..., lab = NULL, logo = NULL) {
  check_report(report)
  if(!is.null(logo) && !file.exists(logo)) stop(sprintf("Cannot find the logo `%s`.", logo), call. = FALSE)
  report$header <- list(name = name, date = date, details = list(...), lab = lab, logo = logo)
  report
}

#' @rdname report_start
#' @export
report_summary <- function(report) {
  add_section(report, "summary", function(r) {
    htmltools::tagList(
      htmltools::tags$h2(class = "claim", html_title(r)),
      htmltools::tags$p(class = "lede", htmltools::HTML(html_lede(r))),
      html_stats(r)
    )
  })
}

#' @rdname report_start
#' @export
report_zones <- function(report, model = 5, chart = TRUE) {
  if(!identical(as.numeric(model), 3) && !identical(as.numeric(model), 5))
    stop("`model` must be 3 or 5.", call. = FALSE)
  add_section(report, "zones", function(r) {
    zones <- if(model == 5) r$five else r$three
    htmltools::tagList(
      if(chart) html_figure(zones),
      htmltools::tags$h2(sprintf("%s training zones from these two thresholds", if(model == 5) "Five" else "Three")),
      html_zones(zones, r$info$settings$sport)
    )
  })
}

#' @rdname report_start
#' @export
report_prediction <- function(report) {
  add_section(report, "prediction", html_prediction)
}

#' @rdname report_start
#' @export
report_checks <- function(report) {
  add_section(report, "checks", function(r) html_checks(r$x))
}

#' @rdname report_start
#' @export
report_test <- function(report, protocol = NULL) {
  add_section(report, "test", function(r) {
    htmltools::tagList(
      htmltools::tags$h2("The test"),
      if(!is.null(protocol)) htmltools::tags$p(protocol),
      html_stages(r$info)
    )
  })
}

#' @rdname report_start
#' @export
report_methods <- function(report) {
  add_section(report, "methods", function(r) {
    htmltools::tagList(htmltools::tags$h2("Every method, for the record"), html_methods(r$x, r$info))
  })
}

#' @rdname report_start
#' @export
report_text <- function(report, title, text) {
  add_section(report, "text", function(r) {
    htmltools::tagList(htmltools::tags$h2(title), lapply(text, htmltools::tags$p))
  })
}

#' @rdname report_start
#' @export
report_page_break <- function(report) {
  add_section(report, "page break", function(r) htmltools::tags$div(class = "page-break"))
}

#' @rdname report_start
#' @export
report_save <- function(report, file = "lactate-test-report.html", pdf = FALSE, open = interactive()) {
  check_report(report)
  ## the page carries everything it needs (styles, tables, images), so it is written as it is
  writeLines(c("<!DOCTYPE html>", as.character(htmltools::doRenderTags(html_page(report)))), file, useBytes = TRUE)
  if(pdf) {
    rlang::check_installed("pagedown", reason = "to print the report to PDF.")
    pagedown::chrome_print(file, output = paste0(tools::file_path_sans_ext(file), ".pdf"), verbose = 0)
  }
  if(open) utils::browseURL(file)
  invisible(file)
}

#' @export
print.lactate_test_report <- function(x, ...) {
  parts <- vapply(x$sections, function(s) attr(s, "part"), "")
  cat(sprintf("A test report with %s.\n", if(length(parts)) paste(parts, collapse = ", ") else "no sections yet"))
  if(interactive() && length(x$sections)) htmltools::html_print(html_page(x))
  invisible(x)
}

#' Report internals
#'
#' `check_report()` stops when `report` is not one; `add_section()` adds a section that is drawn when the report is
#' saved, so the header can come in any order; `html_*()` draw the parts. For internal use only.
#'
#' @param report,r A report from [report_start()].
#' @param part The name of the section, for printing.
#' @param draw A function of the report that returns the section's HTML.
#' @param info The `lactater` attribute of the results.
#' @param zones The result of [lactate_zones()].
#' @param x The result of [lactate_threshold()].
#' @param sport The sport.
#' @param boundary 1 for LT1, 2 for LT2.
#' @name report_internals
#' @keywords internal
NULL

#' @rdname report_internals
check_report <- function(report) {
  if(!inherits(report, "lactate_test_report")) stop("`report` must come from `report_start()`.", call. = FALSE)
}

#' @rdname report_internals
add_section <- function(report, part, draw) {
  check_report(report)
  attr(draw, "part") <- part
  report$sections <- c(report$sections, list(draw))
  report
}

#' @rdname report_internals
html_page <- function(report) {
  tags <- htmltools::tags
  css <- paste(readLines(system.file("report", "report.css", package = "lactater")), collapse = "\n")
  info <- report$info
  tags$html(
    lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(paste(c("Lactate step test", report$header$name), collapse = ", ")),
      tags$link(rel = "stylesheet", href = paste0(
        "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@500&family=IBM+Plex+Sans:wght@400;600",
        "&display=swap"
      )),
      tags$style(htmltools::HTML(css))
    ),
    tags$body(
      html_header(report),
      tags$main(lapply(report$sections, function(draw) draw(report))),
      tags$footer(sprintf(
        "Made with lactater %s on %s. Thresholds from a %s fitted to %d stages.", utils::packageVersion("lactater"),
        trimws(format(Sys.Date(), "%e %B %Y")), info$settings$fit, sum(info$data$intensity > 0)
      ))
    )
  )
}

#' @rdname report_internals
html_header <- function(report) {
  tags <- htmltools::tags
  header <- report$header
  details <- c(list(date = header$date, sport = tools::toTitleCase(report$info$settings$sport)), header$details)
  if(!is.null(details$body_mass)) details$body_mass <- paste(details$body_mass, "kg")
  details <- Filter(Negate(is.null), details)
  labels <- sub("^(.)", "\\U\\1", gsub("_", " ", names(details)), perl = TRUE)
  tags$header(
    class = "band",
    if(!is.null(header$lab) || !is.null(header$logo)) {
      tags$div(
        class = "band-top",
        tags$span(class = "lab", header$lab),
        if(!is.null(header$logo)) htmltools::HTML(gt::local_image(header$logo, height = 36))
      )
    },
    tags$h1(if(!is.null(header$name)) header$name else "Lactate step test"),
    tags$dl(unname(Map(function(label, value) tags$div(tags$dt(label), tags$dd(paste(value, collapse = ", "))),
                       labels, details)))
  )
}

#' @rdname report_internals
html_method <- function(zones, boundary) {
  gsub("Dmax", "D<sub>max</sub>", attr(zones, "lactater_zones")$labels[boundary], fixed = TRUE)
}

#' @rdname report_internals
html_title <- function(r) {
  lt <- attr(r$three, "lactater_zones")$lt
  sport <- r$info$settings$sport
  sprintf("LT1 at %s, LT2 at %s", intensity_text(lt[1], sport), intensity_text(lt[2], sport))
}

#' @rdname report_internals
html_lede <- function(r) {
  stages <- r$info$data[r$info$data$intensity > 0, ]
  sport <- r$info$settings$sport
  sprintf(
    paste(
      "A %s step test of %d stages, up to %s. LT1 (%s) and LT2 (%s) split the intensities into three domains: below",
      "LT1 exercise is moderate, between them heavy, and above LT2 severe, where lactate keeps rising even at a",
      "constant intensity."
    ),
    sport, nrow(stages), intensity_text(max(stages$intensity), sport), html_method(r$three, 1),
    html_method(r$three, 2)
  )
}

#' @rdname report_internals
html_stats <- function(r) {
  tags <- htmltools::tags
  info <- r$info
  three <- r$three
  sport <- info$settings$sport
  peak <- max(info$data$intensity)
  mass <- suppressWarnings(as.numeric(r$header$details$body_mass)[1])
  if(length(mass) == 0) mass <- NA_real_
  per_kg <- function(v) if(!is.na(mass) && sport %in% c("cycling", "rowing")) sprintf("%.1f W/kg", v / mass)
  stat <- function(label, value, details) {
    tags$div(
      class = "stat",
      tags$p(class = "stat-label", htmltools::HTML(label)),
      tags$p(class = "stat-value", value),
      lapply(details, function(d) {
        if(length(d)) tags$p(class = "stat-detail", htmltools::HTML(paste(d, collapse = " \u00b7 ")))
      })
    )
  }
  boundary <- function(k) {
    row <- three[k + 1, ]
    stat(
      sprintf("LT%d, %s", k, html_method(three, k)),
      intensity_text(row$intensity_from, sport),
      list(
        c(
          per_kg(row$intensity_from),
          if(sport != "cycling") format_pace(convert_to_pace(row$intensity_from, sport), sport),
          sprintf("%.0f%% of peak", 100 * row$intensity_from / peak)
        ),
        c(
          sprintf("%.1f mmol/L", row$lactate_from),
          if("heart_rate_from" %in% names(row)) sprintf("%.0f bpm", row$heart_rate_from),
          if("rpe_from" %in% names(row)) sprintf("RPE %.0f", row$rpe_from)
        )
      )
    )
  }
  tags$div(
    class = "stats",
    boundary(1),
    boundary(2),
    stat(
      "Peak",
      intensity_text(peak, sport),
      list(
        per_kg(peak),
        c(
          sprintf("%.1f mmol/L", max(info$data$lactate)),
          if("heart_rate" %in% names(info$data)) {
            sprintf("HR<sub>max</sub> %.0f bpm", max(info$data$heart_rate, na.rm = TRUE))
          }
        )
      )
    )
  )
}

#' @rdname report_internals
html_figure <- function(zones) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  plot <- autoplot(zones) + ggplot2::labs(title = NULL, subtitle = NULL)
  ggplot2::ggsave(path, plot, width = 8, height = 2.8, dpi = 200)
  htmltools::tags$figure(
    ## gt embeds the image in the page, so the report stays one file
    htmltools::HTML(gt::local_image(path, height = "auto")),
    htmltools::tags$figcaption(
      "The lactate curve of the test, with the zones in the colors of their domain: green moderate, yellow heavy,",
      "red severe."
    )
  )
}

#' @rdname report_internals
html_zones <- function(zones, sport) {
  unit <- intensity_unit(sport)
  span <- function(from, to, digits, format = function(v) formatC(v, format = "f", digits = digits)) {
    both <- paste0(format(from), "\u2013", format(to))
    ifelse(is.na(from) & is.na(to), "", ifelse(
      is.na(from), paste("<", format(to)), ifelse(is.na(to), paste(">", format(from)), both)
    ))
  }
  n <- nrow(zones)
  domain <- match(zones$domain, unique(zones$domain))
  table <- data.frame(
    zone = zones$zone,
    description = ifelse(zones$description == "VO2max", "V&#775;O<sub>2max</sub>", zones$description),
    intensity = paste(span(zones$intensity_from, zones$intensity_to, intensity_digits(sport)), unit)
  )
  if(n == 5) {
    table$domain <- sprintf("%s domain, zone %d of the three-zone model", tools::toTitleCase(zones$domain), domain)
  }
  if("pace_from" %in% names(zones)) {
    table$pace <- span(zones$pace_to, zones$pace_from, 0, function(v) format_pace(v, sport))
  }
  if("heart_rate_from" %in% names(zones)) table$heart_rate <- span(zones$heart_rate_from, zones$heart_rate_to, 0)
  if("rpe_from" %in% names(zones)) table$rpe <- span(zones$rpe_from, zones$rpe_to, 0)
  table$lactate <- span(zones$lactate_from, zones$lactate_to, 1)
  numeric <- intersect(c("intensity", "pace", "heart_rate", "rpe", "lactate"), names(table))
  out <- gt::gt(table, groupname_col = if(n == 5) "domain") |>
    gt::fmt_markdown(columns = "description") |>
    gt::cols_align("right", columns = numeric) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(columns = "zone")) |>
    gt::cols_label(.list = c(
      list(zone = "", description = "", intensity = "Intensity"),
      if(!is.null(table$pace)) list(pace = "Pace"),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = "RPE"),
      list(lactate = gt::html("Lactate (mmol&middot;L<sup>&minus;1</sup>)"))
    ))
  ## the zone cell in the color of its band in the chart
  colours <- zone_colours[[as.character(n)]]
  for(k in seq_len(n)) {
    out <- gt::tab_style(
      out, gt::cell_fill(color = colours[k], alpha = 0.3), gt::cells_body(columns = "zone", rows = k)
    )
  }
  html_table(out)
}

#' @rdname report_internals
html_prediction <- function(r) {
  tags <- htmltools::tags
  sport <- r$info$settings$sport
  chosen <- attr(r$three, "lactater_zones")$labels
  notes <- character()
  prediction <- tryCatch(
    withCallingHandlers(predict_performance(r$x), warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) conditionMessage(e)
  )
  if(is.character(prediction)) {
    return(htmltools::tagList(tags$h2("Predicted performance"), tags$p(prediction)))
  }
  ## the predictions from LT1 and LT2 when an equation takes them, otherwise from every threshold that has one
  from_chosen <- prediction$method %in% chosen
  if(any(from_chosen)) prediction <- prediction[from_chosen, ]
  unit <- intensity_unit(sport)
  outside <- grepl("outside the range", notes)
  extrapolated <- vapply(prediction$method, function(m) any(startsWith(notes[outside], paste0(m, " ("))), logical(1))
  boundary <- match(prediction$method, chosen)
  method <- gsub("Dmax", "D<sub>max</sub>", prediction$method, fixed = TRUE)
  table <- data.frame(
    event = prediction$event,
    from = paste0(
      ifelse(is.na(boundary), "", paste0("LT", boundary, ", ")), method, " at ",
      format_intensity(prediction$threshold, sport), " ", unit
    )
  )
  times <- "time" %in% names(prediction)
  if(times) {
    low <- prediction$time_low
    high <- prediction$time_high
    table$expected <- prediction$time
  } else {
    low <- format_intensity(prediction$intensity_low, sport)
    high <- paste(format_intensity(prediction$intensity_high, sport), unit)
    table$expected <- paste(format_intensity(prediction$intensity, sport), unit)
  }
  table$range <- paste(low, "to", high)
  table$expected <- paste0(table$expected, ifelse(extrapolated, " \u2020", ""))
  out <- gt::gt(table) |>
    gt::fmt_markdown(columns = "from") |>
    gt::cols_align("right", columns = c("expected", "range")) |>
    gt::cols_label(
      event = "Race", from = "From the threshold", expected = if(times) "Expected time" else "Expected average power",
      range = "95 of 100 athletes"
    ) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(columns = "expected"))
  if(any(extrapolated)) {
    out <- gt::tab_source_note(out, paste(
      "\u2020 The threshold lies outside the range of the athletes the equation was built on: an extrapolation, to",
      "read with care."
    ))
  }
  for(note in unique(notes[!outside])) out <- gt::tab_source_note(out, note)
  who <- if(is.na(boundary[1])) "this threshold" else sprintf("an LT%d like this one", boundary[1])
  expected <- sub(" \u2020$", "", table$expected[1])
  reading <- if(times) {
    sprintf("How to read it: athletes with %s ran the %s in %s on average; 95 of 100 of them took between %s and %s.",
            who, prediction$event[1], expected, low[1], high[1])
  } else {
    sprintf("How to read it: athletes with %s averaged %s in a %s; 95 of 100 of them averaged between %s and %s.",
            who, expected, tolower(prediction$event[1]), low[1], high[1])
  }
  htmltools::tagList(
    tags$h2("Predicted performance"),
    tags$p(sprintf(
      "What the threshold predicts for a race, from the equations of %s.",
      paste(unique(prediction$source), collapse = " and ")
    )),
    html_table(out),
    tags$p(class = "caption", reading)
  )
}

#' @rdname report_internals
html_checks <- function(x) {
  tags <- htmltools::tags
  checks <- lactate_checks(x)
  flagged <- checks[checks$status != "ok", ]
  summary <- sprintf("%d of %d checks of the data passed.", sum(checks$status == "ok"), nrow(checks))
  if(nrow(flagged) == 0) {
    return(htmltools::tagList(tags$h2("What to double-check"), tags$p(paste(summary, "Nothing needs a second look."))))
  }
  ## text glyphs, which take the color (the heavy check mark and the warning sign often show as emoji)
  symbols <- c(note = "i", warning = "!")
  colours <- c(note = "#1f5fa8", warning = "#b45309")
  table <- data.frame(status = symbols[flagged$status], check = flagged$check, details = flagged$details)
  out <- gt::gt(table) |>
    gt::cols_label(status = "", check = "", details = "") |>
    gt::cols_width(status ~ gt::pct(4), check ~ gt::pct(22), details ~ gt::pct(74))
  for(s in names(symbols)) {
    out <- gt::tab_style(out, gt::cell_text(color = colours[[s]], weight = 600),
                         gt::cells_body("status", rows = flagged$status == s))
  }
  htmltools::tagList(tags$h2("What to double-check"), tags$p(summary), html_table(out))
}

#' @rdname report_internals
html_stages <- function(info) {
  data <- info$data
  sport <- info$settings$sport
  table <- data.frame(
    stage = ifelse(data$intensity == 0, "Rest", as.character(cumsum(data$intensity > 0))),
    intensity = format_intensity(data$intensity, sport),
    lactate = sprintf("%.2f", data$lactate)
  )
  if("heart_rate" %in% names(data)) table$heart_rate <- data$heart_rate
  if("rpe" %in% names(data)) table$rpe <- data$rpe
  out <- gt::gt(table) |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_align("right", columns = -"stage") |>
    gt::cols_label(.list = c(
      list(
        stage = "Stage", intensity = paste0("Intensity (", intensity_unit(sport), ")"),
        lactate = gt::html("Lactate (mmol&middot;L<sup>&minus;1</sup>)")
      ),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = sprintf("RPE (%s)", info$settings$rpe_scale))
    ))
  html_table(out, size = 12, compact = TRUE)
}

#' @rdname report_internals
html_methods <- function(x, info) {
  sport <- info$settings$sport
  subscript <- function(v) gsub("Dmax", "D<sub>max</sub>", v, fixed = TRUE)
  family <- as.character(x$method_category)
  ## the family named once, on its first row, so the table stays short
  table <- data.frame(
    family = ifelse(duplicated(family), "", subscript(family)),
    method = subscript(as.character(x$method)),
    intensity = ifelse(is.na(x$intensity), "not found", format_intensity(x$intensity, sport)),
    ## one decimal, or two when needed (Bsln+ targets such as 1.43)
    lactate = ifelse(is.na(x$lactate), "", sprintf(
      ifelse(abs(x$lactate * 10 - round(x$lactate * 10)) > 1e-8, "%.2f", "%.1f"), x$lactate
    ))
  )
  if("heart_rate" %in% names(x)) table$heart_rate <- x$heart_rate
  if("rpe" %in% names(x)) table$rpe <- x$rpe
  rule <- which(!duplicated(family))[-1]
  out <- gt::gt(table) |>
    gt::sub_missing(missing_text = "") |>
    gt::fmt_markdown(columns = c("family", "method")) |>
    gt::cols_align("right", columns = -c("family", "method")) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(columns = "family")) |>
    gt::tab_style(gt::cell_borders(sides = "top", color = "#d9dde3"), gt::cells_body(rows = rule)) |>
    gt::cols_label(.list = c(
      list(
        family = "", method = "Method", intensity = paste0("Intensity (", intensity_unit(sport), ")"),
        lactate = gt::html("Lactate (mmol&middot;L<sup>&minus;1</sup>)")
      ),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = sprintf("RPE (%s)", info$settings$rpe_scale))
    )) |>
    gt::tab_options(table_body.hlines.style = "hidden")
  html_table(out, size = 11, compact = TRUE)
}

#' A table of the report
#'
#' @param table A gt table.
#' @param size The font size, in pixels.
#' @param compact Whether to set the rows closer, for the tables kept for the record.
#' @return its HTML.
#' @keywords internal
html_table <- function(table, size = 13, compact = FALSE) {
  table <- gt_lactater(table, size = size)
  if(compact) table <- gt::tab_options(table, data_row.padding = gt::px(2), row_group.padding = gt::px(2))
  htmltools::HTML(gt::as_raw_html(table, inline_css = FALSE))
}
