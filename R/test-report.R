#' A report of a step test, as a web page
#'
#' Writes the default report of a step test in one call: the header with the athlete's details, LT1 and LT2 with what
#' they mean, the training zones, the predicted performance, what to double-check, the test, and every method. Open it
#' in a browser, or print it to PDF from there, or set `pdf = TRUE`: two A4 pages, the answer and the zones on the
#' first, the record on the second (a section moves to the next page whole). To choose the sections, their order, or
#' to add your lab's name and logo, build the report step by step with [report_start()]. Needs the gt package.
#'
#' @param x The result of [lactate_threshold()], or a list of two to five of them with their `dates`.
#' @param file Where to write the report, an `.html` file.
#' @param athlete A named list for the header, passed to [report_header()], e.g. `list(name = "Ana Silva", date = "3
#'   November 2026", sex = "female", age = 27, body_mass = 61)`.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, as in [lactate_zones()].
#' @param protocol One line on how the test was done: ergometer, stages, sampling site, analyzer. Not shown for a
#'   series.
#' @param notes Free text for the end of the report.
#' @param pdf Whether to print a PDF next to the HTML file as well. Needs the pagedown package and Google Chrome or
#'   Chromium.
#' @param open Whether to open the report in the browser.
#' @param dates With a list of two to five tests in `x`, the date of each test (dates, or days since the first test).
#'   The report then starts with the progress over the tests, and describes the latest one.
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
#'   protocol = "Cycle ergometer, 3-min stages of +25 W from 50 W, earlobe capillary blood",
#'   open = FALSE
#' )
test_report <- function(x, file = "lactate-test-report.html", athlete = list(), lt1 = NULL, lt2 = NULL,
                        protocol = NULL, notes = NULL, pdf = FALSE, open = interactive(),
                        dates = NULL) {
  report <- do.call(report_header, c(list(report_start(x, lt1 = lt1, lt2 = lt2, dates = dates)), athlete))
  report <- if(is.null(report$series)) {
    report |>
      report_summary() |>
      report_zones() |>
      report_prediction() |>
      report_checks() |>
      report_test(protocol = protocol) |>
      report_methods()
  } else {
    ## a series: the progress first, then the latest test, which follows on from the latest change; the zones chart
    ## of every test is already in the progress
    report |>
      report_progress() |>
      report_summary() |>
      report_zones(chart = FALSE) |>
      report_checks()
  }
  if(!is.null(notes)) report <- report_text(report, "Notes", notes)
  report_save(report, file = file, pdf = pdf, open = open)
}

#' Build a test report step by step
#'
#' Start a report with `report_start()`, then add the parts you want, in the order you want them, and write it with
#' `report_save()`. Printing a report previews it in the viewer. [test_report()] builds the default report in one call.
#'
#' To report a series of tests of one athlete, give `report_start()` a list of tests and their `dates`: the sections
#' above then describe the latest test, and `report_progress()` shows the progress over all of them.
#'
#' * `report_header()`: who was tested and where. The athlete's name, the date, any other details you name (`sex`,
#'   `age`, `club`, `tester`...), your lab or institution, and its logo.
#' * `report_summary()`: the answer: what LT1 and LT2 mean, and LT1, LT2 and the peak with their lactate, heart rate,
#'   RPE, percentage of peak and, with `body_mass`, per kg.
#' * `report_progress()`: for a series, how LT1 and LT2 changed since the first test, every lactate curve with the
#'   zones of each test, test by test, and whether the latest change is larger than the noise of the method
#'   ([compare_tests()]).
#' * `report_zones()`: the zones chart and table, three or five zones.
#' * `report_reference()`: for cycling, the percentile of the power at 3 mmol/L and at baseline + 1 mmol/L among people
#'   of the same age and sex in Germany ([reference_percentiles()]), from the `age`, `sex` and `body_mass` given to
#'   `report_header()`. For the general population, not for athletes.
#' * `report_prediction()`: the predicted race performance, from the chosen thresholds when an equation takes them.
#' * `report_checks()`: what to double-check in the data.
#' * `report_test()`: the protocol and the measured stages.
#' * `report_methods()`: the threshold of every method.
#' * `report_text()`: a section of your own, such as notes or a training plan.
#' * `report_page_break()`: start a new page when printed.
#'
#' @param x The result of [lactate_threshold()], or a list of two to five of them for a series of tests of one
#'   athlete.
#' @param lt1,lt2 The thresholds that mark LT1 and LT2, as in [lactate_zones()].
#' @param dates For a series, the date of each test: dates, or days since the first test. Without them, the names of
#'   `x` are read as dates.
#' @param cv The typical error of the methods, as in [compare_tests()]; the defaults cover only some methods.
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
#' report <- report_start(results) |>
#'   report_header(
#'     name = "Demo athlete", date = "3 November 2026", sex = "male", age = 31, body_mass = 72,
#'     lab = "Exercise physiology lab"
#'   ) |>
#'   report_summary() |>
#'   report_zones(model = 3) |>
#'   report_text("Training", "Two sessions a week in zone 2, one in zone 3.")
#' report_save(report, tempfile(fileext = ".html"), open = FALSE)
report_start <- function(x, lt1 = NULL, lt2 = NULL, dates = NULL) {
  rlang::check_installed("gt", reason = "to build the report.")
  series <- NULL
  if(!inherits(x, "lactate_thresholds") && is.list(x)) {
    series <- prepare_series(x, dates)
    ## one pair for the whole series: the one of the latest test, unless given
    pair <- chosen_pair(lactater_info(series$tests[[length(series$tests)]]), lt1, lt2)
    lt1 <- pair$lt1
    lt2 <- pair$lt2
    ## every test needs both thresholds, which is better found now than when the report is saved
    series$zones <- series_zones(series$tests, series$labels, lt1, lt2)
    x <- series$tests[[length(series$tests)]]
  }
  info <- lactater_info(x)
  pair <- chosen_pair(info, lt1, lt2)
  lt1 <- pair$lt1
  lt2 <- pair$lt2
  structure(
    list(
      x = x, info = info, series = series, lt = list(lt1, lt2), three = lactate_zones(x, lt1 = lt1, lt2 = lt2),
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
    labels <- r$series$labels
    htmltools::tagList(
      ## LT1 and LT2 are in the stats below; a series only needs to say which test this is
      if(!is.null(labels)) htmltools::tags$h2(sprintf("The latest test (%s)", labels[length(labels)])),
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
report_progress <- function(report, cv = NULL) {
  check_report(report)
  if(is.null(report$series))
    stop("`report_progress()` needs a series: give `report_start()` a list of tests and their `dates`.", call. = FALSE)
  add_section(report, "progress", function(r) html_progress(r, cv))
}

#' @rdname report_start
#' @export
report_reference <- function(report) {
  add_section(report, "reference", html_reference)
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
#' @param tests A list of results of [lactate_threshold()].
#' @param dates Their dates, or days since the first test.
prepare_series <- function(tests, dates) {
  if(length(tests) < 2 || length(tests) > 5 || !all(vapply(tests, inherits, logical(1), "lactate_thresholds")))
    stop("`x` must be one result of `lactate_threshold()`, or a list of two to five.", call. = FALSE)
  if(is.null(dates)) dates <- names(tests)
  if(is.null(dates) || length(dates) != length(tests))
    stop("Give each test its date with `dates`: dates, or days since the first test.", call. = FALSE)
  ## date-times, as Excel dates read by readxl, count as their dates
  if(inherits(dates, "POSIXt")) dates <- as.Date(dates)
  ## one explicit format: as.Date() alone reads "03/11/2026" as the year 3
  if(is.character(dates)) dates <- as.Date(dates, format = "%Y-%m-%d")
  days <- if(inherits(dates, "Date")) as.numeric(dates - min(dates)) else suppressWarnings(as.numeric(dates))
  if(anyNA(days) || anyDuplicated(days))
    stop("`dates` must be distinct dates (Date objects or \"YYYY-MM-DD\") or numbers of days.", call. = FALSE)
  sports <- unique(vapply(tests, function(t) lactater_info(t)$settings$sport, ""))
  if(length(sports) > 1) stop("The tests of a series must come from one sport.", call. = FALSE)
  labels <- date_labels(if(inherits(dates, "Date")) dates else days)
  order <- order(days)
  list(tests = unname(tests)[order], labels = labels[order])
}

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
      ## each section carries its part as a class, which the layout reads
      tags$main(lapply(report$sections, function(draw) {
        tags$section(class = gsub(" ", "-", attr(draw, "part")), draw(report))
      })),
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
  series <- report$series
  tests <- if(!is.null(series)) {
    sprintf("%d, %s to %s", length(series$labels), series$labels[1], series$labels[length(series$labels)])
  }
  details <- c(
    list(date = header$date, tests = tests, sport = tools::toTitleCase(report$info$settings$sport)), header$details
  )
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
  method_html(attr(zones, "lactater_zones")$labels[boundary])
}

#' @rdname report_internals
html_lede <- function(r) {
  stages <- r$info$data[r$info$data$intensity > 0, ]
  sport <- r$info$settings$sport
  ## the test in one line: the methods are named over the numbers below, and the domains under the figure
  sprintf("A %s step test of %d stages, up to %s.", sport, nrow(stages), intensity_text(max(stages$intensity), sport))
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
  ## how far the methods aimed at the same threshold lie apart, so the reader sees how much the choice matters; as a
  ## warning beside the number when the check of the data flags it
  checks <- lactate_checks(r$x)
  others <- function(k) {
    values <- info$intensity[as.integer(method_aim(names(info$intensity))) == k]
    values <- values[!is.na(values)]
    if(length(values) > 1) {
      range <- paste(format_intensity(min(values), sport), "to", intensity_text(max(values), sport))
      if(identical(checks$status[checks$check == paste0("LT", k)], "warning")) {
        sprintf(
          "<span class=\"stat-warning\"><span class=\"stat-flag\">!</span> Methods disagree: %s</span>",
          gsub(" ", "&nbsp;", range, fixed = TRUE)
        )
      } else {
        gsub(" ", "&nbsp;", sprintf("%d %s-rise methods: %s", length(values), c("first", "second")[k], range),
             fixed = TRUE)
      }
    }
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
        ),
        others(k)
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
  plot <- autoplot(zones)
  panel <- inherits(plot, "patchwork")
  html_plot(
    plot,
    paste(
      "The lactate curve with the zones in the colors of their domain: green moderate, yellow heavy, red severe.",
      if(panel) "Below, the heart rate of the stages, read at LT1 and LT2."
    ),
    height = if(panel) 4.4 else 3.2
  )
}

#' @rdname report_internals
#' @param plot A ggplot or a patchwork, drawn without its title and subtitle.
#' @param caption The text under it.
#' @param height Its height, in inches; it is 7 inches wide.
html_plot <- function(plot, caption, height = 3.5) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  untitled <- if(inherits(plot, "patchwork")) {
    patchwork::plot_annotation(title = NULL, subtitle = NULL)
  } else {
    ggplot2::labs(title = NULL, subtitle = NULL)
  }
  ggplot2::ggsave(path, plot + untitled, width = 7, height = height, dpi = 200)
  htmltools::tags$figure(
    ## gt embeds the image in the page, so the report stays one file
    htmltools::HTML(gt::local_image(path, height = "auto")),
    htmltools::tags$figcaption(caption)
  )
}

#' @rdname report_internals
html_progress <- function(r, cv) {
  tags <- htmltools::tags
  series <- r$series
  tests <- series$tests
  n <- length(tests)
  sport <- r$info$settings$sport
  unit <- intensity_unit(sport)
  signed <- function(v) paste(sub("^-", "\u2212", sprintf("%+.*f", intensity_digits(sport), v)), unit)
  ## LT1 and LT2 of every test from its zones, so that a number works as well as a method
  rows <- do.call(rbind, lapply(seq_len(n), function(k) {
    zones <- series$zones[[k]]
    data <- lactater_info(tests[[k]])$data
    data.frame(
      test = series$labels[k], lt1 = zones$intensity_from[3], lt2 = zones$intensity_from[4],
      heart_rate = if("heart_rate_from" %in% names(zones)) zones$heart_rate_from[4] else NA_real_,
      peak = max(data$intensity), peak_lactate = max(data$lactate)
    )
  }))
  chosen <- attr(r$three, "lactater_zones")$labels
  methods <- intersect(chosen, as.character(r$x$method))

  table <- data.frame(
    test = rows$test, lt1 = format_intensity(rows$lt1, sport), lt2 = format_intensity(rows$lt2, sport),
    heart_rate = ifelse(is.na(rows$heart_rate), "", sprintf("%.0f", rows$heart_rate)),
    peak = format_intensity(rows$peak, sport), peak_lactate = sprintf("%.1f", rows$peak_lactate)
  )
  if(all(table$heart_rate == "")) table$heart_rate <- NULL
  by_test <- gt::gt(table) |>
    gt::cols_align("right", columns = -"test") |>
    gt::tab_style(gt::cell_text(whitespace = "nowrap"), gt::cells_body(columns = "test")) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(rows = n)) |>
    gt::cols_label(.list = c(
      list(
        test = "Test", lt1 = gt::html(sprintf("LT1, %s (%s)", method_html(chosen[1]), unit)),
        lt2 = gt::html(sprintf("LT2, %s (%s)", method_html(chosen[2]), unit))
      ),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate at LT2 (bpm)"),
      list(
        peak = paste0("Peak (", unit, ")"),
        peak_lactate = "Peak lactate (mmol/L)"
      )
    ))

  ## the latest test against the one before, method by method, with the chosen ones first
  comparison <- compare_tests(tests[[n - 1]], tests[[n]], cv = cv)
  comparison <- comparison[comparison$method %in% methods | !is.na(comparison$verdict), ]
  comparison <- comparison[order(!comparison$method %in% methods), ]
  boundary <- match(as.character(comparison$method), chosen)
  latest <- data.frame(
    method = paste0(ifelse(is.na(boundary), "", paste0("LT", boundary, ", ")), method_html(comparison$method)),
    before = format_intensity(comparison$before, sport), after = format_intensity(comparison$after, sport),
    change = signed(comparison$change),
    interval = ifelse(
      is.na(comparison$change_low), "",
      paste(sub("^-", "\u2212", format_intensity(comparison$change_low, sport)), "to",
            sub("^-", "\u2212", format_intensity(comparison$change_high, sport)))
    ),
    ratio = ifelse(is.na(comparison$ratio), "", sprintf("%.1f", abs(comparison$ratio))),
    verdict = ifelse(is.na(comparison$verdict), "noise not known", as.character(comparison$verdict))
  )
  against <- gt::gt(latest) |>
    gt::fmt_markdown(columns = "method") |>
    gt::cols_align("right", columns = c("before", "after", "change", "interval", "ratio")) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(columns = "verdict", rows = latest$verdict ==
                                                                "beyond the noise")) |>
    gt::cols_label(
      method = "", before = series$labels[n - 1], after = series$labels[n], change = "Change",
      interval = "90% interval", ratio = gt::html("Change &divide; noise"), verdict = "Against the noise"
    )

  change <- rows[n, c("lt1", "lt2")] - rows[1, c("lt1", "lt2")]
  plot <- plot_series(tests, series$labels, r$lt[[1]], r$lt[[2]])
  with_hr <- length(plot) == 3
  htmltools::tagList(
    tags$h2(class = "claim", sprintf("Since the first test: LT1 %s, LT2 %s", signed(change$lt1), signed(change$lt2))),
    tags$p(class = "lede", htmltools::HTML(sprintf(
      paste(
        "%d tests from %s to %s, with LT1 as %s and LT2 as %s. A threshold also moves when fitness does not, so",
        "a change counts only when it is larger than the noise of the method."
      ),
      n, series$labels[1], series$labels[n], method_html(chosen[1]), method_html(chosen[2])
    ))),
    html_plot(
      plot,
      paste(
        "Every lactate curve: the latest test in blue, the first in dark gray, the tests between in light gray;",
        if(with_hr) "then the heart rate of the stages in the same colors, with LT1 and LT2 as filled points,",
        "and below them the five training zones of each test up to its highest stage, with LT1 and LT2 at their",
        "boundaries; dashed lines: 2 and 4 mmol/L."
      ),
      height = if(with_hr) 5 + 0.35 * n else 3.6 + 0.3 * n
    ),
    tags$h2("Test by test"),
    html_table(by_test),
    tags$h2(sprintf("The latest test against the one before (%s)", series$labels[n - 1])),
    html_table(against),
    tags$p(class = "caption", paste(
      "The noise is what two tests of this method usually differ by when nothing changed. A change is beyond the",
      "noise when its whole 90% interval lies on one side of zero (a change of more than 1.6 times the noise);",
      "a change within the noise may still be real: one pair of tests cannot tell."
    ))
  )
}

#' @rdname report_internals
html_reference <- function(r) {
  details <- r$header$details
  if(is.null(details$age) || is.null(details$sex)) {
    stop("`report_reference()` needs the athlete's `age` and `sex` in `report_header()`.", call. = FALSE)
  }
  ## a warning about the protocol goes into the report, under the table
  notes <- character()
  table <- withCallingHandlers(
    reference_percentiles(
      r$x, age = as.numeric(details$age), sex = as.character(details$sex),
      body_mass = if(!is.null(details$body_mass)) as.numeric(details$body_mass)
    ),
    warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  reference <- attr(table, "lactater_reference")
  ## one row per threshold: watts, and per kg beside them
  percentile <- ifelse(is.na(table$percentile), table$standing, sprintf("%.0f (%s)", table$percentile, table$standing))
  watts <- table$unit == "W"
  shown <- data.frame(
    threshold = ifelse(
      table$method[watts] == "OBLA 3.0", "At 3 mmol/L (OBLA 3.0)", "At baseline + 1 mmol/L (Bsln + 1.0)"
    ),
    power = sprintf("%.0f W", table$value[watts]), percentile = percentile[watts]
  )
  if(!all(watts)) {
    shown$per_kg <- sprintf("%.2f W/kg", table$value[!watts])
    shown$percentile_per_kg <- percentile[!watts]
  }
  out <- gt::gt(shown) |>
    gt::cols_align("right", columns = -"threshold") |>
    gt::cols_label(.list = list(
      threshold = "", power = "Power", percentile = "Percentile", per_kg = "Per kg", percentile_per_kg = "Percentile"
    )[names(shown)])
  htmltools::tagList(
    htmltools::tags$h2("Compared with the population"),
    html_table(out),
    htmltools::tags$p(class = "caption", paste(
      sprintf(
        "Percentiles of %s aged %s in Germany (Fiedler et al., 2025), tested on a cycle ergometer from 25 W with 25 W",
        if(reference$sex == "female") "women" else "men", format(reference$age)
      ),
      "more every 2 min. A different protocol shifts the comparison, and these are values of the general population,",
      "not of athletes.", paste(notes, collapse = " ")
    ))
  )
}

#' @rdname report_internals
html_zones <- function(zones, sport) {
  unit <- intensity_unit(sport)
  span <- function(from, to, digits, format = function(v) formatC(v, format = "f", digits = digits)) {
    ## a range whose ends print the same (RPE 9.6 to 10.4) is one number
    both <- ifelse(format(from) == format(to), format(from), paste0(format(from), "\u2013", format(to)))
    ## a dash where there is no value (the lactate of a zone below the first stage), not an empty cell
    ifelse(is.na(from) & is.na(to), "\u2013", ifelse(
      is.na(from), paste("<", format(to)), ifelse(is.na(to), paste(">", format(from)), both)
    ))
  }
  n <- nrow(zones)
  table <- data.frame(
    ## the domain named once, on its first zone, so the table stays short
    domain = ifelse(duplicated(zones$domain), "", tools::toTitleCase(zones$domain)),
    zone = zones$zone,
    description = ifelse(zones$description == "VO2max", "V&#775;O<sub>2max</sub>", zones$description),
    ## the units in the column labels, so a range fits on one line
    intensity = span(zones$intensity_from, zones$intensity_to, intensity_digits(sport))
  )
  if("pace_from" %in% names(zones)) {
    table$pace <- span(zones$pace_to, zones$pace_from, 0, function(v) sub(" .*", "", format_pace(v, sport)))
  }
  if("heart_rate_from" %in% names(zones)) table$heart_rate <- span(zones$heart_rate_from, zones$heart_rate_to, 0)
  ## RPE as whole numbers, unless two boundaries would then print the same ("< 10" over "10")
  if("rpe_from" %in% names(zones)) {
    table$rpe <- span(zones$rpe_from, zones$rpe_to, as.integer(anyDuplicated(round(zones$rpe_to[-n])) > 0))
  }
  table$lactate <- span(zones$lactate_from, zones$lactate_to, 1)
  numeric <- intersect(c("intensity", "pace", "heart_rate", "rpe", "lactate"), names(table))
  rule <- which(!duplicated(zones$domain))[-1]
  out <- gt::gt(table) |>
    gt::fmt_markdown(columns = "description") |>
    gt::cols_align("right", columns = numeric) |>
    gt::tab_style(gt::cell_text(weight = 600), gt::cells_body(columns = "zone")) |>
    gt::tab_style(gt::cell_text(whitespace = "nowrap"), gt::cells_body()) |>
    gt::tab_style(gt::cell_text(color = "#5b6672"), gt::cells_body(columns = "domain")) |>
    gt::tab_style(gt::cell_borders(sides = "top", color = "#9aa3ad"), gt::cells_body(rows = rule)) |>
    gt::cols_label(.list = c(
      list(domain = "Domain", zone = "", description = "", intensity = sprintf("Intensity (%s)", unit)),
      if(!is.null(table$pace)) list(pace = paste0("Pace (min", sub("^\\S+ ", "", format_pace(0, sport)), ")")),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = "RPE"),
      list(lactate = "Lactate (mmol/L)")
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
  extrapolated <- prediction$extrapolated
  boundary <- match(prediction$method, chosen)
  method <- method_html(prediction$method)
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
    gt::tab_options(column_labels.hidden = TRUE) |>
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
        lactate = "Lactate (mmol/L)"
      ),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = sprintf("RPE (%s)", sub("-", "\u2011", info$settings$rpe_scale)))
    ))
  html_table(out, size = 12, compact = TRUE)
}

#' @rdname report_internals
html_methods <- function(x, info) {
  sport <- info$settings$sport
  family <- as.character(x$method_category)
  ## the family named once, on its first row, so the table stays short; a family of one method is named once too
  method <- as.character(x$method)
  table <- data.frame(
    family = ifelse(duplicated(family), "", method_html(family)),
    method = ifelse(method == family, "", method_html(method)),
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
    gt::tab_style(gt::cell_text(whitespace = "nowrap"), gt::cells_body(columns = c("family", "method"))) |>
    gt::tab_style(gt::cell_borders(sides = "top", color = "#d9dde3"), gt::cells_body(rows = rule)) |>
    gt::cols_label(.list = c(
      list(
        family = "", method = "Method", intensity = paste0("Intensity (", intensity_unit(sport), ")"),
        lactate = "Lactate (mmol/L)"
      ),
      if(!is.null(table$heart_rate)) list(heart_rate = "Heart rate (bpm)"),
      if(!is.null(table$rpe)) list(rpe = sprintf("RPE (%s)", sub("-", "\u2011", info$settings$rpe_scale)))
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
