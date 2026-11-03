#' Lactate threshold report table
#'
#' Formats the result of [lactate_threshold()] as a publication-ready table: thresholds grouped by method
#' family, with the confidence interval when available, and the checks that need attention as notes.
#' Needs the gt package.
#'
#' @param x The result of [lactate_threshold()].
#' @param title The table title, or `NULL` for none.
#'
#' @return a `gt_tbl` object (print it, or save it with `gt::gtsave()`).
#' @export
#'
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' lactate_report(results)
lactate_report <- function(x, title = "Lactate thresholds") {
  rlang::check_installed("gt", reason = "to build the report table.")
  info <- lactater_info(x)
  sport <- info$settings$sport
  unit <- intensity_unit(sport)
  digits <- intensity_digits(sport)
  value <- function(v) formatC(v, format = "f", digits = digits)

  table <- data.frame(
    category = as.character(x$method_category),
    ## with the subscript of the Dmax family
    method = method_html(as.character(x$method)),
    intensity = ifelse(is.na(x$intensity), "not found", value(x$intensity))
  )
  if("intensity_low" %in% names(x)) {
    interval <- sprintf("%s to %s", value(x$intensity_low), value(x$intensity_high))
    table$interval <- ifelse(is.na(x$intensity_low), "", interval)
  }
  if("pace" %in% names(x)) table$pace <- ifelse(is.na(x$pace), "", format_pace(x$pace, sport))
  ## one decimal, or two when needed (Bsln+ targets such as 1.43)
  two_decimals <- abs(x$lactate * 10 - round(x$lactate * 10)) > 1e-8
  table$lactate <- ifelse(is.na(x$lactate), "", sprintf(ifelse(two_decimals, "%.2f", "%.1f"), x$lactate))
  if("heart_rate" %in% names(x)) table$heart_rate <- x$heart_rate
  if("rpe" %in% names(x)) table$rpe <- x$rpe

  checks <- lactate_checks(x)
  flagged <- checks[checks$status != "ok", ]
  stages <- sum(info$data$intensity > 0)

  ## one row group per method family, built by hand so the labels can carry their subscripts
  families <- unique(table$category)
  report <- gt::gt(table[setdiff(names(table), "category")])
  for(family in families) {
    report <- gt::tab_row_group(
      report, label = gt::html(method_html(family)),
      rows = which(table$category == family), id = family
    )
  }
  report <- gt::row_group_order(report, families)
  if(!is.null(title)) {
    report <- gt::tab_header(
      report,
      title = title,
      subtitle = sprintf(
        "%s \u00b7 %s fit \u00b7 %d stages%s",
        tools::toTitleCase(sport), info$settings$fit, stages, if(any(info$data$intensity == 0)) " + rest" else ""
      )
    )
  }
  report <- report |>
    gt::sub_missing(missing_text = "") |>
    gt::fmt_markdown(columns = "method") |>
    gt::cols_label(
      method = "Method",
      intensity = sprintf("Intensity (%s)", unit),
      lactate = gt::html("Lactate (mmol&middot;L<sup>&minus;1</sup>)")
    ) |>
    gt::cols_align(align = "right", columns = -"method") |>
    gt_lactater()
  if(!is.null(table$interval)) {
    report <- gt::cols_label(report, interval = sprintf("%g%% CI", 100 * info$settings$ci_level))
  }
  if(!is.null(table$pace)) report <- gt::cols_label(report, pace = "Pace")
  if(!is.null(table$heart_rate)) report <- gt::cols_label(report, heart_rate = "Heart rate (bpm)")
  if(!is.null(table$rpe)) report <- gt::cols_label(report, rpe = sprintf("RPE (%s)", info$settings$rpe_scale))
  for(i in seq_len(nrow(flagged))) {
    symbol <- if(flagged$status[i] == "warning") "\u26a0" else "\u2139"
    report <- gt::tab_source_note(report, sprintf("%s %s: %s", symbol, flagged$check[i], flagged$details[i]))
  }
  gt::tab_source_note(report, "Made with lactater")
}

#' The look of lactater's tables
#'
#' Gives a gt table the look of the tables in lactater's reports and articles, so your own tables can match them:
#' IBM Plex Sans, the full width of the page, bold column labels over one dark rule, light rules between rows, bold
#' row groups, and no fills or colors. Needs the gt package.
#'
#' @param table A gt table.
#' @param size The font size, in pixels.
#'
#' @return the gt table.
#' @export
#'
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' gt::gt(head(demo_data)) |>
#'   gt_lactater()
gt_lactater <- function(table, size = 14) {
  rlang::check_installed("gt", reason = "to style the table.")
  ink <- lactater_colours("light")[["ink"]]
  table |>
    gt::opt_table_font(font = list(gt::google_font("IBM Plex Sans"), gt::default_fonts())) |>
    gt::tab_options(
      table.width = gt::pct(100), table.font.size = gt::px(size), table.font.color = ink,
      table.background.color = "transparent", table.border.top.style = "hidden",
      table.border.bottom.color = "#d9dde3", heading.align = "left", heading.title.font.weight = "bold",
      heading.border.bottom.style = "hidden", column_labels.font.weight = "bold",
      column_labels.border.top.style = "hidden", column_labels.border.bottom.color = ink,
      column_labels.border.bottom.width = gt::px(1.5), column_labels.vlines.style = "hidden",
      row_group.font.weight = "bold", row_group.background.color = "transparent",
      row_group.border.top.color = "#d9dde3", row_group.border.bottom.color = "#d9dde3",
      table_body.hlines.color = "#e8eaee", table_body.border.bottom.color = "#d9dde3", data_row.padding = gt::px(6),
      source_notes.font.size = gt::px(size - 2), footnotes.font.size = gt::px(size - 2)
    )
}
