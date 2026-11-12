# Every fixture's full output is snapshotted, so each change shows its numeric diff in review.
for (name in names(fixtures)) {
  test_that(paste("fixture output:", name), {
    .data <- fixtures[[name]]
    run <- function() {
      args <- list(
        .data = .data, intensity_column = "intensity", lactate_column = "lactate",
        include_baseline = TRUE, sport = fixture_sport(name), plot = FALSE
      )
      if ("heart_rate" %in% names(.data)) args$heart_rate_column <- "heart_rate"
      out <- tryCatch(do.call(lactate_threshold, args), error = function(e) conditionMessage(e))
      if (is.character(out)) return(out)
      as.data.frame(out[intersect(c("method", "intensity", "pace", "lactate", "heart_rate"), names(out))])
    }
    expect_snapshot(print(run()))
  })
}
