results <- lactate_threshold(demo_data, "intensity", "lactate", "heart_rate", plot = FALSE)

test_that("lactate_threshold returns a lactate_thresholds tibble", {
  expect_s3_class(results, c("lactate_thresholds", "tbl_df"))
  expect_equal(names(results)[1:2], c("method_category", "method"))
  expect_match(pillar::tbl_sum(results), "cycling, 3rd degree polynomial, 7 stages \\+ rest")
})

test_that("autoplot draws the overview and single methods", {
  expect_s3_class(autoplot(results), "patchwork")
  ## the table beside the overview: one column per value, with heart rate when given
  table <- autoplot(results)[[4]]
  expect_equal(table$scales$get_scales("x")$labels, c("Lactate\nmmol/L", "Heart rate\nbpm", "% of\npeak"))
  ## every column, the outer ones included, sits inside the panel
  expect_equal(ggplot2::ggplot_build(table)$layout$panel_params[[1]]$x.range, c(0.5, 3.5))
  grDevices::pdf(NULL)
  expect_silent(print(autoplot(results)))
  grDevices::dev.off()
  expect_s3_class(autoplot(results, method = "Dmax"), "ggplot")
  expect_error(autoplot(results, method = "Nope"), "must be one of")
})

test_that("every method plots in both styles, labelled in whole watts", {
  old <- options(lactater.style = "night")
  on.exit(options(old))
  for(style in c("night", "light")) {
    options(lactater.style = style)
    for(m in as.character(results$method)) {
      p <- autoplot(results, method = m)
      expect_s3_class(p, "ggplot")
      expect_silent(ggplot2::ggplot_build(p))
    }
  }
  labels <- unlist(lapply(autoplot(results, "OBLA 4.0")$layers, function(l) l$aes_params$label))
  expect_true(paste(round(attr(results, "lactater")$intensity[["OBLA 4.0"]]), "W") %in% labels)
  expect_error(theme_lactater(style = "pink"), "should be one of")
})

test_that("autoplot explains when the attributes are gone", {
  stripped <- results
  attr(stripped, "lactater") <- NULL
  expect_error(autoplot(stripped), "lost its lactater attributes")
})

test_that("the plot column still holds one plot per threshold", {
  with_plots <- lactate_threshold(demo_data, "intensity", "lactate", method = "OBLA")
  expect_length(with_plots$plot, 5)
  expect_s3_class(with_plots$plot[[1]], "ggplot")
})
