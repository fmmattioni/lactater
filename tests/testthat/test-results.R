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
  headers <- vapply(table$scales$get_scales("x")$labels, function(e) paste(deparse(e), collapse = ""), "")
  expect_equal(headers, c(
    'atop("Lactate", "mmol/L")', 'atop("Heart rate", "bpm")', 'atop("% peak", "PO")', 'atop("%", HR[max])'
  ))
  ## every column, the outer ones included, sits inside the panel
  expect_equal(ggplot2::ggplot_build(table)$layout$panel_params[[1]]$x.range, c(0.5, 4.5))
  ## % HRmax: the heart rate at the threshold over the highest heart rate of the test
  expect_equal(table$data$label[table$data$column == 4 & table$data$row == max(table$data$row)],
               sprintf("%.0f", 100 * results$heart_rate[1] / max(demo_data$heart_rate)))
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

test_that("method labels carry their subscripts, and the arrow stops on the edge of the dot", {
  expect_equal(
    method_plotmath(c("ModDmax", "Log-Poly-ModDmax", "IAT", "VO2max", "LTratio")),
    c('"ModD"[max]*""', '"Log-Poly-ModD"[max]*""', '"IAT"', "dot(V)*O[2*max]", '"LT"[ratio]')
  )
  expect_equal(method_html(c("LTratio", "Log-Exp-ModDmax", "IAT")),
               c("LT<sub>ratio</sub>", "Log-Exp-ModD<sub>max</sub>", "IAT"))
  expect_type(method_label("Dmax", bold = TRUE), "expression")
  ## a label at the top left of a 2-inch box, and the dot at the bottom right
  pointer <- grid::gTree(
    from = c(0, 1), to = c(1, 0), bend = 0.15, colour = "black", gap = 2.6, cl = "lactater_pointer"
  )
  grDevices::pdf(NULL, width = 4, height = 4)
  grid::pushViewport(grid::viewport(width = grid::unit(2, "in"), height = grid::unit(2, "in")))
  arrow <- grid::makeContent(pointer)$children[[1]]
  grDevices::dev.off()
  end <- c(utils::tail(as.numeric(arrow$x), 1), utils::tail(as.numeric(arrow$y), 1))
  expect_equal(sqrt(sum((end - c(2, 0))^2)), 2.6 / 25.4)
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
