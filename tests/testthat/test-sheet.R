test_that("recording_sheet copies the sheet, the form and the template", {
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE))
  files <- recording_sheet(dir, open = FALSE)
  expect_named(files, c("sheet", "form", "template"))
  expect_true(all(file.exists(files)))
  expect_named(utils::read.csv(files[["template"]]), c("intensity", "lactate", "heart_rate", "rpe"))
  expect_error(recording_sheet(dir, open = FALSE), "already exists")
})

test_that("a test recorded with the form is read back whole, and goes into the analysis", {
  skip_if_not_installed("readxl")
  ## written by the form's own builder (data-raw/recording_fixture.js)
  test <- read_recording(test_path("testdata", "recording.xlsx"))
  expect_equal(test$stages$intensity, c(0, 50, 75, 100, 125, 150, 175, 191))
  expect_equal(test$stages$lactate[8], 8.64)
  expect_equal(test$recovery$time, c(1, 3, 5, 7, 10))
  expect_equal(
    test$athlete, list(name = "Ana Silva & <Co>", date = "2026-11-04", sex = "female", age = 27, body_mass = 61.5)
  )
  expect_equal(test$stage_duration, 3)
  expect_equal(test$site, "earlobe")
  expect_null(test$grade)
  expect_equal(
    test$protocol, "Lode Excalibur, 3-min stages of +25 W from 50 W, earlobe whole blood, Biosen C-Line, 08:45"
  )
  expect_equal(test$details$notes, "felt good; \"tough\" last stage")
  results <- lactate_threshold(
    test$stages, intensity, lactate, heart_rate, rpe_column = rpe, rpe_scale = test$rpe_scale, sport = test$sport,
    sample = test$sample, method = c("OBLA", "Stegmann"), recovery = test$recovery,
    stage_duration = test$stage_duration, site = test$site, grade = test$grade, plot = FALSE
  )
  expect_false(is.na(results$intensity[results$method == "Stegmann"]))
  expect_equal(attr(results, "lactater")$settings$site, "earlobe")
})

test_that("a CSV gives the stages and the defaults, and wrong files stop", {
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  utils::write.csv(demo_data[c("intensity", "lactate")], file, row.names = FALSE)
  test <- read_recording(file)
  expect_equal(nrow(test$stages), nrow(demo_data))
  expect_null(test$recovery)
  expect_equal(c(test$sport, test$sample, test$rpe_scale), c("cycling", "blood", "6-20"))
  expect_null(test$protocol)
  expect_length(test$athlete, 0)
  expect_error(read_recording("nope.xlsx"), "Cannot find")
  text <- tempfile(fileext = ".txt")
  writeLines("x", text)
  expect_error(read_recording(text), "xlsx")
  power <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(power = 1:3), power, row.names = FALSE)
  expect_error(read_recording(power), "intensity")
})

test_that("an incomplete last stage is time-weighted", {
  ## the nominal last stage: 200 W, of which 2 of 3 min were completed
  nominal <- transform(demo_data, intensity = replace(intensity, intensity == 191, 200))
  run <- function(data = nominal, ...) lactate_threshold(data, intensity, lactate, method = "Dmax", plot = FALSE, ...)
  weighted <- run(stage_duration = 3, last_stage_duration = 2)
  expect_equal(max(attr(weighted, "lactater")$data$intensity), 175 + 2 / 3 * 25)
  expect_equal(run(stage_duration = 3, last_stage_duration = 3)$intensity, run()$intensity)
  expect_error(run(last_stage_duration = 2), "stage_duration")
  expect_error(run(stage_duration = 3, last_stage_duration = 4), "last_stage_duration")
  expect_warning(run(demo_data, stage_duration = 3, last_stage_duration = 2), "time-weighted already")
  expect_error(
    lactate_threshold(lactate_tests, intensity, lactate, stage_duration = 3, last_stage_duration = 2, .by = "test"),
    "one test at a time"
  )
})

test_that("read_recording time-weights a whole last step, and keeps one that is already shorter", {
  skip_if_not_installed("readxl")
  read <- function(last) {
    file <- file.path(tempdir(), paste0(last, ".xlsx"))
    file.create(file)
    local_mocked_bindings(
      excel_sheets = function(...) c("stages", "test"),
      read_excel = function(path, sheet, ...) {
        if(sheet == "stages") return(data.frame(intensity = c(0, 150, 175, last), lactate = c(1, 4.2, 6.7, 8.6)))
        data.frame(field = c("stage_duration", "last_stage_s"), value = c("3", "120"))
      },
      .package = "readxl"
    )
    read_recording(file)
  }
  expect_message(test <- read(200), "191.7 instead of 200")
  expect_equal(max(test$stages$intensity), 175 + 2 / 3 * 25)
  expect_no_message(test <- read(191))
  expect_equal(max(test$stages$intensity), 191)
})
