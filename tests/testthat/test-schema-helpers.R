source(test_path("../../functions/schema_helpers.R"))

test_that("imgdata base schema preserves existing column contract", {
  expect_equal(length(IMGDATA_BASE_COLUMNS), 23)
  expect_true(all(c(
    "FileName",
    "ImageNum",
    "datetime_utc",
    "laser_alt_m",
    "barometric_alt_m",
    "pixel_dimension_mm"
  ) %in% IMGDATA_BASE_COLUMNS))
})

test_that("photos measured schema includes grading and measurement columns", {
  expect_true(all(IMGDATA_BASE_COLUMNS %in% PHOTOS_MEASURED_COLUMNS))
  expect_true(all(c(
    "EGNO",
    "photogram_quality",
    "photogram_comments",
    "TL",
    "TL_w05.00",
    "TL_w95.00",
    "W.fluke"
  ) %in% PHOTOS_MEASURED_COLUMNS))
})

test_that("schema helpers detect and add missing columns", {
  df <- data.frame(FileName = "MAX_0001.JPG", stringsAsFactors = FALSE)
  missing <- schema_missing_columns(df, c("FileName", "laser_alt_m"))

  expect_equal(missing, "laser_alt_m")
  expect_false(schema_has_columns(df, c("FileName", "laser_alt_m")))

  out <- ensure_columns(df, c("FileName", "laser_alt_m"))
  expect_true("laser_alt_m" %in% names(out))
  expect_true(is.na(out$laser_alt_m[1]))
})

test_that("barometric offset preserves missing values and offsets valid values", {
  expect_equal(apply_barometric_offset(c(10, NA, 20), 1.5), c(11.5, NA, 21.5))
  expect_equal(apply_barometric_offset(c(10, 20), NA), c(10, 20))
})

test_that("report messages are formatted for Shiny display", {
  report <- make_report()
  report <- add_report_message(report, "warning", "Missing altitude", "example.csv", 3)

  formatted <- format_report_messages(report)
  expect_match(formatted, "\\[WARNING\\] Missing altitude")
  expect_match(formatted, "example.csv")
})

test_that("backup_existing_file copies an existing file with timestamped name", {
  src <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", src)

  backup <- backup_existing_file(src, timestamp = "20260130_120000")

  expect_true(file.exists(backup))
  expect_match(basename(backup), "\\.backup-20260130_120000\\.csv$")
  expect_equal(readLines(backup), readLines(src))
})
