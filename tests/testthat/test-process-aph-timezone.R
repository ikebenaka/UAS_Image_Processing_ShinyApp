library(lubridate)

source(test_path("../../functions/process_aph.R"))

test_that("choose_aph_exif_time_interpretation detects UTC camera timestamps", {
  choice <- choose_aph_exif_time_interpretation(
    datetime_original = c("2023:01:11 16:32:12", "2023:01:11 16:32:13"),
    trigger_seconds = c(16 * 3600 + 32 * 60 + 12, 16 * 3600 + 32 * 60 + 13)
  )

  expect_equal(choice$selected_timezone, "UTC")
  expect_equal(choice$utc_matches, 2)
  expect_equal(choice$nyc_matches, 0)
  expect_equal(choice$match_idx, c(1L, 2L))
  expect_equal(choice$justtime, c("16:32:12", "16:32:13"))
})

test_that("choose_aph_exif_time_interpretation keeps New York default when it matches better", {
  choice <- choose_aph_exif_time_interpretation(
    datetime_original = c("2023:01:11 11:32:12", "2023:01:11 11:32:13"),
    trigger_seconds = c(16 * 3600 + 32 * 60 + 12, 16 * 3600 + 32 * 60 + 13)
  )

  expect_equal(choice$selected_timezone, "America/New_York")
  expect_equal(choice$nyc_matches, 2)
  expect_equal(choice$utc_matches, 0)
  expect_equal(choice$match_idx, c(1L, 2L))
  expect_equal(choice$justtime, c("16:32:12", "16:32:13"))
})
