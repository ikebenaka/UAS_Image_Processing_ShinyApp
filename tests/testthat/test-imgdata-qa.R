source(test_path("../../functions/schema_helpers.R"))

test_that("add_imgdata_qa_warnings adds missing base columns and reports row warnings", {
  imgdata <- data.frame(
    FileName = c("IMG_0001.JPG", "IMG_0001.JPG"),
    datetime_utc = c("2026-01-30 15:36:41", NA),
    justtime = c("15:36:41", NA),
    platform = c("Astro", "Astro"),
    pilot = c("igb", NA),
    permit = c("27066", NA),
    species = c("eg", NA),
    Latitude = c(30, 95),
    Longitude = c(-81, -190),
    laser_alt_m = c(25, 0),
    barometric_alt_m = c(30, 150),
    FocalLength_mm = c(24, NA),
    ImageWidth_px = c(3840, NA),
    ImageHeight_px = c(2160, NA),
    SensorWidth_mm = c(35.7, NA),
    pixel_dimension_mm = c(0.0093, NA),
    stringsAsFactors = FALSE
  )

  checked <- add_imgdata_qa_warnings(imgdata, "Astro")

  expect_true(all(IMGDATA_BASE_COLUMNS %in% names(checked)))
  expect_match(checked$qa_warnings[1], "duplicate FileName")
  expect_match(checked$qa_warnings[2], "missing datetime_utc")
  expect_match(checked$qa_warnings[2], "Latitude outside valid range")
  expect_match(checked$qa_warnings[2], "laser_alt_m outside valid range")
  expect_false("qa_warnings" %in% names(strip_qa_warnings_column(checked)))
  expect_match(imgdata_qa_status(checked, "Astro"), "IMG_0001.JPG")
})

test_that("imgdata_qa_status summarizes clean and warned outputs", {
  clean <- add_imgdata_qa_warnings(data.frame(
    FileName = "IMG_0001.JPG",
    ImageNum = 1,
    datetime_utc = "2026-01-30 15:36:41",
    justtime = "15:36:41",
    flightnum = 1,
    platform = "Astro",
    pilot = "igb",
    permit = "27066",
    species = "eg",
    whaleinfo = NA,
    Latitude = 30,
    Longitude = -81,
    gps_alt_m = 30,
    raw_laser_alt_cm = 2500,
    tilt_deg = NA,
    costilt = NA,
    laser_alt_m = 25,
    barometric_alt_m = 30,
    FocalLength_mm = 24,
    ImageWidth_px = 3840,
    ImageHeight_px = 2160,
    SensorWidth_mm = 35.7,
    pixel_dimension_mm = 0.0093,
    stringsAsFactors = FALSE
  ), "Astro")

  expect_match(imgdata_qa_status(clean, "Astro"), "no row-level warnings")
})
