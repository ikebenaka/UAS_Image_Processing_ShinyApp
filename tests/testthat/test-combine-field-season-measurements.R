source(test_path("../../functions/associate_measurements.R"))

test_that("combine_field_season_measurements combines existing per-day photos_measured files", {
  season_dir <- file.path(tempdir(), paste0("combine-season-", as.integer(runif(1, 1, 1e7))))
  photos_measured_dir <- file.path(season_dir, "20260130", "Astro", "Photos_Measured")
  dir.create(photos_measured_dir, recursive = TRUE)

  write.csv(data.frame(
    FileName = "FRAME_0001.jpg",
    EGNO = "eg001",
    laser_alt_m = 25,
    FocalLength_mm = 24,
    pixel_dimension_mm = 0.004,
    TL = 100,
    W.at.eyes = 20,
    stringsAsFactors = FALSE
  ), file.path(photos_measured_dir, "20260130_Astro_photos_measured.csv"), row.names = FALSE)

  status_lines <- character()
  status <- function(line) status_lines <<- c(status_lines, line)

  combine_field_season_measurements(season_dir, status)

  season_name <- basename(season_dir)
  year <- basename(dirname(season_dir))
  pixels_file <- file.path(season_dir, paste0(season_name, "_", year, "_Combined_Photos_Measured.csv"))
  meters_file <- file.path(season_dir, paste0(season_name, "_", year, "_Combined_Photos_Measured_Meters.csv"))

  expect_true(file.exists(pixels_file))
  expect_true(file.exists(meters_file))

  pixels <- read.csv(pixels_file, stringsAsFactors = FALSE, check.names = FALSE)
  meters <- read.csv(meters_file, stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(nrow(pixels), 1)
  expect_equal(pixels$TL, 100)
  expect_equal(as.character(pixels$source_flight_day), "20260130")
  expect_equal(pixels$source_platform_folder, "Astro")
  expect_equal(meters$TL, 25 * 0.004 / 24 * 100)
  expect_true(any(grepl("Wrote 1 rows", status_lines)))
})
