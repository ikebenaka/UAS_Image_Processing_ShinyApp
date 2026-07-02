source(test_path("../../functions/associate_measurements.R"))

test_that("collate_measurements_for_flight_day updates per-day file and skips season outputs", {
  root <- file.path(tempdir(), paste0("collate-flight-day-", as.integer(runif(1, 1, 1e7))))
  flight_day <- file.path(root, "20260130")
  platform_dir <- file.path(flight_day, "EVO II Pro")
  photos_measured_dir <- file.path(platform_dir, "Photos_Measured")
  egno_dir <- file.path(photos_measured_dir, "eg001")
  dir.create(egno_dir, recursive = TRUE)

  photos_measured_file <- file.path(photos_measured_dir, "20260130_EVO_II_Pro_photos_measured.csv")
  write.csv(data.frame(
    FileName = "IMG_0001.JPG",
    EGNO = "eg001",
    laser_alt_m = 25,
    FocalLength_mm = 24,
    pixel_dimension_mm = 0.004,
    TL = NA_real_,
    stringsAsFactors = FALSE
  ), photos_measured_file, row.names = FALSE)

  write.csv(data.frame(
    Object = c("Image ID", "Image Path", "TL", "TL", "W.at.eyes"),
    Value = c(
      "IMG_0001",
      file.path(egno_dir, "IMG_0001.JPG"),
      "1.5",
      "312.46",
      "55.5"
    ),
    Value_unit = c("Metadata", "Metadata", "Meters", "Pixels", "Pixels"),
    stringsAsFactors = FALSE
  ), file.path(egno_dir, "IMG_0001.csv"), row.names = FALSE)

  status_lines <- character()
  status <- function(line) status_lines <<- c(status_lines, line)

  collate_measurements_for_flight_day(flight_day, status)

  output <- read.csv(photos_measured_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(output$TL, 312.46)
  expect_equal(output$W.at.eyes, 55.5)
  expect_true(any(grepl("season-level combined outputs skipped", status_lines)))
  expect_length(list.files(flight_day, pattern = "Combined_Photos_Measured", recursive = TRUE), 0)
  expect_true(length(list.files(photos_measured_dir, pattern = "backup-", full.names = TRUE)) >= 1)
})
