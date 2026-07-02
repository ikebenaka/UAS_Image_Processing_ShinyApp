source(test_path("../../functions/schema_helpers.R"))
source(test_path("../../functions/process_photos_measured.R"))

test_that("process_photos_measured includes graded video frames and omits internal source columns", {
  flight_day <- file.path(tempdir(), paste0("photos-measured-video-", as.integer(runif(1, 1, 1e7))), "20260130")
  platform_dir <- file.path(flight_day, "Astro")
  dir.create(file.path(platform_dir, "video_frames", "card01"), recursive = TRUE)
  frame_file <- "C0001.MP4_00_00_14_vlc_00001.jpg"
  file.create(file.path(platform_dir, "video_frames", "card01", frame_file))

  write.csv(data.frame(
    FileName = "IMG_0001.JPG",
    EGNO = "eg001",
    whaleinfo = "eg001",
    stringsAsFactors = FALSE
  ), file.path(platform_dir, "20260130_Astro_imgdata.csv"), row.names = FALSE)

  write.csv(data.frame(
    FileName = frame_file,
    EGNO = "eg002",
    whaleinfo = "eg002",
    source_video_card = "card01",
    camera_focus = 1,
    body_straightness = 2,
    body_roll = 3,
    body_arch = 4,
    body_pitch = 5,
    body_length_measurability = 6,
    body_width_measurability = 7,
    photogram_comments = "good",
    stringsAsFactors = FALSE
  ), file.path(platform_dir, "20260130_Astro_video_frames.csv"), row.names = FALSE)

  status <- process_photos_measured(flight_day)
  output_file <- file.path(platform_dir, "Photos_Measured", "20260130_Astro_photos_measured.csv")
  output <- read.csv(output_file, stringsAsFactors = FALSE, check.names = FALSE)

  expect_match(status, "20260130_Astro_video_frames.csv")
  expect_equal(output$FileName, frame_file)
  expect_equal(output$photogram_quality, 28)
  expect_false("metadata_source_file" %in% names(output))
  expect_false("metadata_row_id" %in% names(output))
  expect_false("media_type" %in% names(output))
  expect_false("source_video_card" %in% names(output))
  expect_false("source_video_file" %in% names(output))
  expect_false("source_video_time_s" %in% names(output))
  expect_true(file.exists(file.path(platform_dir, "Photos_Measured", "eg002", frame_file)))
})
