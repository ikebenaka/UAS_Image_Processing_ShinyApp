source(test_path("../../functions/schema_helpers.R"))
source(test_path("../../functions/process_video_frames.R"))

test_that("process_video_frames creates folder when missing", {
  platform_dir <- file.path(tempdir(), paste0("Astro-", as.integer(runif(1, 1, 1e7))))
  dir.create(platform_dir)

  result <- process_video_frames(platform_dir, platform_dir, platform_name = "Astro")

  expect_true(dir.exists(file.path(platform_dir, "video_frames")))
  expect_true(is.null(result$frame_data))
  expect_match(paste(result$status, collapse = "\n"), "Created video_frames folder")
})

test_that("process_video_frames writes parsed frame metadata", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_01_23_vlc_00004.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_true(file.exists(result$output_file))
  expect_equal(nrow(result$frame_data), 1)
  expect_equal(result$frame_data$source_video_file, "C0007.MP4")
  expect_equal(result$frame_data$source_video_time_s, 83L)
  expect_equal(result$frame_data$FileName, "C0007.MP4_00_01_23_vlc_00004.JPG")
  expect_false("media_type" %in% names(result$frame_data))
  expect_false("parse_ok" %in% names(result$frame_data))
})

test_that("process_video_frames parses Astro card and flight tokens", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "card01_f2_C0007.MP4_00_01_23_vlc_00004.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$source_video_card, "card01")
  expect_equal(result$frame_data$source_video_flight, 2)
  expect_equal(result$frame_data$source_video_file, "C0007.MP4")
})

test_that("process_video_frames infers Astro card from video_frames subfolder", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames", "card01")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_01_23_vlc_00004.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$source_video_card, "card01")
  expect_equal(result$frame_data$source_video_file, "C0007.MP4")
})

test_that("process_video_frames infers Astro flight from video_frames subfolder", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames", "card01", "f2")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_01_23_vlc_00004.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$source_video_card, "card01")
  expect_equal(result$frame_data$source_video_flight, 2)
  expect_equal(result$frame_data$source_video_file, "C0007.MP4")
})

test_that("process_video_frames flags malformed frame names", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "EVO II Pro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "bad-name.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "EVO II Pro")

  expect_equal(result$frame_data$FileName, "bad-name.JPG")
  expect_match(paste(result$status, collapse = "\n"), "did not match the expected pattern")
})

test_that("prepare_video_frame_folder creates folder when video exists", {
  platform_dir <- file.path(tempdir(), paste0("Astro-", as.integer(runif(1, 1, 1e7))))
  video_dir <- file.path(platform_dir, "video")
  dir.create(video_dir, recursive = TRUE)
  file.create(file.path(video_dir, "C0007.MP4"))

  result <- prepare_video_frame_folder(platform_dir, platform_name = "Astro")

  expect_length(result$video_files, 1)
  expect_true(dir.exists(file.path(platform_dir, "video_frames")))
  expect_match(paste(result$status, collapse = "\n"), "created video_frames folder")
})

test_that("prepare_video_frame_folder does nothing when no video exists", {
  platform_dir <- file.path(tempdir(), paste0("Astro-", as.integer(runif(1, 1, 1e7))))
  dir.create(platform_dir)

  result <- prepare_video_frame_folder(platform_dir, platform_name = "Astro")

  expect_length(result$video_files, 0)
  expect_false(dir.exists(file.path(platform_dir, "video_frames")))
  expect_equal(result$status, character())
})

test_that("process_video_frames_for_flight_day skips platforms with no video context", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  dir.create(platform_dir, recursive = TRUE)

  status <- process_video_frames_for_flight_day(flight_day)

  expect_false(dir.exists(file.path(platform_dir, "video_frames")))
  expect_match(status, "No video files or existing video_frames folders")
})

test_that("process_video_frames_for_flight_day accepts a selected platform folder", {
  fixture_root <- file.path(tempdir(), paste0("video-frame-platform-", as.integer(runif(1, 1, 1e7))))
  flight_day <- file.path(fixture_root, "20260130")
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  dir.create(video_frames_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_00_14_vlc_00002.JPG"))

  status <- process_video_frames_for_flight_day(platform_dir)
  output_file <- file.path(platform_dir, "20260130_Astro_video_frames.csv")

  expect_true(file.exists(output_file))
  expect_match(status, "Wrote video frame metadata")
})

test_that("video_frame_platform_dirs treats selected EVO platform as platform, not EVO_logs", {
  fixture_root <- file.path(tempdir(), paste0("video-frame-platform-evo-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(fixture_root, "20230410", "EVO II Pro")
  dir.create(file.path(platform_dir, "EVO_logs"), recursive = TRUE)
  dir.create(file.path(platform_dir, "video_frames"), recursive = TRUE)

  dirs <- video_frame_platform_dirs(platform_dir)

  expect_equal(dirs, platform_dir)
})

test_that("process_video_frames_for_flight_day prefers child platform folders over root video_frames", {
  fixture_root <- file.path(tempdir(), paste0("video-frame-flight-day-", as.integer(runif(1, 1, 1e7))))
  flight_day <- file.path(fixture_root, "20230410")
  root_video_frames_dir <- file.path(flight_day, "video_frames")
  platform_dir <- file.path(flight_day, "EVO II Pro")
  platform_video_frames_dir <- file.path(platform_dir, "video_frames")
  dir.create(root_video_frames_dir, recursive = TRUE)
  dir.create(platform_video_frames_dir, recursive = TRUE)
  file.create(file.path(root_video_frames_dir, "MAX_0047_f2.MP4_00_00_14_vlc_00002.JPG"))

  status <- process_video_frames_for_flight_day(flight_day)
  platform_output_file <- file.path(platform_dir, "20230410_EVO_II_Pro_video_frames.csv")
  root_output_file <- file.path(flight_day, "20230410_video_frames.csv")
  platform_output <- read.csv(platform_output_file, stringsAsFactors = FALSE)

  expect_true(file.exists(platform_output_file))
  expect_false(file.exists(root_output_file))
  expect_equal(platform_output$FileName, "MAX_0047_f2.MP4_00_00_14_vlc_00002.JPG")
  expect_match(status, "flight-day video_frames folder")
  expect_match(status, "EVO II Pro")
})

test_that("process_video_frames assigns Drone Amplified altitude from a single photo_info log", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_00_14_vlc_00002.JPG"))

  write.csv(data.frame(
    `Unix Time (ms)` = c(1, 2, 3, 4),
    `Unix Time (ms) from Drone GPS` = c(1, 2, 3, 4),
    `Image File` = "",
    `Video Time (s)` = c(12, 13, 14, 19),
    `Camera Range (m)` = c(20, 22, 24, 25),
    Latitude = c(30, 31, 32, 33),
    Longitude = c(-70, -71, -72, -73),
    `Altitude (m above takeoff location)` = c(40, 42, 44, 46),
    check.names = FALSE
  ), file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$laser_alt_m, 22)
  expect_equal(result$frame_data$barometric_alt_m, 42)
  expect_equal(result$frame_data$datetime_utc, "1970-01-01 00:00:00")
  expect_equal(result$frame_data$justtime, "00:00:00")
  expect_equal(result$frame_data$source_video_flight, 1)
  expect_false("altitude_source" %in% names(result$frame_data))
  expect_false("altitude_samples_n" %in% names(result$frame_data))
})

test_that("process_video_frames copies pilot permit and species from imgdata", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_00_14_vlc_00002.JPG"))

  write.csv(data.frame(
    FileName = "image001.jpg",
    pilot = "IGB",
    permit = "27066",
    species = "Eg",
    stringsAsFactors = FALSE
  ), file.path(platform_dir, "20260130_Astro_imgdata.csv"), row.names = FALSE)
  write.csv(data.frame(
    `Unix Time (ms) from Drone GPS` = 1769787374000,
    `Video Time (s)` = 14,
    `Camera Range (m)` = 24,
    Latitude = 32,
    Longitude = -72,
    `Altitude (m above takeoff location)` = 44,
    check.names = FALSE
  ), file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$pilot, "IGB")
  expect_equal(result$frame_data$permit, "27066")
  expect_equal(result$frame_data$species, "Eg")
  expect_match(paste(result$status, collapse = "\n"), "Copied pilot, permit, species")
})

test_that("process_video_frames requires f-number when multiple Drone Amplified logs are present", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007.MP4_00_00_14_vlc_00002.JPG"))

  log_data <- data.frame(
    `Video Time (s)` = 14,
    `Camera Range (m)` = 22,
    Latitude = 30,
    Longitude = -70,
    `Altitude (m above takeoff location)` = 40,
    check.names = FALSE
  )
  write.csv(log_data, file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)
  write.csv(log_data, file.path(logs_dir, "flight_log_2026_1_30_11_31_12_140034002B_photo_info.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_true(is.na(result$frame_data$laser_alt_m))
  expect_false("altitude_source" %in% names(result$frame_data))
  expect_match(paste(result$status, collapse = "\n"), "did not identify a unique Drone Amplified flight log")
})

test_that("process_video_frames uses f-number to choose among multiple Drone Amplified logs", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "C0007_f2.MP4_00_00_14_vlc_00002.JPG"))

  first_log <- data.frame(
    `Video Time (s)` = 14,
    `Camera Range (m)` = 18,
    Latitude = 30,
    Longitude = -70,
    `Altitude (m above takeoff location)` = 35,
    check.names = FALSE
  )
  second_log <- data.frame(
    `Video Time (s)` = 14,
    `Camera Range (m)` = 28,
    Latitude = 31,
    Longitude = -71,
    `Altitude (m above takeoff location)` = 45,
    check.names = FALSE
  )
  write.csv(first_log, file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)
  write.csv(second_log, file.path(logs_dir, "flight_log_2026_1_30_11_31_12_140034002B_photo_info.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$laser_alt_m, 28)
  expect_equal(result$frame_data$barometric_alt_m, 45)
  expect_false("source_log_file" %in% names(result$frame_data))
})

test_that("process_video_frames uses frame flight token to choose among Drone Amplified logs", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "card01_f2_C0007.MP4_00_00_14_vlc_00002.JPG"))

  first_log <- data.frame(
    `Video Time (s)` = 14,
    `Camera Range (m)` = 18,
    Latitude = 30,
    Longitude = -70,
    `Altitude (m above takeoff location)` = 35,
    check.names = FALSE
  )
  second_log <- data.frame(
    `Video Time (s)` = 14,
    `Camera Range (m)` = 28,
    Latitude = 31,
    Longitude = -71,
    `Altitude (m above takeoff location)` = 45,
    check.names = FALSE
  )
  write.csv(first_log, file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)
  write.csv(second_log, file.path(logs_dir, "flight_log_2026_1_30_11_31_12_140034002B_photo_info.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro")

  expect_equal(result$frame_data$laser_alt_m, 28)
  expect_equal(result$frame_data$barometric_alt_m, 45)
})

test_that("video_inventory flags duplicate Astro video names across card folders", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  card1 <- file.path(platform_dir, "video", "card#1 - flights 1-4")
  card2 <- file.path(platform_dir, "video", "card#2 - flights 5-6")
  dir.create(card1, recursive = TRUE)
  dir.create(card2, recursive = TRUE)
  file.create(file.path(card1, "C0007.MP4"))
  file.create(file.path(card2, "C0007.MP4"))

  inventory <- video_inventory(platform_dir)

  expect_equal(nrow(inventory), 2)
  expect_true(all(inventory$duplicate_video_name))
  expect_equal(sort(inventory$source_video_card), c("card01", "card02"))
  expect_true(any(grepl("^card01_C0007\\.MP4_$", inventory$suggested_frame_prefix)))
})

test_that("choose_photo_info_log_for_frame can infer flight from card inventory", {
  inventory <- data.frame(
    source_video_card = "card02",
    source_video_file = "C0007.MP4",
    folder_flight_range = "5-6",
    stringsAsFactors = FALSE
  )
  logs <- paste0("log", 1:7, "_photo_info.csv")

  selected <- choose_photo_info_log_for_frame(
    source_video_file = "C0007.MP4",
    photo_info_files = logs,
    source_video_card = "card02",
    inventory = inventory
  )

  expect_equal(selected, "log5_photo_info.csv")
})

test_that("read_sony_video_xml_metadata reads duration and dimensions", {
  xml_file <- file.path(tempdir(), paste0("sony-video-", as.integer(runif(1, 1, 1e7)), ".XML"))
  writeLines(c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<NonRealTimeMeta>",
    "  <Duration value=\"2997\"/>",
    "  <VideoFrame formatFps=\"29.97p\"/>",
    "  <VideoLayout pixel=\"3840\" numOfVerticalLine=\"2160\" aspectRatio=\"16:9\"/>",
    "</NonRealTimeMeta>"
  ), xml_file)

  metadata <- read_sony_video_xml_metadata(xml_file)

  expect_equal(round(metadata$duration_s, 1), 100)
  expect_equal(metadata$image_width_px, 3840)
  expect_equal(metadata$image_height_px, 2160)
})

test_that("assign_photo_info_logs_to_video_inventory maps Astro videos by XML duration", {
  fixture_root <- file.path(tempdir(), paste0("astro-video-map-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(fixture_root, "20260130", "Astro")
  video_dir <- file.path(platform_dir, "video", "card01")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_dir, "C0001.MP4"))
  writeLines(c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<NonRealTimeMeta>",
    "  <Duration value=\"2997\"/>",
    "  <VideoFrame formatFps=\"29.97p\"/>",
    "  <VideoLayout pixel=\"3840\" numOfVerticalLine=\"2160\" aspectRatio=\"16:9\"/>",
    "</NonRealTimeMeta>"
  ), file.path(video_dir, "C0001M01.XML"))

  write.csv(data.frame(
    `Video Time (s)` = c(0.5, 100),
    `Camera Range (m)` = c(22, 23),
    check.names = FALSE
  ), file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)

  inventory <- video_inventory(platform_dir)
  logs <- find_drone_amplified_photo_info_files(platform_dir)
  mapped <- assign_photo_info_logs_to_video_inventory(inventory, logs)

  expect_equal(mapped$assigned_flightnum, 1)
  expect_match(basename(mapped$assigned_log_file), "_photo_info\\.csv$")
  expect_equal(mapped$ImageWidth_px, 3840)
})

test_that("write_video_inventory populates folder flight range from inferred log mapping", {
  fixture_root <- file.path(tempdir(), paste0("astro-inventory-range-", as.integer(runif(1, 1, 1e7))))
  flight_day <- file.path(fixture_root, "20260130")
  platform_dir <- file.path(flight_day, "Astro")
  video_dir <- file.path(platform_dir, "video", "card01")
  logs_dir <- file.path(platform_dir, "logs")
  dir.create(video_dir, recursive = TRUE)
  dir.create(logs_dir, recursive = TRUE)
  file.create(file.path(video_dir, "C0001.MP4"))
  writeLines(c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<NonRealTimeMeta>",
    "  <Duration value=\"2997\"/>",
    "  <VideoFrame formatFps=\"29.97p\"/>",
    "  <VideoLayout pixel=\"3840\" numOfVerticalLine=\"2160\" aspectRatio=\"16:9\"/>",
    "</NonRealTimeMeta>"
  ), file.path(video_dir, "C0001M01.XML"))
  write.csv(data.frame(
    `Video Time (s)` = c(0.5, 100),
    `Camera Range (m)` = c(22, 23),
    check.names = FALSE
  ), file.path(logs_dir, "flight_log_2026_1_30_10_35_07_140034002B_photo_info.csv"), row.names = FALSE)

  result <- write_video_inventory(platform_dir, "20260130", "Astro")

  expect_equal(result$inventory$folder_flight_range, "1")
  expect_equal(result$inventory$assigned_flightnum, 1)
})

test_that("summarize_photo_info_altitude excludes zero and out-of-range altitude values", {
  log_data <- data.frame(
    video_time_s = c(12, 13, 14, 15, 16),
    camera_range_m = c(0, 4.9, 22, 24, 150),
    latitude = c(30, 31, 32, 33, 34),
    longitude = c(-70, -71, -72, -73, -74),
    barometric_alt_m = c(0, 4, 40, 44, 150),
    unix_time_gps_ms = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  summary <- summarize_photo_info_altitude(log_data, frame_time_s = 14, window_s = 2)

  expect_equal(summary$laser_alt_m, 23)
  expect_equal(summary$barometric_alt_m, 42)
  expect_equal(summary$altitude_samples_n, 2)
})

test_that("add_video_frame_qa_warnings reports row-level metadata problems", {
  frame_data <- data.frame(
    FileName = c("frame1.jpg", "frame1.jpg"),
    parse_ok = c(TRUE, FALSE),
    source_video_file = c("C0001.MP4", NA),
    source_video_time_s = c(14, NA),
    source_video_card = c("card01", NA),
    source_video_flight = c(1, NA),
    datetime_utc = c("2026-01-30 15:36:41", NA),
    justtime = c("15:36:41", NA),
    laser_alt_m = c(20, 0),
    barometric_alt_m = c(25, 150),
    pilot = c("igb", NA),
    permit = c("27066", NA),
    species = c("eg", NA),
    FocalLength_mm = c(24, NA),
    ImageWidth_px = c(3840, NA),
    ImageHeight_px = c(2160, NA),
    SensorWidth_mm = c(35.7, NA),
    pixel_dimension_mm = c(0.0093, NA),
    stringsAsFactors = FALSE
  )

  result <- add_video_frame_qa_warnings(frame_data, "Astro")

  expect_match(result$status, "2 video frame row")
  expect_match(result$frame_data$qa_warnings[1], "duplicate output filename")
  expect_match(result$frame_data$qa_warnings[2], "filename did not match")
  expect_match(result$frame_data$qa_warnings[2], "missing source video filename")
  expect_match(result$frame_data$qa_warnings[2], "laser altitude outside valid range")
})

test_that("read_drone_amplified_photo_info tolerates missing expected columns", {
  log_file <- file.path(tempdir(), paste0("missing-camera-range-", as.integer(runif(1, 1, 1e7)), ".csv"))
  write.csv(data.frame(`Video Time (s)` = 14, check.names = FALSE), log_file, row.names = FALSE)

  log_data <- read_drone_amplified_photo_info(log_file)

  expect_true("camera_range_m" %in% names(log_data))
  expect_true(is.na(log_data$camera_range_m))
})

test_that("process_video_frames assigns EVO altitude from video metadata and cleaned lidar", {
  flight_day <- file.path(tempdir(), paste0("20230410-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "EVO II Pro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  video_dir <- file.path(platform_dir, "video")
  evo_logs_dir <- file.path(platform_dir, "EVO_logs")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(video_dir, recursive = TRUE)
  dir.create(evo_logs_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "MAX_0047_f2.MP4_00_00_14_vlc_00002.JPG"))

  write.csv(data.frame(
    FileName = "MAX_0047_f2.MP4",
    datetime_utc = "2023-04-10 16:04:42",
    starttime = "15:59:55",
    endtime = "16:04:42",
    duration = 287,
    fps = 30,
    flightnum = 2,
    platform = "EVO II Pro",
    pilot = "IGB",
    permit = "27066",
    species = "Eg",
    FocalLength_mm = 10.6,
    ImageWidth_px = 3840,
    ImageHeight_px = 2160,
    SensorWidth_mm = 13.46688,
    pixel_dimension_mm = 0.003507,
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20230410_video_metadata.csv"), row.names = FALSE)

  write.csv(data.frame(
    gmt_time = c("16:00:07", "16:00:08", "16:00:09", "16:00:20"),
    Laser_Alt = c(18, 20, 22, 24),
    laser_altitude_cm = c(1800, 2000, 2200, 2400),
    latitude = c(41, 42, 43, 44),
    longitude = c(-70, -71, -72, -73),
    gps_altitude_m = c(30, 31, 32, 33),
    tilt_deg = c(1, 2, 3, 4),
    converted = c(0.9, 0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20230410_video_CleanedLidar.csv"), row.names = FALSE)
  write.csv(data.frame(
    `datetime(utc)` = c("2023-04-10 16:00:08", "2023-04-10 16:00:09", "2023-04-10 16:00:10"),
    `height_above_takeoff(feet)` = c(100, 110, 120),
    check.names = FALSE
  ), file.path(evo_logs_dir, "airdata.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "EVO II Pro")

  expect_equal(result$frame_data$laser_alt_m, 20)
  expect_equal(result$frame_data$raw_laser_alt_cm, 2000)
  expect_equal(result$frame_data$barometric_alt_m, 33.528)
  expect_equal(result$frame_data$FocalLength_mm, 10.6)
  expect_equal(result$frame_data$flightnum, 2)
  expect_false("altitude_source" %in% names(result$frame_data))
  expect_false("source_log_file" %in% names(result$frame_data))
})

test_that("process_video_frames warns when EVO frame does not match video metadata", {
  flight_day <- file.path(tempdir(), paste0("20230410-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "EVO II Pro")
  video_frames_dir <- file.path(platform_dir, "video_frames")
  video_dir <- file.path(platform_dir, "video")
  dir.create(video_frames_dir, recursive = TRUE)
  dir.create(video_dir, recursive = TRUE)
  file.create(file.path(video_frames_dir, "MAX_9999_f2.MP4_00_00_14_vlc_00002.JPG"))

  write.csv(data.frame(
    FileName = "MAX_0047_f2.MP4",
    datetime_utc = "2023-04-10 16:04:42",
    starttime = "15:59:55",
    endtime = "16:04:42",
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20230410_video_metadata.csv"), row.names = FALSE)
  write.csv(data.frame(
    gmt_time = "16:00:09",
    Laser_Alt = 20,
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20230410_video_CleanedLidar.csv"), row.names = FALSE)

  result <- process_video_frames(platform_dir, flight_day, platform_name = "EVO II Pro")

  expect_true(is.na(result$frame_data$laser_alt_m))
  expect_match(paste(result$status, collapse = "\n"), "did not match a row")
})

test_that("video_inventory fills EVO metadata from video_metadata.csv and filename flight tokens", {
  platform_dir <- file.path(tempdir(), paste0("EVO-video-inventory-", as.integer(runif(1, 1, 1e7))))
  video_dir <- file.path(platform_dir, "video")
  dir.create(video_dir, recursive = TRUE)
  file.create(file.path(video_dir, "MAX_0042_f2.MP4"))

  write.csv(data.frame(
    FileName = "MAX_0042_f2.MP4",
    duration = 287.5,
    flightnum = 2,
    FocalLength_mm = 10.6,
    ImageWidth_px = 3840,
    ImageHeight_px = 2160,
    SensorWidth_mm = 13.46688,
    pixel_dimension_mm = 0.003507,
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20230410_video_metadata.csv"), row.names = FALSE)

  inventory <- video_inventory(platform_dir)

  expect_equal(inventory$source_video_duration_s, 287.5)
  expect_equal(inventory$ImageWidth_px, 3840)
  expect_equal(inventory$ImageHeight_px, 2160)
  expect_equal(inventory$FocalLength_mm, 10.6)
  expect_equal(inventory$SensorWidth_mm, 13.46688)
  expect_equal(inventory$pixel_dimension_mm, 0.003507)
  expect_equal(inventory$folder_flight_range, "2")
})

test_that("video_inventory matches EVO renamed _f files to original video_metadata names", {
  platform_dir <- file.path(tempdir(), paste0("EVO-video-renamed-", as.integer(runif(1, 1, 1e7))))
  video_dir <- file.path(platform_dir, "video")
  dir.create(video_dir, recursive = TRUE)
  file.create(file.path(video_dir, "MAX_0109_f2.MP4"))

  write.csv(data.frame(
    FileName = "MAX_0109.MP4",
    duration = 80.767,
    flightnum = 2,
    FocalLength_mm = 10.6,
    ImageWidth_px = 3840,
    ImageHeight_px = 2160,
    SensorWidth_mm = 13.46688,
    pixel_dimension_mm = 0.003507,
    stringsAsFactors = FALSE
  ), file.path(video_dir, "20250112_video_metadata.csv"), row.names = FALSE)

  inventory <- video_inventory(platform_dir)

  expect_equal(inventory$source_video_file, "MAX_0109_f2.MP4")
  expect_equal(inventory$source_video_duration_s, 80.767)
  expect_equal(inventory$ImageWidth_px, 3840)
  expect_equal(inventory$folder_flight_range, "2")
})

test_that("video_inventory can use corrected Astro video_corr files with placeholder pixel dimension", {
  platform_dir <- file.path(tempdir(), paste0("Astro-video-corr-", as.integer(runif(1, 1, 1e7))))
  dir.create(file.path(platform_dir, "video", "card01"), recursive = TRUE)
  dir.create(file.path(platform_dir, "video_corr", "card01"), recursive = TRUE)
  file.create(file.path(platform_dir, "video", "card01", "C0007.MP4"))
  file.create(file.path(platform_dir, "video_corr", "card01", "C0007_corr.MP4"))

  inventory <- video_inventory(platform_dir, "corrected")

  expect_equal(nrow(inventory), 1)
  expect_equal(inventory$source_video_file, "C0007_corr.MP4")
  expect_true(grepl("card01", inventory$source_video_relative_path))
  expect_true(is.na(inventory$pixel_dimension_mm))
})

test_that("process_video_frames reports corrected Astro video pixel dimension placeholder", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  dir.create(file.path(platform_dir, "video_corr", "card01"), recursive = TRUE)
  dir.create(file.path(platform_dir, "video_frames", "card01"), recursive = TRUE)
  file.create(file.path(platform_dir, "video_corr", "card01", "C0007_corr.MP4"))
  file.create(file.path(platform_dir, "video_frames", "card01", "C0007_corr.MP4_00_00_14_vlc_00002.JPG"))

  result <- process_video_frames(platform_dir, flight_day, platform_name = "Astro", astro_video_source = "corrected")

  expect_match(paste(result$status, collapse = "\n"), "Corrected Astro video")
  expect_match(result$frame_data$qa_warnings, "corrected Astro video pixel_dimension_mm placeholder")
})
