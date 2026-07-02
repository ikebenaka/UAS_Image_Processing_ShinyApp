source(test_path("../../functions/schema_helpers.R"))
source(test_path("../../functions/split_whaleinfo_rows.R"))

test_that("split_whaleinfo_rows creates one row per comma-separated whale", {
  df <- data.frame(
    FileName = "MAX_0001.JPG",
    whaleinfo = "3293, CO2",
    stringsAsFactors = FALSE
  )

  out <- split_whaleinfo_rows(df)

  expect_equal(nrow(out), 2)
  expect_equal(out$EGNO, c("3293", "CO2"))
  expect_true(all(c(
    "camera_focus",
    "body_straightness",
    "body_width_measurability"
  ) %in% names(out)))
})

test_that("split_whaleinfo_rows accepts semicolon separators", {
  df <- data.frame(
    FileName = "MAX_0001.JPG",
    whaleinfo = "3293;CO2",
    stringsAsFactors = FALSE
  )

  out <- split_whaleinfo_rows(df)

  expect_equal(out$EGNO, c("3293", "CO2"))
})

test_that("split_whaleinfo_rows errors when whaleinfo is missing", {
  df <- data.frame(FileName = "MAX_0001.JPG", stringsAsFactors = FALSE)

  expect_error(split_whaleinfo_rows(df), "whaleinfo")
})

test_that("split_whaleinfo_rows preserves rows with blank whaleinfo", {
  df <- data.frame(
    FileName = "MAX_0001.JPG",
    whaleinfo = "",
    stringsAsFactors = FALSE
  )

  out <- split_whaleinfo_rows(df)

  expect_equal(nrow(out), 1)
  expect_true(is.na(out$EGNO) || out$EGNO == "")
})

test_that("folder metadata discovery finds imgdata and optional video frames", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  dir.create(platform_dir, recursive = TRUE)
  imgdata_file <- file.path(platform_dir, "20260130_Astro_imgdata.csv")
  video_file <- file.path(platform_dir, "20260130_Astro_video_frames.csv")
  write.csv(data.frame(FileName = "DSC0001.JPG", whaleinfo = "3293"), imgdata_file, row.names = FALSE)
  write.csv(data.frame(frame_file = "C0001.MP4_00_00_01_vlc_00001.JPG", whaleinfo = "CO2"), video_file, row.names = FALSE)

  files <- find_platform_metadata_files(platform_dir)

  expect_equal(files$imgdata_file, imgdata_file)
  expect_equal(files$video_frames_file, video_file)
})

test_that("folder split writes still and video metadata separately with backups", {
  flight_day <- file.path(tempdir(), paste0("20260130-", as.integer(runif(1, 1, 1e7))))
  platform_dir <- file.path(flight_day, "Astro")
  dir.create(platform_dir, recursive = TRUE)
  imgdata_file <- file.path(platform_dir, "20260130_Astro_imgdata.csv")
  video_file <- file.path(platform_dir, "20260130_Astro_video_frames.csv")

  write.csv(data.frame(FileName = "DSC0001.JPG", whaleinfo = "3293;CO2"), imgdata_file, row.names = FALSE)
  write.csv(data.frame(frame_file = "C0001.MP4_00_00_01_vlc_00001.JPG", whaleinfo = "3293,CO2"), video_file, row.names = FALSE)

  result <- split_whaleinfo_metadata_folder(flight_day)
  img_out <- read.csv(imgdata_file, stringsAsFactors = FALSE)
  video_out <- read.csv(video_file, stringsAsFactors = FALSE)
  backups <- list.files(platform_dir, pattern = "backup-", full.names = TRUE)

  expect_equal(nrow(img_out), 2)
  expect_equal(nrow(video_out), 2)
  expect_equal(img_out$EGNO, c("3293", "CO2"))
  expect_equal(video_out$EGNO, c("3293", "CO2"))
  expect_false("frame_file" %in% names(img_out))
  expect_true("frame_file" %in% names(video_out))
  expect_true(length(backups) >= 2)
  expect_true(nrow(result$data) >= 4)
})
