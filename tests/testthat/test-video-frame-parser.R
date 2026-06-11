source(test_path("../../functions/schema_helpers.R"))

test_that("parse_video_frame_filename parses EVO frame names", {
  out <- parse_video_frame_filename("MAX_0047_f2.MP4_00_00_14_vlc_00002.jpg")

  expect_true(out$parse_ok)
  expect_equal(out$source_video_file, "MAX_0047_f2.MP4")
  expect_equal(out$source_video_timecode, "00:00:14")
  expect_equal(out$source_video_time_s, 14L)
  expect_equal(out$frame_number, 2L)
})

test_that("parse_video_frame_filename parses Astro frame names", {
  out <- parse_video_frame_filename("C0007.MP4_00_01_23_vlc_00004.JPG")

  expect_true(out$parse_ok)
  expect_equal(out$source_video_file, "C0007.MP4")
  expect_equal(out$source_video_timecode, "00:01:23")
  expect_equal(out$source_video_time_s, 83L)
  expect_equal(out$frame_number, 4L)
})

test_that("parse_video_frame_filename reports malformed names", {
  out <- parse_video_frame_filename("C0007_00_01_23_vlc_00004.JPG")

  expect_false(out$parse_ok)
  expect_true(is.na(out$source_video_file))
  expect_true(is.na(out$source_video_time_s))
})

test_that("parse_video_frame_filenames returns one row per input", {
  out <- parse_video_frame_filenames(c(
    "MAX_0047_f2.MP4_00_00_14_vlc_00002.jpg",
    "bad-name.jpg"
  ))

  expect_equal(nrow(out), 2)
  expect_equal(out$parse_ok, c(TRUE, FALSE))
})
