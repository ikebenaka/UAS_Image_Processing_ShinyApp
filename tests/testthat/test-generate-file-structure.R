source(test_path("../../functions/generate_file_structure.R"))

test_that("generate_folder_structure creates current Astro videography layout", {
  root <- file.path(tempdir(), paste0("astro-structure-", as.integer(runif(1, 1, 1e7))))
  flight_day <- file.path(root, "20260130")
  dir.create(flight_day, recursive = TRUE)

  generate_folder_structure(flight_day, "FreeFly Astro")

  expected_dirs <- file.path(flight_day, "Astro", c(
    "jpg",
    "jpg/card01",
    "jpg/thumbdrive",
    "video",
    "video/card01",
    "video_frames",
    "video_frames/card01",
    "log",
    "flight_logs",
    "jpg_corr",
    "video_corr",
    "video_corr/card01"
  ))

  expect_true(all(dir.exists(expected_dirs)))
  expect_false(dir.exists(file.path(flight_day, "Astro", "calibrated_stills")))
  expect_false(dir.exists(file.path(flight_day, "Astro", "calibrated_video")))
})
