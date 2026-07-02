source(test_path("../../functions/schema_helpers.R"))
source(test_path("../../functions/process_astro.R"))

test_that("astro_image_groups discovers current jpg card folders", {
  root <- file.path(tempdir(), paste0("astro-images-", as.integer(runif(1, 1, 1e7))))
  astro_dir <- file.path(root, "Astro")
  dir.create(file.path(astro_dir, "jpg", "card01"), recursive = TRUE)
  dir.create(file.path(astro_dir, "jpg", "thumbdrive"), recursive = TRUE)
  file.create(file.path(astro_dir, "jpg", "card01", "DSC0001.JPG"))
  file.create(file.path(astro_dir, "jpg", "thumbdrive", "260130_180113_739.jpg"))

  groups <- astro_image_groups(astro_dir)

  expect_equal(nrow(groups), 2)
  expect_true(any(grepl("card01$", groups$folder)))
  expect_true(any(grepl("thumbdrive$", groups$folder)))
})

test_that("astro_image_groups can use corrected jpg_corr card folders", {
  root <- file.path(tempdir(), paste0("astro-corrected-images-", as.integer(runif(1, 1, 1e7))))
  astro_dir <- file.path(root, "Astro")
  dir.create(file.path(astro_dir, "jpg", "card01"), recursive = TRUE)
  dir.create(file.path(astro_dir, "jpg_corr", "card01"), recursive = TRUE)
  file.create(file.path(astro_dir, "jpg", "card01", "DSC0001.JPG"))
  file.create(file.path(astro_dir, "jpg_corr", "card01", "DSC0001_corr.JPG"))

  groups <- astro_image_groups(astro_dir, "corrected")

  expect_equal(nrow(groups), 1)
  expect_true(grepl("jpg_corr", groups$folder))
})

test_that("read_astro_photo_info_triggers keeps trigger order and drops invalid laser ranges", {
  root <- file.path(tempdir(), paste0("astro-triggers-", as.integer(runif(1, 1, 1e7))))
  astro_dir <- file.path(root, "Astro")
  log_dir <- file.path(astro_dir, "log")
  dir.create(log_dir, recursive = TRUE)

  write.csv(data.frame(
    `Unix Time (ms) from Drone GPS` = c(3000, 1000, 2000),
    `Image File` = c(
      "http://example/260130_180003_000.jpg",
      "http://example/260130_180001_000.jpg",
      "http://example/260130_180002_000.jpg"
    ),
    `Camera Range (m)` = c(20, 0, 22),
    Latitude = c(3, 1, 2),
    Longitude = c(-3, -1, -2),
    `Altitude (m above takeoff location)` = c(30, 10, 20),
    check.names = FALSE
  ), file.path(log_dir, "flight_log_2026_1_30_11_55_02_140034002B_photo_info.csv"), row.names = FALSE)

  triggers <- read_astro_photo_info_triggers(astro_dir)

  expect_equal(triggers$logged_image_file, c(
    "260130_180001_000.jpg",
    "260130_180002_000.jpg",
    "260130_180003_000.jpg"
  ))
  expect_true(is.na(triggers$laser_alt_m[1]))
  expect_equal(triggers$laser_alt_m[2:3], c(22, 20))
})
