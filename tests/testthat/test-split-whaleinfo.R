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
