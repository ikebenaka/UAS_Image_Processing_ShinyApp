IMGDATA_BASE_COLUMNS <- c(
  "FileName",
  "ImageNum",
  "datetime_utc",
  "justtime",
  "flightnum",
  "platform",
  "pilot",
  "permit",
  "species",
  "whaleinfo",
  "Latitude",
  "Longitude",
  "gps_alt_m",
  "raw_laser_alt_cm",
  "tilt_deg",
  "costilt",
  "laser_alt_m",
  "barometric_alt_m",
  "FocalLength_mm",
  "ImageWidth_px",
  "ImageHeight_px",
  "SensorWidth_mm",
  "pixel_dimension_mm"
)

GRADING_COLUMNS <- c(
  "EGNO",
  "camera_focus",
  "body_straightness",
  "body_roll",
  "body_arch",
  "body_pitch",
  "body_length_measurability",
  "body_width_measurability",
  "photogram_quality",
  "photogram_comments"
)

MEASUREMENT_COLUMNS <- c(
  "TL",
  "L.to.blowhole",
  "L.to.start.of.fluke",
  sprintf("TL_w%05.2f", seq(5, 95, by = 5)),
  "W.at.eyes",
  "W.fluke"
)

PHOTOS_MEASURED_COLUMNS <- c(
  IMGDATA_BASE_COLUMNS,
  GRADING_COLUMNS,
  MEASUREMENT_COLUMNS
)

VIDEO_FRAME_PROVENANCE_COLUMNS <- c(
  "media_type",
  "source_video_file",
  "source_video_time_s",
  "source_video_timecode",
  "frame_file",
  "frame_number",
  "video_start_datetime_utc",
  "video_end_datetime_utc",
  "altitude_source",
  "altitude_match_time_diff_s",
  "altitude_samples_n",
  "altitude_window_s",
  "source_log_file",
  "drone_altitude_above_takeoff_m"
)

VALID_ALTITUDE_RANGE_M <- c(min = 5, max = 100)

schema_missing_columns <- function(data, required_columns) {
  setdiff(required_columns, names(data))
}

schema_has_columns <- function(data, required_columns) {
  length(schema_missing_columns(data, required_columns)) == 0
}

ensure_columns <- function(data, required_columns, default = NA) {
  missing <- schema_missing_columns(data, required_columns)
  for (col in missing) {
    data[[col]] <- default
  }
  data
}

make_report <- function() {
  data.frame(
    level = character(),
    message = character(),
    file = character(),
    row = integer(),
    stringsAsFactors = FALSE
  )
}

add_report_message <- function(report, level, message, file = NA_character_, row = NA_integer_) {
  if (missing(report) || is.null(report)) report <- make_report()
  rbind(
    report,
    data.frame(
      level = level,
      message = message,
      file = file,
      row = row,
      stringsAsFactors = FALSE
    )
  )
}

format_report_messages <- function(report) {
  if (is.null(report) || !nrow(report)) return("No warnings or errors.")
  apply(
    report,
    1,
    function(x) {
      location <- ""
      if (!is.na(x[["file"]]) && nzchar(x[["file"]])) {
        location <- paste0(" [", x[["file"]], "]")
      }
      if (!is.na(suppressWarnings(as.integer(x[["row"]])))) {
        location <- paste0(location, " row ", x[["row"]])
      }
      paste0("[", toupper(x[["level"]]), "] ", x[["message"]], location)
    }
  )
}

barometric_offset_value <- function(offset_m) {
  value <- suppressWarnings(as.numeric(offset_m))
  if (length(value) == 0 || is.na(value)) return(0)
  value
}

apply_barometric_offset <- function(barometric_alt_m, offset_m = 0) {
  offset <- barometric_offset_value(offset_m)
  ifelse(is.na(barometric_alt_m), NA_real_, suppressWarnings(as.numeric(barometric_alt_m)) + offset)
}

timestamp_for_filename <- function(time = Sys.time()) {
  format(time, "%Y%m%d_%H%M%S")
}

backup_existing_file <- function(file_path, timestamp = timestamp_for_filename()) {
  if (is.null(file_path) || !nzchar(file_path) || !file.exists(file_path)) {
    return(NA_character_)
  }

  ext <- tools::file_ext(file_path)
  stem <- if (nzchar(ext)) {
    tools::file_path_sans_ext(file_path)
  } else {
    file_path
  }
  backup_path <- if (nzchar(ext)) {
    paste0(stem, ".backup-", timestamp, ".", ext)
  } else {
    paste0(stem, ".backup-", timestamp)
  }

  ok <- file.copy(file_path, backup_path, overwrite = FALSE)
  if (!isTRUE(ok)) {
    stop("Failed to create backup for existing file: ", file_path, call. = FALSE)
  }
  backup_path
}
