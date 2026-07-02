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
  "source_video_card",
  "source_video_file",
  "source_video_timecode",
  "source_video_time_s",
  "frame_number"
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

timecode_to_seconds <- function(hours, minutes, seconds) {
  hours <- suppressWarnings(as.integer(hours))
  minutes <- suppressWarnings(as.integer(minutes))
  seconds <- suppressWarnings(as.integer(seconds))
  if (any(is.na(c(hours, minutes, seconds)))) return(NA_integer_)
  hours * 3600L + minutes * 60L + seconds
}

parse_video_frame_filename <- function(path) {
  filename <- basename(path)
  stem <- tools::file_path_sans_ext(filename)
  path_card <- video_frame_card_from_path(path)
  path_flight <- video_frame_flight_from_path(path)
  pattern <- "^(?:(card\\d+)_)?(?:(f\\d+)_)?(.+\\.(?:MP4|MOV|M4V))_(\\d{2})_(\\d{2})_(\\d{2})_vlc_(\\d+)$"
  match <- regexec(pattern, stem, ignore.case = TRUE)
  parts <- regmatches(stem, match)[[1]]

  if (!length(parts)) {
    return(data.frame(
      frame_file = filename,
      source_video_card = NA_character_,
      source_video_flight = NA_integer_,
      source_video_file = NA_character_,
      source_video_timecode = NA_character_,
      source_video_time_s = NA_integer_,
      frame_number = NA_integer_,
      parse_ok = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  timecode <- paste(parts[5], parts[6], parts[7], sep = ":")
  data.frame(
    frame_file = filename,
    source_video_card = if (nzchar(parts[2])) tolower(parts[2]) else path_card,
    source_video_flight = if (nzchar(parts[3])) suppressWarnings(as.integer(sub("^f", "", tolower(parts[3])))) else path_flight,
    source_video_file = parts[4],
    source_video_timecode = timecode,
    source_video_time_s = timecode_to_seconds(parts[5], parts[6], parts[7]),
    frame_number = suppressWarnings(as.integer(parts[8])),
    parse_ok = TRUE,
    stringsAsFactors = FALSE
  )
}

video_frame_flight_from_path <- function(path) {
  parts <- strsplit(normalizePath(path, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  flight_parts <- grep("^f\\d+$|^flight\\s*\\d+$", parts, ignore.case = TRUE, value = TRUE)
  if (!length(flight_parts)) return(NA_integer_)
  match <- regexec("(?:f|flight\\s*)(\\d+)", tail(flight_parts, 1), ignore.case = TRUE)
  match_parts <- regmatches(tail(flight_parts, 1), match)[[1]]
  if (!length(match_parts)) return(NA_integer_)
  suppressWarnings(as.integer(match_parts[2]))
}

video_frame_card_from_path <- function(path) {
  parts <- strsplit(normalizePath(path, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  card_parts <- grep("^card#?\\s*\\d+$|^card\\d+$", parts, ignore.case = TRUE, value = TRUE)
  if (!length(card_parts)) return(NA_character_)
  match <- regexec("card#?\\s*(\\d+)", tail(card_parts, 1), ignore.case = TRUE)
  match_parts <- regmatches(tail(card_parts, 1), match)[[1]]
  if (!length(match_parts)) return(NA_character_)
  sprintf("card%02d", suppressWarnings(as.integer(match_parts[2])))
}

parse_video_frame_filenames <- function(paths) {
  if (!length(paths)) {
    return(data.frame(
      frame_file = character(),
      source_video_card = character(),
      source_video_flight = integer(),
      source_video_file = character(),
      source_video_timecode = character(),
      source_video_time_s = integer(),
      frame_number = integer(),
      parse_ok = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(paths, parse_video_frame_filename))
}

rbind_fill_data_frames <- function(data_frames) {
  data_frames <- Filter(function(x) !is.null(x) && nrow(x) > 0, data_frames)
  if (!length(data_frames)) return(data.frame())

  all_names <- unique(unlist(lapply(data_frames, names)))
  data_frames <- lapply(data_frames, function(data) {
    missing <- setdiff(all_names, names(data))
    for (col in missing) data[[col]] <- NA
    data[, all_names, drop = FALSE]
  })

  do.call(rbind, data_frames)
}

add_imgdata_qa_warnings <- function(imgdata, platform_name = NA_character_) {
  imgdata <- ensure_columns(imgdata, IMGDATA_BASE_COLUMNS)
  warning_lists <- vector("list", nrow(imgdata))

  add_warning <- function(row_index, message) {
    warning_lists[[row_index]] <<- c(warning_lists[[row_index]], message)
  }

  for (i in seq_len(nrow(imgdata))) {
    for (col in c("FileName", "datetime_utc", "justtime", "platform", "pilot", "permit", "species")) {
      if (col %in% names(imgdata) && is_blank_scalar(imgdata[[col]][i])) {
        add_warning(i, paste("missing", col))
      }
    }

    for (col in c("Latitude", "Longitude")) {
      if (col %in% names(imgdata) && is.na(suppressWarnings(as.numeric(imgdata[[col]][i])))) {
        add_warning(i, paste("missing", col))
      }
    }

    if ("Latitude" %in% names(imgdata)) {
      lat <- suppressWarnings(as.numeric(imgdata$Latitude[i]))
      if (!is.na(lat) && (lat < -90 || lat > 90)) add_warning(i, "Latitude outside valid range")
    }
    if ("Longitude" %in% names(imgdata)) {
      lon <- suppressWarnings(as.numeric(imgdata$Longitude[i]))
      if (!is.na(lon) && (lon < -180 || lon > 180)) add_warning(i, "Longitude outside valid range")
    }

    for (col in c("laser_alt_m", "barometric_alt_m")) {
      if (col %in% names(imgdata)) {
        altitude <- suppressWarnings(as.numeric(imgdata[[col]][i]))
        if (is.na(altitude)) {
          add_warning(i, paste("missing", col))
        } else if (!is_valid_altitude_m(altitude)) {
          add_warning(i, paste(col, "outside valid range"))
        }
      }
    }

    for (col in c("FocalLength_mm", "ImageWidth_px", "ImageHeight_px", "SensorWidth_mm", "pixel_dimension_mm")) {
      if (col %in% names(imgdata)) {
        value <- suppressWarnings(as.numeric(imgdata[[col]][i]))
        if (is.na(value) || value <= 0) add_warning(i, paste("missing or invalid", col))
      }
    }
  }

  if ("FileName" %in% names(imgdata)) {
    duplicated_file <- duplicated(imgdata$FileName) | duplicated(imgdata$FileName, fromLast = TRUE)
    for (i in which(duplicated_file)) add_warning(i, "duplicate FileName")
  }

  imgdata$qa_warnings <- vapply(
    warning_lists,
    function(messages) paste(unique(messages), collapse = "; "),
    character(1)
  )

  imgdata
}

imgdata_qa_status <- function(imgdata, platform_name = "platform") {
  if (!"qa_warnings" %in% names(imgdata)) return(character())
  warning_n <- sum(nzchar(imgdata$qa_warnings))
  if (warning_n > 0) {
    paste("Warning:", warning_n, platform_name, "imgdata row(s) have QA warnings. See qa_warnings in the imgdata CSV.")
  } else {
    paste("QA check:", platform_name, "imgdata has no row-level warnings.")
  }
}

is_blank_scalar <- function(value) {
  length(value) == 0 || is.na(value) || !nzchar(as.character(value))
}

is_valid_altitude_m <- function(value, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  value <- suppressWarnings(as.numeric(value))
  !is.na(value) && is.finite(value) && value >= valid_range_m[["min"]] && value <= valid_range_m[["max"]]
}
