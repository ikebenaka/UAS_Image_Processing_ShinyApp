process_video_frames <- function(platform_dir, flight_date_directory, platform_name = basename(platform_dir), video_frames_dir = NULL, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  status_messages <- character()
  flight_date <- basename(flight_date_directory)
  platform_token <- gsub("[^A-Za-z0-9]+", "_", platform_name)

  if (is.null(video_frames_dir)) {
    video_frames_dir <- file.path(platform_dir, "video_frames")
  }

  if (!dir.exists(video_frames_dir)) {
    dir.create(video_frames_dir, recursive = TRUE)
    status_messages <- c(status_messages, paste("Created video_frames folder in", platform_name))
  } else {
    status_messages <- c(status_messages, paste("video_frames folder already exists in", platform_name))
  }

  platform_video_frames_dir <- file.path(platform_dir, "video_frames")
  if (normalizePath(video_frames_dir, winslash = "/", mustWork = FALSE) !=
      normalizePath(platform_video_frames_dir, winslash = "/", mustWork = FALSE)) {
    status_messages <- c(
      status_messages,
      paste("Using video frame stills from", video_frames_dir, "for", platform_name)
    )
  }

  frame_files <- list.files(
    video_frames_dir,
    pattern = "\\.(jpe?g|png|tif|tiff)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  output_file <- file.path(
    platform_dir,
    paste0(flight_date, "_", platform_token, "_video_frames.csv")
  )

  inventory_result <- write_video_inventory(platform_dir, flight_date, platform_token, astro_video_source)
  status_messages <- c(status_messages, inventory_result$status)

  if (!length(frame_files)) {
    status_messages <- c(
      status_messages,
      paste("No video frame stills found in", video_frames_dir)
    )
    return(list(
      output_file = output_file,
      frame_data = NULL,
      status = status_messages
    ))
  }

  frame_data <- parse_video_frame_filenames(frame_files)
  frame_data$FileName <- frame_data$frame_file
  frame_data$ImageNum <- frame_data$frame_number
  frame_data$datetime_utc <- NA_character_
  frame_data$justtime <- NA_character_
  frame_data$flightnum <- NA_integer_
  frame_data$media_type <- "video_frame"
  frame_data$platform <- platform_name
  frame_data$pilot <- NA_character_
  frame_data$permit <- NA_character_
  frame_data$species <- NA_character_
  frame_data$whaleinfo <- NA_character_
  frame_data$altitude_source <- NA_character_
  frame_data$altitude_match_time_diff_s <- NA_real_
  frame_data$altitude_samples_n <- NA_integer_
  frame_data$altitude_window_s <- NA_real_
  frame_data$source_log_file <- NA_character_
  frame_data$drone_altitude_above_takeoff_m <- NA_real_
  frame_data$Latitude <- NA_real_
  frame_data$Longitude <- NA_real_
  frame_data$gps_alt_m <- NA_real_
  frame_data$laser_alt_m <- NA_real_
  frame_data$raw_laser_alt_cm <- NA_real_
  frame_data$tilt_deg <- NA_real_
  frame_data$costilt <- NA_real_
  frame_data$barometric_alt_m <- NA_real_
  frame_data$FocalLength_mm <- NA_real_
  frame_data$ImageWidth_px <- NA_real_
  frame_data$ImageHeight_px <- NA_real_
  frame_data$SensorWidth_mm <- NA_real_
  frame_data$pixel_dimension_mm <- NA_real_

  context_result <- apply_platform_context_from_imgdata(frame_data, platform_dir)
  frame_data <- context_result$frame_data
  status_messages <- c(status_messages, context_result$status)

  altitude_result <- assign_drone_amplified_altitudes(frame_data, platform_dir, astro_video_source = astro_video_source)
  frame_data <- altitude_result$frame_data
  status_messages <- c(status_messages, altitude_result$status)

  altitude_result <- assign_evo_video_frame_altitudes(frame_data, platform_dir)
  frame_data <- altitude_result$frame_data
  status_messages <- c(status_messages, altitude_result$status)

  frame_data <- preserve_existing_video_frame_annotations(frame_data, output_file)
  qa_result <- add_video_frame_qa_warnings(frame_data, platform_name, astro_video_source)
  frame_data <- qa_result$frame_data
  status_messages <- c(status_messages, qa_result$status)
  malformed_n <- sum(!frame_data$parse_ok, na.rm = TRUE)

  frame_data <- video_frame_output_columns(frame_data)

  backup_file <- backup_existing_file(output_file)
  write.csv(strip_qa_warnings_column(frame_data), output_file, row.names = FALSE)

  status_messages <- c(
    status_messages,
    if (!is.na(backup_file)) paste("Backed up existing video frame metadata to", basename(backup_file)) else NULL,
    paste("Wrote video frame metadata to", output_file),
    paste("Parsed", nrow(frame_data), "video frame still(s)."),
    if (malformed_n > 0) paste("Warning:", malformed_n, "video frame filename(s) did not match the expected pattern.") else NULL
  )

  frame_data <- remove_internal_video_frame_columns(frame_data)

  list(
    output_file = output_file,
    frame_data = frame_data,
    status = status_messages
  )
}

is_astro_platform_name <- function(platform_name) {
  grepl("^Astro", basename(platform_name), ignore.case = TRUE)
}

add_video_frame_qa_warnings <- function(frame_data, platform_name = NA_character_, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  if (!nrow(frame_data)) {
    frame_data$qa_warnings <- character()
    return(list(frame_data = frame_data, status = character()))
  }

  warning_lists <- vector("list", nrow(frame_data))
  add_warning <- function(row_index, message) {
    warning_lists[[row_index]] <<- c(warning_lists[[row_index]], message)
  }

  for (i in seq_len(nrow(frame_data))) {
    if ("parse_ok" %in% names(frame_data) && !isTRUE(frame_data$parse_ok[i])) {
      add_warning(i, "filename did not match expected pattern")
    }
    if (is_blank_value(frame_data$source_video_file[i])) {
      add_warning(i, "missing source video filename")
    }
    if (is.na(frame_data$source_video_time_s[i])) {
      add_warning(i, "missing source video time")
    }
    if (identical(platform_name, "Astro") && is_blank_value(frame_data$source_video_card[i])) {
      add_warning(i, "missing source video card")
    }
    if (identical(platform_name, "Astro") && is.na(frame_data$source_video_flight[i])) {
      add_warning(i, "missing inferred source video flight")
    }
    if (identical(platform_name, "Astro") && astro_video_source == "corrected") {
      add_warning(i, "corrected Astro video pixel_dimension_mm placeholder; update calibration before measurement conversion")
    }
    if (is_blank_value(frame_data$datetime_utc[i])) {
      add_warning(i, "missing GPS-derived datetime")
    }
    if (is_blank_value(frame_data$justtime[i])) {
      add_warning(i, "missing GPS-derived time")
    }
    if (is.na(frame_data$laser_alt_m[i])) {
      add_warning(i, "missing valid laser altitude")
    } else if (!valid_altitude_value(frame_data$laser_alt_m[i])) {
      add_warning(i, "laser altitude outside valid range")
    }
    if (is.na(frame_data$barometric_alt_m[i])) {
      add_warning(i, "missing valid barometric altitude")
    } else if (!valid_altitude_value(frame_data$barometric_alt_m[i])) {
      add_warning(i, "barometric altitude outside valid range")
    }
    for (col in c("pilot", "permit", "species")) {
      if (col %in% names(frame_data) && is_blank_value(frame_data[[col]][i])) {
        add_warning(i, paste("missing", col))
      }
    }
    for (col in c("FocalLength_mm", "ImageWidth_px", "ImageHeight_px", "SensorWidth_mm", "pixel_dimension_mm")) {
      if (col %in% names(frame_data) && is.na(frame_data[[col]][i])) {
        add_warning(i, paste("missing", col))
      }
    }
  }

  if ("FileName" %in% names(frame_data)) {
    duplicate_file <- duplicated(frame_data$FileName) | duplicated(frame_data$FileName, fromLast = TRUE)
    for (i in which(duplicate_file)) {
      add_warning(i, "duplicate output filename")
    }
  }

  frame_data$qa_warnings <- vapply(
    warning_lists,
    function(messages) paste(unique(messages), collapse = "; "),
    character(1)
  )
  attr(frame_data, "qa_warnings") <- frame_data$qa_warnings

  warned_rows <- sum(nzchar(frame_data$qa_warnings))
  status <- if (warned_rows > 0) {
    warning_rows <- which(nzchar(frame_data$qa_warnings))
    c(
      paste("Warning:", warned_rows, "video frame row(s) have QA warnings:"),
      paste(
        "video frame row",
        warning_rows,
        paste0("(", frame_data$FileName[warning_rows], "):"),
        frame_data$qa_warnings[warning_rows]
      )
    )
  } else {
    "QA check: no video frame metadata warnings detected."
  }

  list(frame_data = frame_data, status = status)
}

is_blank_value <- function(value) {
  length(value) == 0 || is.na(value) || !nzchar(as.character(value))
}

video_frame_output_columns <- function(frame_data) {
  output_columns <- c(
    IMGDATA_BASE_COLUMNS,
    "source_video_card",
    "source_video_flight",
    "source_video_file",
    "source_video_timecode",
    "source_video_time_s",
    "frame_number",
    setdiff(names(frame_data), c(
      IMGDATA_BASE_COLUMNS,
      "media_type",
      "frame_file",
      "parse_ok",
      "altitude_source",
      "altitude_match_time_diff_s",
      "altitude_samples_n",
      "altitude_window_s",
      "source_log_file",
      "drone_altitude_above_takeoff_m",
      "source_video_card",
      "source_video_flight",
      "source_video_file",
      "source_video_timecode",
      "source_video_time_s",
      "frame_number"
    ))
  )

  frame_data <- ensure_columns(frame_data, output_columns)
  frame_data[, output_columns, drop = FALSE]
}

write_video_inventory <- function(platform_dir, flight_date, platform_token, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  inventory <- video_inventory(platform_dir, astro_video_source)
  if (!nrow(inventory)) {
    status <- if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
      paste("Warning: Corrected Astro video selected, but no video files were found in", file.path(platform_dir, "video_corr"))
    } else {
      character()
    }
    return(list(inventory = inventory, status = status))
  }
  status <- character()
  if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
    status <- c(
      status,
      "Warning: Corrected Astro video selected. Corrected video pixel_dimension_mm is not configured yet; pixel_dimension_mm will be written as NA and must be updated before size calculations."
    )
  }
  photo_info_files <- find_drone_amplified_photo_info_files(platform_dir)
  if (length(photo_info_files)) {
    inventory <- assign_photo_info_logs_to_video_inventory(inventory, photo_info_files)
    missing_range <- is.na(inventory$folder_flight_range) | !nzchar(inventory$folder_flight_range)
    has_assigned <- "assigned_flightnum" %in% names(inventory) & !is.na(inventory$assigned_flightnum)
    inventory$folder_flight_range[missing_range & has_assigned] <-
      as.character(inventory$assigned_flightnum[missing_range & has_assigned])
  }

  output_file <- file.path(platform_dir, paste0(flight_date, "_", platform_token, "_video_inventory.csv"))
  backup_file <- backup_existing_file(output_file)
  write.csv(inventory, output_file, row.names = FALSE)

  duplicate_n <- sum(inventory$duplicate_video_name, na.rm = TRUE)
  duplicate_card_dirs <- unique(inventory$source_video_card[inventory$duplicate_video_name])
  duplicate_card_dirs <- duplicate_card_dirs[!is.na(duplicate_card_dirs) & nzchar(duplicate_card_dirs)]
  status <- c(
    status,
    if (!is.na(backup_file)) paste("Backed up existing video inventory to", basename(backup_file)) else NULL,
    paste("Wrote video inventory to", output_file),
    if (duplicate_n > 0) paste(
      "Warning:",
      duplicate_n,
      "video file(s) have duplicate names across card folders. Export VLC frames into matching video_frames card subfolders instead of renaming each frame, e.g.",
      paste(file.path("video_frames", duplicate_card_dirs), collapse = ", ")
    ) else NULL
  )

  list(inventory = inventory, status = status)
}

video_inventory <- function(platform_dir, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  video_files <- detect_platform_video_files(platform_dir, astro_video_source)
  if (!length(video_files)) {
    return(data.frame())
  }

  video_root <- if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
    file.path(platform_dir, "video_corr")
  } else {
    file.path(platform_dir, "video")
  }
  rel_path <- relative_path(video_files, video_root)
  card_label <- vapply(dirname(rel_path), video_card_label, character(1))
  video_name <- basename(video_files)
  duplicate_name <- duplicated(video_name) | duplicated(video_name, fromLast = TRUE)
  flight_range <- vapply(dirname(rel_path), video_folder_flight_range, character(1))
  filename_flight <- vapply(video_name, video_filename_flight_range, character(1))
  missing_flight_range <- is.na(flight_range) | !nzchar(flight_range)
  flight_range[missing_flight_range] <- filename_flight[missing_flight_range]
  video_xml <- vapply(video_files, sony_video_xml_path, character(1))
  video_meta <- lapply(video_xml, read_sony_video_xml_metadata)
  duration_s <- vapply(video_meta, function(x) x$duration_s, numeric(1))
  image_width_px <- vapply(video_meta, function(x) x$image_width_px, numeric(1))
  image_height_px <- vapply(video_meta, function(x) x$image_height_px, numeric(1))
  platform_video_metadata <- read_platform_video_metadata(platform_dir)
  if (nrow(platform_video_metadata)) {
    metadata_match <- match(video_metadata_match_key(video_name), video_metadata_match_key(platform_video_metadata$FileName))
    matched <- !is.na(metadata_match)
    duration_s[matched & is.na(duration_s)] <- platform_video_metadata$duration[metadata_match[matched & is.na(duration_s)]]
    image_width_px[matched & is.na(image_width_px)] <- platform_video_metadata$ImageWidth_px[metadata_match[matched & is.na(image_width_px)]]
    image_height_px[matched & is.na(image_height_px)] <- platform_video_metadata$ImageHeight_px[metadata_match[matched & is.na(image_height_px)]]
    metadata_flight <- as.character(platform_video_metadata$flightnum[metadata_match[matched]])
    metadata_flight[is.na(metadata_flight) | metadata_flight == "NA"] <- NA_character_
    matched_indices <- which(matched)
    missing_matched_flight <- is.na(flight_range[matched_indices]) | !nzchar(flight_range[matched_indices])
    flight_range[matched_indices[missing_matched_flight]] <- metadata_flight[missing_matched_flight]
  }
  focal_length_mm <- rep(if (is_astro_platform_name(platform_dir)) 24 else NA_real_, length(video_files))
  sensor_width_mm <- rep(if (is_astro_platform_name(platform_dir)) 35.7 else NA_real_, length(video_files))
  if (nrow(platform_video_metadata)) {
    metadata_match <- match(video_metadata_match_key(video_name), video_metadata_match_key(platform_video_metadata$FileName))
    matched <- !is.na(metadata_match)
    focal_length_mm[matched] <- platform_video_metadata$FocalLength_mm[metadata_match[matched]]
    sensor_width_mm[matched] <- platform_video_metadata$SensorWidth_mm[metadata_match[matched]]
  }
  pixel_dimension_mm <- if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
    rep(NA_real_, length(video_files))
  } else if (is_astro_platform_name(platform_dir)) {
    ifelse(!is.na(image_width_px) & image_width_px > 0, sensor_width_mm / image_width_px, NA_real_)
  } else {
    if (nrow(platform_video_metadata)) {
      metadata_match <- match(video_metadata_match_key(video_name), video_metadata_match_key(platform_video_metadata$FileName))
      out <- rep(NA_real_, length(video_files))
      matched <- !is.na(metadata_match)
      out[matched] <- platform_video_metadata$pixel_dimension_mm[metadata_match[matched]]
      out
    } else {
      rep(NA_real_, length(video_files))
    }
  }

  data.frame(
    source_video_card = card_label,
    source_video_file = video_name,
    source_video_relative_path = rel_path,
    source_video_duration_s = duration_s,
    FocalLength_mm = focal_length_mm,
    ImageWidth_px = image_width_px,
    ImageHeight_px = image_height_px,
    SensorWidth_mm = sensor_width_mm,
    pixel_dimension_mm = pixel_dimension_mm,
    duplicate_video_name = duplicate_name,
    folder_flight_range = flight_range,
    suggested_frame_prefix = ifelse(
      !is.na(card_label) & nzchar(card_label),
      paste0(card_label, "_", video_name, "_"),
      paste0(video_name, "_")
    ),
    stringsAsFactors = FALSE
  )
}

read_platform_video_metadata <- function(platform_dir) {
  video_dir <- file.path(platform_dir, "video")
  roots <- unique(c(video_dir, platform_dir))
  roots <- roots[dir.exists(roots)]
  if (!length(roots)) return(data.frame())

  files <- unique(unlist(lapply(roots, function(root) {
    list.files(root, pattern = "_video_metadata\\.csv$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  })))
  files <- files[!grepl("_video_frames\\.csv$|_video_inventory\\.csv$", basename(files), ignore.case = TRUE)]
  if (!length(files)) return(data.frame())

  rows <- lapply(files, function(file_path) {
    data <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(data) || !"FileName" %in% names(data)) return(data.frame())
    for (col in c("duration", "flightnum", "FocalLength_mm", "ImageWidth_px", "ImageHeight_px", "SensorWidth_mm", "pixel_dimension_mm")) {
      if (!col %in% names(data)) data[[col]] <- NA_real_
    }
    data.frame(
      FileName = basename(as.character(data$FileName)),
      duration = suppressWarnings(as.numeric(data$duration)),
      flightnum = suppressWarnings(as.numeric(data$flightnum)),
      FocalLength_mm = suppressWarnings(as.numeric(data$FocalLength_mm)),
      ImageWidth_px = suppressWarnings(as.numeric(data$ImageWidth_px)),
      ImageHeight_px = suppressWarnings(as.numeric(data$ImageHeight_px)),
      SensorWidth_mm = suppressWarnings(as.numeric(data$SensorWidth_mm)),
      pixel_dimension_mm = suppressWarnings(as.numeric(data$pixel_dimension_mm)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  if (!nrow(out)) return(out)
  out[!duplicated(tolower(out$FileName)), , drop = FALSE]
}

sony_video_xml_path <- function(video_file) {
  stem <- tools::file_path_sans_ext(video_file)
  candidate <- paste0(stem, "M01.XML")
  if (file.exists(candidate)) return(candidate)

  candidate <- paste0(stem, ".XML")
  if (file.exists(candidate)) return(candidate)

  NA_character_
}

read_sony_video_xml_metadata <- function(xml_file) {
  empty <- list(
    duration_s = NA_real_,
    image_width_px = NA_real_,
    image_height_px = NA_real_
  )
  if (is.na(xml_file) || !file.exists(xml_file)) return(empty)

  text <- paste(readLines(xml_file, warn = FALSE), collapse = "\n")
  duration_match <- regexec("<Duration\\s+value=\"([0-9]+)\"", text, ignore.case = TRUE)
  duration_parts <- regmatches(text, duration_match)[[1]]
  fps_match <- regexec("formatFps=\"([0-9.]+)p\"", text, ignore.case = TRUE)
  fps_parts <- regmatches(text, fps_match)[[1]]
  layout_match <- regexec("VideoLayout\\s+pixel=\"([0-9]+)\"\\s+numOfVerticalLine=\"([0-9]+)\"", text, ignore.case = TRUE)
  layout_parts <- regmatches(text, layout_match)[[1]]

  frame_count <- if (length(duration_parts)) suppressWarnings(as.numeric(duration_parts[2])) else NA_real_
  fps <- if (length(fps_parts)) suppressWarnings(as.numeric(fps_parts[2])) else NA_real_

  list(
    duration_s = if (!is.na(frame_count) && !is.na(fps) && fps > 0) frame_count / fps else NA_real_,
    image_width_px = if (length(layout_parts)) suppressWarnings(as.numeric(layout_parts[2])) else NA_real_,
    image_height_px = if (length(layout_parts)) suppressWarnings(as.numeric(layout_parts[3])) else NA_real_
  )
}

relative_path <- function(paths, root) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path_norm <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  sub(paste0("^", gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", root_norm), "/?"), "", path_norm)
}

video_card_label <- function(folder_name) {
  folder_name <- basename(folder_name)
  match <- regexec("card#?\\s*(\\d+)", folder_name, ignore.case = TRUE)
  parts <- regmatches(folder_name, match)[[1]]
  if (!length(parts)) return(NA_character_)
  sprintf("card%02d", suppressWarnings(as.integer(parts[2])))
}

video_folder_flight_range <- function(folder_name) {
  folder_name <- basename(folder_name)
  match <- regexec("flights?\\s*(\\d+)\\s*-\\s*(\\d+)", folder_name, ignore.case = TRUE)
  parts <- regmatches(folder_name, match)[[1]]
  if (length(parts)) return(paste0(parts[2], "-", parts[3]))

  match <- regexec("flight\\s*(\\d+)", folder_name, ignore.case = TRUE)
  parts <- regmatches(folder_name, match)[[1]]
  if (length(parts)) return(parts[2])

  NA_character_
}

video_filename_flight_range <- function(file_name) {
  file_name <- basename(file_name)
  match <- regexec("(?i)(?:^|[_-])f(\\d+)(?:[_\\.-]|$)", file_name, perl = TRUE)
  parts <- regmatches(file_name, match)[[1]]
  if (length(parts)) parts[2] else NA_character_
}

video_metadata_match_key <- function(file_name) {
  key <- tolower(basename(file_name))
  sub("(?i)_f\\d+(?=\\.[a-z0-9]+$)", "", key, perl = TRUE)
}

remove_internal_video_frame_columns <- function(frame_data) {
  internal_columns <- c(
    "media_type",
    "frame_file",
    "parse_ok",
    "altitude_source",
    "altitude_match_time_diff_s",
    "altitude_samples_n",
    "altitude_window_s",
    "source_log_file",
    "drone_altitude_above_takeoff_m"
  )
  frame_data[, setdiff(names(frame_data), internal_columns), drop = FALSE]
}

preserve_existing_video_frame_annotations <- function(frame_data, output_file) {
  if (!file.exists(output_file) || !"FileName" %in% names(frame_data)) {
    return(frame_data)
  }

  existing <- read.csv(output_file, stringsAsFactors = FALSE, check.names = FALSE)
  existing_key <- if ("FileName" %in% names(existing)) "FileName" else if ("frame_file" %in% names(existing)) "frame_file" else NA_character_
  if (is.na(existing_key)) {
    return(frame_data)
  }

  preserve_cols <- intersect(
    c("whaleinfo", GRADING_COLUMNS, "photogram_quality", "photogram_comments"),
    names(existing)
  )
  preserve_cols <- unique(preserve_cols)

  if (!length(preserve_cols)) {
    return(frame_data)
  }

  match_idx <- match(frame_data$FileName, existing[[existing_key]])
  for (col in preserve_cols) {
    if (!col %in% names(frame_data)) {
      frame_data[[col]] <- NA
    }
    existing_values <- existing[[col]][match_idx]
    current_values <- frame_data[[col]]
    current_blank <- is.na(current_values)
    if (is.character(current_values)) {
      current_blank <- current_blank | current_values == ""
    }
    replace_rows <- !is.na(existing_values) & current_blank
    frame_data[[col]][replace_rows] <- existing_values[replace_rows]
  }

  frame_data
}

apply_platform_context_from_imgdata <- function(frame_data, platform_dir) {
  status_messages <- character()
  imgdata_file <- find_platform_imgdata_file(platform_dir)
  if (is.na(imgdata_file)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  imgdata <- read.csv(imgdata_file, stringsAsFactors = FALSE, check.names = FALSE)
  context_cols <- intersect(c("pilot", "permit", "species"), names(imgdata))
  if (!length(context_cols)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  filled_cols <- character()
  for (col in context_cols) {
    value <- first_nonblank_value(imgdata[[col]])
    if (is.na(value)) next
    if (!col %in% names(frame_data)) frame_data[[col]] <- NA_character_
    blank <- is.na(frame_data[[col]]) | frame_data[[col]] == ""
    frame_data[[col]][blank] <- value
    filled_cols <- c(filled_cols, col)
  }

  if (length(filled_cols)) {
    status_messages <- paste(
      "Copied",
      paste(unique(filled_cols), collapse = ", "),
      "from",
      basename(imgdata_file),
      "into video frame metadata."
    )
  }

  list(frame_data = frame_data, status = status_messages)
}

find_platform_imgdata_file <- function(platform_dir) {
  files <- list.files(
    platform_dir,
    pattern = "_imgdata\\.csv$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  files <- files[!grepl("\\.backup-", basename(files), ignore.case = TRUE)]
  if (!length(files)) return(NA_character_)
  sort(files)[1]
}

first_nonblank_value <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(NA_character_)
  values[1]
}

assign_evo_video_frame_altitudes <- function(frame_data, platform_dir, window_s = 2, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  status_messages <- character()
  if (!nrow(frame_data) || !any(frame_data$parse_ok, na.rm = TRUE)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  pending <- is.na(frame_data$altitude_source) | frame_data$altitude_source == ""
  if (!any(pending, na.rm = TRUE)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  video_metadata_file <- find_single_video_file(platform_dir, "_video_metadata\\.csv$")
  cleaned_lidar_file <- find_single_video_file(platform_dir, "_video_CleanedLidar\\.csv$")
  if (is.na(video_metadata_file)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  video_metadata <- read_evo_video_metadata(video_metadata_file)
  if (!nrow(video_metadata)) {
    return(list(frame_data = frame_data, status = status_messages))
  }
  lidar_data <- if (!is.na(cleaned_lidar_file)) read_evo_cleaned_lidar(cleaned_lidar_file) else data.frame()
  baro_data <- read_evo_airdata_logs(platform_dir)

  assigned_n <- 0L
  assigned_baro_n <- 0L
  missing_video_n <- 0L
  missing_altitude_n <- 0L
  missing_baro_n <- 0L

  for (i in seq_len(nrow(frame_data))) {
    if (!isTRUE(frame_data$parse_ok[i]) || is.na(frame_data$source_video_time_s[i]) || !isTRUE(pending[i])) {
      next
    }

    video_row <- video_metadata[
      video_metadata_match_key(video_metadata$FileName) == video_metadata_match_key(frame_data$source_video_file[i]),
      ,
      drop = FALSE
    ]
    if (!nrow(video_row)) {
      missing_video_n <- missing_video_n + 1L
      next
    }

    frame_datetime <- video_row$start_datetime_utc[1] + frame_data$source_video_time_s[i]
    frame_log_seconds <- as.numeric(format(frame_datetime, "%H")) * 3600 +
      as.numeric(format(frame_datetime, "%M")) * 60 +
      as.numeric(format(frame_datetime, "%S"))

    frame_data$datetime_utc[i] <- format(frame_datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    frame_data$justtime[i] <- format(frame_datetime, "%H:%M:%S", tz = "GMT")
    frame_data$flightnum[i] <- video_row$flightnum[1]
    if (is.na(frame_data$source_video_flight[i]) && !is.na(frame_data$flightnum[i])) {
      frame_data$source_video_flight[i] <- frame_data$flightnum[i]
    }
    frame_data$platform[i] <- video_row$platform[1]
    frame_data$pilot[i] <- video_row$pilot[1]
    frame_data$permit[i] <- video_row$permit[1]
    frame_data$species[i] <- video_row$species[1]
    frame_data$FocalLength_mm[i] <- video_row$FocalLength_mm[1]
    frame_data$ImageWidth_px[i] <- video_row$ImageWidth_px[1]
    frame_data$ImageHeight_px[i] <- video_row$ImageHeight_px[1]
    frame_data$SensorWidth_mm[i] <- video_row$SensorWidth_mm[1]
    frame_data$pixel_dimension_mm[i] <- video_row$pixel_dimension_mm[1]

    lidar_match <- if (nrow(lidar_data)) {
      summarize_evo_lidar_altitude(
        lidar_data,
        frame_log_seconds = frame_log_seconds,
        window_s = window_s,
        valid_range_m = valid_range_m
      )
    } else {
      NULL
    }

    if (is.null(lidar_match)) {
      missing_altitude_n <- missing_altitude_n + 1L
    } else {
      frame_data$altitude_source[i] <- "evo_cleaned_lidar"
      frame_data$altitude_match_time_diff_s[i] <- lidar_match$altitude_match_time_diff_s
      frame_data$altitude_samples_n[i] <- lidar_match$altitude_samples_n
      frame_data$altitude_window_s[i] <- window_s
      frame_data$source_log_file[i] <- if (!is.na(cleaned_lidar_file)) basename(cleaned_lidar_file) else NA_character_
      frame_data$Latitude[i] <- lidar_match$Latitude
      frame_data$Longitude[i] <- lidar_match$Longitude
      frame_data$gps_alt_m[i] <- lidar_match$gps_alt_m
      frame_data$laser_alt_m[i] <- lidar_match$laser_alt_m
      frame_data$raw_laser_alt_cm[i] <- lidar_match$raw_laser_alt_cm
      frame_data$tilt_deg[i] <- lidar_match$tilt_deg
      frame_data$costilt[i] <- lidar_match$costilt
      assigned_n <- assigned_n + 1L
    }

    baro_match <- if (nrow(baro_data)) {
      summarize_evo_baro_altitude(baro_data, frame_datetime, window_s = window_s)
    } else {
      NULL
    }

    if (is.null(baro_match)) {
      missing_baro_n <- missing_baro_n + 1L
    } else {
      frame_data$barometric_alt_m[i] <- baro_match$barometric_alt_m
      assigned_baro_n <- assigned_baro_n + 1L
    }
  }

  status_messages <- c(
    status_messages,
    paste("Found EVO video metadata", if (!is.na(cleaned_lidar_file)) "and cleaned lidar files." else "file."),
    if (assigned_n > 0) paste("Assigned EVO lidar altitude to", assigned_n, "video frame still(s).") else NULL,
    if (assigned_baro_n > 0) paste("Assigned EVO barometric altitude to", assigned_baro_n, "video frame still(s).") else NULL,
    if (missing_video_n > 0) paste("Warning:", missing_video_n, "video frame still(s) did not match a row in *_video_metadata.csv.") else NULL,
    if (missing_altitude_n > 0) paste("Warning:", missing_altitude_n, "video frame still(s) had no valid Laser_Alt within +/-", window_s, "seconds.") else NULL,
    if (missing_baro_n > 0) paste("Warning:", missing_baro_n, "video frame still(s) had no EVO barometric altitude within +/-", window_s, "seconds.") else NULL
  )

  list(frame_data = frame_data, status = status_messages)
}

find_single_video_file <- function(platform_dir, pattern) {
  search_roots <- unique(c(file.path(platform_dir, "video"), platform_dir))
  search_roots <- search_roots[dir.exists(search_roots)]
  files <- unique(sort(unlist(lapply(search_roots, function(search_root) {
    list.files(
      search_root,
      pattern = pattern,
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }))))
  if (!length(files)) return(NA_character_)
  files[1]
}

read_evo_video_metadata <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"FileName" %in% names(data) || !"starttime" %in% names(data)) {
    return(data.frame())
  }
  data$start_seconds <- vapply(data$starttime, hms_to_seconds, numeric(1))
  data$end_datetime_utc <- parse_video_metadata_datetime(data)
  data$start_datetime_utc <- video_start_datetime(data$end_datetime_utc, data$starttime)
  numeric_cols <- c(
    "flightnum",
    "FocalLength_mm",
    "ImageWidth_px",
    "ImageHeight_px",
    "SensorWidth_mm",
    "pixel_dimension_mm"
  )
  for (col in numeric_cols) {
    if (col %in% names(data)) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  data[!is.na(data$start_seconds) & !is.na(data$start_datetime_utc), , drop = FALSE]
}

parse_video_metadata_datetime <- function(data) {
  if (!"datetime_utc" %in% names(data)) {
    return(as.POSIXct(rep(NA_character_, nrow(data)), tz = "GMT"))
  }
  as.POSIXct(data$datetime_utc, tz = "GMT")
}

video_start_datetime <- function(end_datetime_utc, starttime) {
  start_seconds <- vapply(starttime, hms_to_seconds, numeric(1))
  end_seconds <- as.numeric(format(end_datetime_utc, "%H")) * 3600 +
    as.numeric(format(end_datetime_utc, "%M")) * 60 +
    as.numeric(format(end_datetime_utc, "%S"))
  start_date <- as.Date(end_datetime_utc, tz = "GMT")
  start_date[!is.na(start_seconds) & !is.na(end_seconds) & start_seconds > end_seconds] <-
    start_date[!is.na(start_seconds) & !is.na(end_seconds) & start_seconds > end_seconds] - 1
  as.POSIXct(paste(start_date, starttime), tz = "GMT")
}

read_evo_cleaned_lidar <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"gmt_time" %in% names(data)) {
    return(data.frame())
  }
  data$time_seconds <- vapply(data$gmt_time, hms_to_seconds, numeric(1))
  data$laser_alt_m <- numeric_column_or_na(data, "Laser_Alt")
  data$raw_laser_alt_cm <- numeric_column_or_na(data, "laser_altitude_cm")
  data$Latitude <- numeric_column_or_na(data, "latitude")
  data$Longitude <- numeric_column_or_na(data, "longitude")
  data$gps_alt_m <- numeric_column_or_na(data, "gps_altitude_m")
  data$tilt_deg <- numeric_column_or_na(data, "tilt_deg")
  data$costilt <- numeric_column_or_na(data, "converted")
  data[!is.na(data$time_seconds), , drop = FALSE]
}

numeric_column_or_na <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }
  suppressWarnings(as.numeric(data[[column_name]]))
}

valid_altitude_value <- function(values, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  values <- suppressWarnings(as.numeric(values))
  !is.na(values) &
    is.finite(values) &
    values >= valid_range_m[["min"]] &
    values <= valid_range_m[["max"]]
}

median_valid_altitude <- function(values, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[valid_altitude_value(values, valid_range_m)]
  if (!length(values)) return(NA_real_)
  median(values, na.rm = TRUE)
}

hms_to_seconds <- function(value) {
  value <- as.character(value)
  if (is.na(value) || !nzchar(value)) return(NA_real_)
  parts <- strsplit(value, ":", fixed = TRUE)[[1]]
  if (length(parts) != 3) return(NA_real_)
  parts <- suppressWarnings(as.numeric(parts))
  if (any(is.na(parts))) return(NA_real_)
  parts[1] * 3600 + parts[2] * 60 + parts[3]
}

summarize_evo_lidar_altitude <- function(lidar_data, frame_log_seconds, window_s = 2, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  valid <- !is.na(lidar_data$time_seconds) &
    abs(lidar_data$time_seconds - frame_log_seconds) <= window_s &
    valid_altitude_value(lidar_data$laser_alt_m, valid_range_m)

  samples <- lidar_data[valid, , drop = FALSE]
  if (!nrow(samples)) return(NULL)

  list(
    laser_alt_m = median(samples$laser_alt_m, na.rm = TRUE),
    raw_laser_alt_cm = median(samples$raw_laser_alt_cm, na.rm = TRUE),
    Latitude = median(samples$Latitude, na.rm = TRUE),
    Longitude = median(samples$Longitude, na.rm = TRUE),
    gps_alt_m = median(samples$gps_alt_m, na.rm = TRUE),
    tilt_deg = median(samples$tilt_deg, na.rm = TRUE),
    costilt = median(samples$costilt, na.rm = TRUE),
    altitude_match_time_diff_s = min(abs(samples$time_seconds - frame_log_seconds), na.rm = TRUE),
    altitude_samples_n = nrow(samples)
  )
}

read_evo_airdata_logs <- function(platform_dir) {
  evo_log_dir <- file.path(platform_dir, "EVO_logs")
  if (!dir.exists(evo_log_dir)) return(data.frame())

  log_files <- list.files(evo_log_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  log_files <- log_files[!grepl("_video_frames\\.csv$", basename(log_files), ignore.case = TRUE)]
  if (!length(log_files)) return(data.frame())

  rows <- lapply(log_files, function(log_file) {
    data <- read.csv(log_file, stringsAsFactors = FALSE, check.names = FALSE)
    if (!"datetime(utc)" %in% names(data) || !"height_above_takeoff(feet)" %in% names(data)) {
      return(data.frame())
    }
    data.frame(
      datetime_utc = as.POSIXct(data[["datetime(utc)"]], tz = "GMT"),
      barometric_alt_m = suppressWarnings(as.numeric(data[["height_above_takeoff(feet)"]])) * 0.3048,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[!is.na(out$datetime_utc), , drop = FALSE]
}

summarize_evo_baro_altitude <- function(baro_data, frame_datetime_utc, window_s = 2) {
  if (is.na(frame_datetime_utc)) return(NULL)
  time_diff_s <- abs(as.numeric(difftime(baro_data$datetime_utc, frame_datetime_utc, units = "secs")))
  valid <- !is.na(time_diff_s) &
    time_diff_s <= window_s &
    valid_altitude_value(baro_data$barometric_alt_m)
  samples <- baro_data[valid, , drop = FALSE]
  if (!nrow(samples)) return(NULL)
  list(barometric_alt_m = median(samples$barometric_alt_m, na.rm = TRUE))
}

assign_drone_amplified_altitudes <- function(frame_data, platform_dir, window_s = 2, valid_range_m = VALID_ALTITUDE_RANGE_M, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  status_messages <- character()
  if (!nrow(frame_data) || !any(frame_data$parse_ok, na.rm = TRUE)) {
    return(list(frame_data = frame_data, status = status_messages))
  }

  photo_info_files <- find_drone_amplified_photo_info_files(platform_dir)
  if (!length(photo_info_files)) {
    return(list(frame_data = frame_data, status = status_messages))
  }
  inventory <- video_inventory(platform_dir, astro_video_source)
  inventory <- assign_photo_info_logs_to_video_inventory(inventory, photo_info_files)

  assigned_n <- 0L
  ambiguous_n <- 0L
  missing_n <- 0L

  for (i in seq_len(nrow(frame_data))) {
    if (!isTRUE(frame_data$parse_ok[i]) || is.na(frame_data$source_video_time_s[i])) {
      next
    }

    log_file <- choose_photo_info_log_for_frame(
      frame_data$source_video_file[i],
      photo_info_files,
      source_video_flight = frame_data$source_video_flight[i],
      source_video_card = frame_data$source_video_card[i],
      inventory = inventory
    )
    if (is.na(log_file)) {
      ambiguous_n <- ambiguous_n + 1L
      next
    }

    log_data <- read_drone_amplified_photo_info(log_file)
    match <- summarize_photo_info_altitude(
      log_data,
      frame_time_s = frame_data$source_video_time_s[i],
      window_s = window_s,
      valid_range_m = valid_range_m
    )

    if (is.null(match)) {
      missing_n <- missing_n + 1L
      next
    }

    frame_data$altitude_source[i] <- "drone_amplified_photo_info"
    frame_data$altitude_match_time_diff_s[i] <- match$altitude_match_time_diff_s
    frame_data$altitude_samples_n[i] <- match$altitude_samples_n
    frame_data$altitude_window_s[i] <- window_s
    frame_data$source_log_file[i] <- basename(log_file)
    frame_data$drone_altitude_above_takeoff_m[i] <- match$barometric_alt_m
    frame_data$Latitude[i] <- match$Latitude
    frame_data$Longitude[i] <- match$Longitude
    frame_data$laser_alt_m[i] <- match$laser_alt_m
    frame_data$raw_laser_alt_cm[i] <- match$laser_alt_m * 100
    frame_data$barometric_alt_m[i] <- match$barometric_alt_m
    frame_data$datetime_utc[i] <- if (!is.na(match$datetime_utc)) {
      format(match$datetime_utc, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    } else {
      NA_character_
    }
    frame_data$justtime[i] <- if (!is.na(match$datetime_utc)) {
      format(match$datetime_utc, "%H:%M:%S", tz = "GMT")
    } else {
      NA_character_
    }
    frame_data$flightnum[i] <- source_video_flight_number(
      frame_data$source_video_file[i],
      frame_data$source_video_flight[i]
    )
    if (is.na(frame_data$flightnum[i])) {
      frame_data$flightnum[i] <- match(log_file, photo_info_files)
    }
    if (is.na(frame_data$flightnum[i]) && "assigned_flightnum" %in% names(inventory)) {
      inv_match <- inventory[
        inventory$source_video_card == frame_data$source_video_card[i] &
          inventory$source_video_file == frame_data$source_video_file[i],
        ,
        drop = FALSE
      ]
      if (nrow(inv_match) == 1) {
        frame_data$flightnum[i] <- inv_match$assigned_flightnum[1]
      }
    }
    if (is.na(frame_data$source_video_flight[i]) && !is.na(frame_data$flightnum[i])) {
      frame_data$source_video_flight[i] <- frame_data$flightnum[i]
    }
    inv_match <- inventory[
      inventory$source_video_card == frame_data$source_video_card[i] &
        inventory$source_video_file == frame_data$source_video_file[i],
      ,
      drop = FALSE
    ]
    if (nrow(inv_match) == 1) {
      frame_data$FocalLength_mm[i] <- 24
      frame_data$ImageWidth_px[i] <- inv_match$ImageWidth_px[1]
      frame_data$ImageHeight_px[i] <- inv_match$ImageHeight_px[1]
      frame_data$SensorWidth_mm[i] <- 35.7
      if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
        frame_data$pixel_dimension_mm[i] <- NA_real_
      } else if (!is.na(frame_data$ImageWidth_px[i]) && frame_data$ImageWidth_px[i] > 0) {
        frame_data$pixel_dimension_mm[i] <- frame_data$SensorWidth_mm[i] / frame_data$ImageWidth_px[i]
      }
    }
    assigned_n <- assigned_n + 1L
  }

  auto_mapped_n <- if ("assigned_log_file" %in% names(inventory)) {
    sum(!is.na(inventory$assigned_log_file))
  } else {
    0L
  }

  status_messages <- c(
    status_messages,
    paste("Found", length(photo_info_files), "Drone Amplified photo_info log(s)."),
    if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") "Warning: Corrected Astro video selected. Corrected video pixel_dimension_mm is not configured yet; pixel_dimension_mm was written as NA and must be updated before size calculations." else NULL,
    if (auto_mapped_n > 0) paste("Mapped", auto_mapped_n, "Astro video file(s) to Drone Amplified log(s) using video duration metadata.") else NULL,
    if (assigned_n > 0) paste("Assigned Drone Amplified altitude to", assigned_n, "video frame still(s).") else NULL,
    if (ambiguous_n > 0) paste("Warning:", ambiguous_n, "video frame still(s) could not be assigned altitude because the source video did not identify a unique Drone Amplified flight log. If these are valid measurement frames, place them in a flight subfolder such as video_frames/card01/f2, or verify that the matching Sony XML sidecar is present next to the video.") else NULL,
    if (missing_n > 0) paste("Warning:", missing_n, "video frame still(s) had no valid Camera Range within +/-", window_s, "seconds.") else NULL
  )

  list(frame_data = frame_data, status = status_messages)
}

assign_photo_info_logs_to_video_inventory <- function(inventory, photo_info_files, duration_tolerance_s = 5) {
  if (!nrow(inventory) || !length(photo_info_files) || !"source_video_duration_s" %in% names(inventory)) {
    return(inventory)
  }

  log_summary <- summarize_drone_amplified_logs(photo_info_files)
  if (!nrow(log_summary)) {
    return(inventory)
  }

  inventory$assigned_log_file <- NA_character_
  inventory$assigned_flightnum <- NA_integer_

  used_video_rows <- rep(FALSE, nrow(inventory))
  for (log_i in seq_len(nrow(log_summary))) {
    candidates <- which(!used_video_rows & !is.na(inventory$source_video_duration_s))
    if (!length(candidates)) next

    duration_diff <- abs(inventory$source_video_duration_s[candidates] - log_summary$max_video_time_s[log_i])
    best <- candidates[which.min(duration_diff)]
    if (length(best) == 1 && !is.na(duration_diff[which.min(duration_diff)]) && duration_diff[which.min(duration_diff)] <= duration_tolerance_s) {
      inventory$assigned_log_file[best] <- log_summary$log_file[log_i]
      inventory$assigned_flightnum[best] <- log_summary$flightnum[log_i]
      used_video_rows[best] <- TRUE
    }
  }

  inventory <- fill_unmapped_inventory_logs_from_card_context(inventory, log_summary)

  inventory
}

fill_unmapped_inventory_logs_from_card_context <- function(inventory, log_summary) {
  if (!nrow(inventory) ||
      !"assigned_log_file" %in% names(inventory) ||
      !"source_video_card" %in% names(inventory)) {
    return(inventory)
  }

  for (card in unique(inventory$source_video_card)) {
    if (is.na(card) || !nzchar(card)) next
    card_rows <- which(inventory$source_video_card == card)
    mapped_rows <- card_rows[!is.na(inventory$assigned_flightnum[card_rows])]
    if (!length(mapped_rows)) next

    for (row_index in card_rows[is.na(inventory$assigned_flightnum[card_rows])]) {
      previous_mapped <- mapped_rows[mapped_rows < row_index]
      next_mapped <- mapped_rows[mapped_rows > row_index]
      nearest <- integer()
      if (length(previous_mapped)) nearest <- c(nearest, tail(previous_mapped, 1))
      if (length(next_mapped)) nearest <- c(nearest, head(next_mapped, 1))
      if (!length(nearest)) next

      distance <- abs(nearest - row_index)
      best <- nearest[which.min(distance)]
      if (length(best) != 1 || distance[which.min(distance)] > 2) next

      inventory$assigned_log_file[row_index] <- inventory$assigned_log_file[best]
      inventory$assigned_flightnum[row_index] <- inventory$assigned_flightnum[best]
    }
  }

  inventory
}

summarize_drone_amplified_logs <- function(photo_info_files) {
  summaries <- lapply(seq_along(photo_info_files), function(i) {
    log_data <- read_drone_amplified_photo_info(photo_info_files[i])
    video_times <- log_data$video_time_s[!is.na(log_data$video_time_s)]
    if (!length(video_times)) return(NULL)
    data.frame(
      log_file = photo_info_files[i],
      flightnum = i,
      max_video_time_s = max(video_times, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  rbind_fill_data_frames(summaries)
}

find_drone_amplified_photo_info_files <- function(platform_dir) {
  log_roots <- c(
    file.path(platform_dir, "logs"),
    file.path(platform_dir, "log"),
    platform_dir
  )
  log_roots <- unique(log_roots[dir.exists(log_roots)])
  if (!length(log_roots)) return(character())

  unique(sort(unlist(lapply(log_roots, function(log_root) {
    list.files(
      log_root,
      pattern = "_photo_info\\.csv$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }))))
}

source_video_flight_number <- function(source_video_file, source_video_flight = NA_integer_) {
  if (!is.na(source_video_flight)) {
    return(suppressWarnings(as.integer(source_video_flight)))
  }

  source_video_file <- basename(source_video_file)
  match <- regexec("_f(\\d+)\\.MP4$", source_video_file, ignore.case = TRUE)
  parts <- regmatches(source_video_file, match)[[1]]
  if (!length(parts)) return(NA_integer_)
  suppressWarnings(as.integer(parts[2]))
}

choose_photo_info_log_for_frame <- function(source_video_file,
                                            photo_info_files,
                                            source_video_flight = NA_integer_,
                                            source_video_card = NA_character_,
                                            inventory = data.frame()) {
  if (length(photo_info_files) == 1) {
    return(photo_info_files[1])
  }

  flight_number <- source_video_flight_number(source_video_file, source_video_flight)
  if (is.na(flight_number)) {
    flight_number <- source_video_flight_from_inventory(
      source_video_file = source_video_file,
      source_video_card = source_video_card,
      inventory = inventory
    )
  }
  if (!is.na(flight_number) && flight_number >= 1 && flight_number <= length(photo_info_files)) {
    return(photo_info_files[flight_number])
  }

  inventory_log <- source_video_log_from_inventory(
    source_video_file = source_video_file,
    source_video_card = source_video_card,
    inventory = inventory
  )
  if (!is.na(inventory_log)) {
    return(inventory_log)
  }

  NA_character_
}

source_video_log_from_inventory <- function(source_video_file, source_video_card, inventory) {
  if (!nrow(inventory) ||
      is.na(source_video_file) ||
      is.na(source_video_card) ||
      !"source_video_card" %in% names(inventory) ||
      !"source_video_file" %in% names(inventory) ||
      !"assigned_log_file" %in% names(inventory)) {
    return(NA_character_)
  }

  matches <- inventory[
    inventory$source_video_card == source_video_card &
      inventory$source_video_file == source_video_file,
    ,
    drop = FALSE
  ]
  if (nrow(matches) != 1 || is.na(matches$assigned_log_file[1]) || !nzchar(matches$assigned_log_file[1])) {
    return(NA_character_)
  }

  matches$assigned_log_file[1]
}

source_video_flight_from_inventory <- function(source_video_file, source_video_card, inventory) {
  if (!nrow(inventory) ||
      is.na(source_video_file) ||
      is.na(source_video_card) ||
      !"source_video_card" %in% names(inventory) ||
      !"source_video_file" %in% names(inventory) ||
      !"folder_flight_range" %in% names(inventory)) {
    return(NA_integer_)
  }

  matches <- inventory[
    inventory$source_video_card == source_video_card &
      inventory$source_video_file == source_video_file,
    ,
    drop = FALSE
  ]
  if (nrow(matches) != 1) {
    return(NA_integer_)
  }

  range_value <- matches$folder_flight_range[1]
  if (is.na(range_value) || !nzchar(range_value)) {
    return(NA_integer_)
  }

  first_flight <- suppressWarnings(as.integer(sub("-.*$", "", range_value)))
  if (is.na(first_flight)) NA_integer_ else first_flight
}

read_drone_amplified_photo_info <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  photo_info_numeric_column <- function(column_name) {
    if (!column_name %in% names(data)) {
      return(rep(NA_real_, nrow(data)))
    }
    suppressWarnings(as.numeric(data[[column_name]]))
  }

  data$video_time_s <- photo_info_numeric_column("Video Time (s)")
  data$camera_range_m <- photo_info_numeric_column("Camera Range (m)")
  data$latitude <- photo_info_numeric_column("Latitude")
  data$longitude <- photo_info_numeric_column("Longitude")
  data$barometric_alt_m <- photo_info_numeric_column("Altitude (m above takeoff location)")
  data$unix_time_gps_ms <- photo_info_numeric_column("Unix Time (ms) from Drone GPS")
  data
}

summarize_photo_info_altitude <- function(log_data, frame_time_s, window_s = 2, valid_range_m = VALID_ALTITUDE_RANGE_M) {
  valid <- !is.na(log_data$video_time_s) &
    abs(log_data$video_time_s - frame_time_s) <= window_s &
    valid_altitude_value(log_data$camera_range_m, valid_range_m)

  samples <- log_data[valid, , drop = FALSE]
  if (!nrow(samples)) return(NULL)

  list(
    laser_alt_m = median_valid_altitude(samples$camera_range_m, valid_range_m),
    barometric_alt_m = median_valid_altitude(samples$barometric_alt_m, valid_range_m),
    Latitude = median(samples$latitude, na.rm = TRUE),
    Longitude = median(samples$longitude, na.rm = TRUE),
    datetime_utc = photo_info_sample_datetime(samples),
    altitude_match_time_diff_s = min(abs(samples$video_time_s - frame_time_s), na.rm = TRUE),
    altitude_samples_n = nrow(samples)
  )
}

photo_info_sample_datetime <- function(samples) {
  gps_ms <- samples$unix_time_gps_ms[!is.na(samples$unix_time_gps_ms)]
  if (!length(gps_ms)) {
    return(as.POSIXct(NA, origin = "1970-01-01", tz = "GMT"))
  }
  as.POSIXct(median(gps_ms, na.rm = TRUE) / 1000, origin = "1970-01-01", tz = "GMT")
}

process_video_frames_for_flight_day <- function(flight_day_folder, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  status_messages <- character()
  platform_dirs <- video_frame_platform_dirs(flight_day_folder)
  root_video_frames_dir <- file.path(flight_day_folder, "video_frames")

  if (!length(platform_dirs)) {
    return("No EVO or Astro platform directories found for video frame processing.")
  }

  for (platform_dir in platform_dirs) {
    platform_video_frames_dir <- file.path(platform_dir, "video_frames")
    frame_source_dir <- platform_video_frames_dir
    has_video_frames_dir <- dir.exists(platform_video_frames_dir)
    has_platform_frame_files <- has_video_frames_dir && length(video_frame_still_files(platform_video_frames_dir)) > 0
    has_root_frame_files <- length(platform_dirs) == 1 &&
      dir.exists(root_video_frames_dir) &&
      length(video_frame_still_files(root_video_frames_dir)) > 0

    if (!has_platform_frame_files && has_root_frame_files) {
      frame_source_dir <- root_video_frames_dir
      has_video_frames_dir <- TRUE
      status_messages <- c(
        status_messages,
        paste(
          "Info: Found frame stills in the flight-day video_frames folder; writing metadata to",
          basename(platform_dir),
          "because it is the only video-capable platform folder."
        )
      )
    }

    has_video_files <- length(detect_platform_video_files(platform_dir, astro_video_source)) > 0
    if (!has_video_frames_dir && !has_video_files) {
      next
    }

    result <- process_video_frames(
      platform_dir = platform_dir,
      flight_date_directory = dirname(platform_dir),
      platform_name = basename(platform_dir),
      video_frames_dir = frame_source_dir,
      astro_video_source = astro_video_source
    )
    status_messages <- c(status_messages, result$status)
  }

  if (!length(status_messages)) {
    return("No video files or existing video_frames folders found for video frame metadata refresh.")
  }

  paste(status_messages, collapse = "\n")
}

video_frame_still_files <- function(video_frames_dir) {
  if (!dir.exists(video_frames_dir)) return(character())
  list.files(
    video_frames_dir,
    pattern = "\\.(jpe?g|png|tif|tiff)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
}

video_frame_platform_dirs <- function(path) {
  if (!dir.exists(path)) return(character())

  if (is_video_platform_dir(path)) {
    return(path)
  }

  top_level <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  child_platform_dirs <- top_level[vapply(top_level, is_video_platform_dir, logical(1))]
  if (length(child_platform_dirs)) {
    return(child_platform_dirs)
  }

  character()
}

is_video_platform_dir <- function(path) {
  basename(path) %in% c("EVO II Pro", "EVO II Dual", "Astro")
}

detect_platform_video_files <- function(platform_dir, astro_video_source = "uncorrected") {
  astro_video_source <- match.arg(astro_video_source, c("uncorrected", "corrected"))
  video_roots <- if (is_astro_platform_name(platform_dir) && astro_video_source == "corrected") {
    file.path(platform_dir, "video_corr")
  } else {
    c(
      file.path(platform_dir, "video"),
      platform_dir
    )
  }
  video_roots <- unique(video_roots[dir.exists(video_roots)])

  if (!length(video_roots)) return(character())

  unique(unlist(lapply(
    video_roots,
    function(video_root) {
      list.files(
        video_root,
        pattern = "\\.(mp4|mov|m4v)$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
    }
  )))
}

prepare_video_frame_folder <- function(platform_dir, platform_name = basename(platform_dir)) {
  video_files <- detect_platform_video_files(platform_dir)
  status_messages <- character()

  if (!length(video_files)) {
    return(list(
      video_files = video_files,
      video_frames_dir = file.path(platform_dir, "video_frames"),
      status = status_messages
    ))
  }

  video_frames_dir <- file.path(platform_dir, "video_frames")
  if (!dir.exists(video_frames_dir)) {
    dir.create(video_frames_dir, recursive = TRUE)
    status_messages <- c(
      status_messages,
      paste("Detected", length(video_files), "video file(s) in", platform_name, "- created video_frames folder for exported VLC frames.")
    )
  } else {
    status_messages <- c(
      status_messages,
      paste("Detected", length(video_files), "video file(s) in", platform_name, "- video_frames folder already exists.")
    )
  }
  if (basename(platform_dir) == "Astro") {
    card01_dir <- file.path(video_frames_dir, "card01")
    if (!dir.exists(card01_dir)) {
      dir.create(card01_dir, recursive = TRUE)
    }
  }

  list(
    video_files = video_files,
    video_frames_dir = video_frames_dir,
    status = status_messages
  )
}

prepare_video_frame_folders_for_flight_day <- function(flight_day_folder) {
  platform_dirs <- video_frame_platform_dirs(flight_day_folder)

  status_messages <- character()
  for (platform_dir in platform_dirs) {
    result <- prepare_video_frame_folder(platform_dir, basename(platform_dir))
    status_messages <- c(status_messages, result$status)
  }

  status_messages
}
