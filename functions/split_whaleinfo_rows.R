split_whaleinfo_rows <- function(data) {
  # Ensure 'whaleinfo' column exists
  if (!"whaleinfo" %in% colnames(data)) {
    stop("The 'whaleinfo' column is missing in the data.")
  }
  
  # Replace any semicolons with commas (if used as separators)
  data$whaleinfo <- gsub(";", ",", data$whaleinfo)
  
  # Split 'whaleinfo' entries by commas and trim whitespace.
  whale_list <- strsplit(as.character(data$whaleinfo), "\\s*,\\s*")
  whale_list <- lapply(whale_list, function(x) {
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (!length(x)) NA_character_ else x
  })

  row_index <- rep(seq_len(nrow(data)), lengths(whale_list))
  data <- data[row_index, , drop = FALSE]

  # Store the split whale ID in EGNO while preserving the original whaleinfo cell.
  data$EGNO <- unlist(whale_list, use.names = FALSE)
  
  # Add grading columns for image assessment
  grading_cols <- c(
    "camera_focus",
    "body_straightness",
    "body_roll",
    "body_arch",
    "body_pitch",
    "body_length_measurability",
    "body_width_measurability"
  )
  for (col in grading_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- NA
    }
  }
  
  return(data)
}

find_platform_metadata_files <- function(platform_dir) {
  imgdata_files <- list.files(
    platform_dir,
    pattern = "_imgdata\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  video_frame_files <- list.files(
    platform_dir,
    pattern = "_video_frames\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  list(
    platform_dir = platform_dir,
    platform_name = basename(platform_dir),
    imgdata_file = if (length(imgdata_files)) imgdata_files[1] else NA_character_,
    imgdata_extra_files = if (length(imgdata_files) > 1) imgdata_files[-1] else character(),
    video_frames_file = if (length(video_frame_files)) video_frame_files[1] else NA_character_,
    video_frames_extra_files = if (length(video_frame_files) > 1) video_frame_files[-1] else character()
  )
}

metadata_platform_dirs <- function(path) {
  if (dir.exists(path) && length(list.files(path, pattern = "_imgdata\\.csv$", full.names = TRUE, ignore.case = TRUE))) {
    return(path)
  }

  dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  dirs[vapply(
    dirs,
    function(d) length(list.files(d, pattern = "_imgdata\\.csv$", full.names = TRUE, ignore.case = TRUE)) > 0,
    logical(1)
  )]
}

add_annotation_columns <- function(data) {
  if (!"whaleinfo" %in% names(data)) data$whaleinfo <- NA_character_
  if (!"EGNO" %in% names(data)) data$EGNO <- NA_character_
  for (col in GRADING_COLUMNS) {
    if (!col %in% names(data)) data[[col]] <- NA
  }
  data
}

read_metadata_for_split <- function(file_path, media_type) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  data <- add_annotation_columns(data)
  data$media_type <- media_type
  data$metadata_source_file <- file_path
  data
}

combine_platform_metadata_for_split <- function(platform_dir) {
  files <- find_platform_metadata_files(platform_dir)
  rows <- list()
  status <- character()

  if (is.na(files$imgdata_file)) {
    return(list(data = data.frame(), files = files, status = paste("No _imgdata.csv found in", files$platform_name)))
  }

  if (length(files$imgdata_extra_files)) {
    status <- c(status, paste("Multiple _imgdata.csv files found in", files$platform_name, "- using", basename(files$imgdata_file)))
  }
  rows[[length(rows) + 1]] <- read_metadata_for_split(files$imgdata_file, "still_image")

  if (!is.na(files$video_frames_file)) {
    if (length(files$video_frames_extra_files)) {
      status <- c(status, paste("Multiple _video_frames.csv files found in", files$platform_name, "- using", basename(files$video_frames_file)))
    }
    rows[[length(rows) + 1]] <- read_metadata_for_split(files$video_frames_file, "video_frame")
  }

  list(
    data = rbind_fill_data_frames(rows),
    files = files,
    status = status
  )
}

split_metadata_by_whaleinfo <- function(data) {
  split_whaleinfo_rows(data)
}

write_split_metadata_files <- function(split_data) {
  status <- character()
  source_files <- unique(split_data$metadata_source_file)

  for (source_file in source_files) {
    source_rows <- split_data[split_data$metadata_source_file == source_file, , drop = FALSE]
    source_rows$metadata_source_file <- NULL
    source_rows$media_type <- NULL
    original_columns <- names(read.csv(source_file, nrows = 0, check.names = FALSE))
    output_columns <- unique(c(original_columns, GRADING_COLUMNS))
    source_rows <- ensure_columns(source_rows, output_columns)
    source_rows <- source_rows[, output_columns, drop = FALSE]

    backup_file <- backup_existing_file(source_file)
    write.csv(source_rows, source_file, row.names = FALSE)

    status <- c(
      status,
      if (!is.na(backup_file)) paste("Backed up", basename(source_file), "to", basename(backup_file)) else NULL,
      paste("Wrote", nrow(source_rows), "split row(s) to", source_file)
    )
  }

  status
}

split_whaleinfo_metadata_folder <- function(path) {
  platform_dirs <- metadata_platform_dirs(path)
  if (!length(platform_dirs)) {
    return(list(
      data = data.frame(),
      status = paste("No platform metadata files found in", path)
    ))
  }

  all_data <- list()
  status <- character()

  for (platform_dir in platform_dirs) {
    combined <- combine_platform_metadata_for_split(platform_dir)
    status <- c(status, combined$status)
    if (!nrow(combined$data)) next

    split_data <- split_metadata_by_whaleinfo(combined$data)
    write_status <- write_split_metadata_files(split_data)
    status <- c(status, paste("Processed", basename(platform_dir)), write_status)
    all_data[[length(all_data) + 1]] <- split_data
  }

  list(
    data = if (length(all_data)) rbind_fill_data_frames(all_data) else data.frame(),
    status = status
  )
}
