process_photos_measured <- function(flight_day_folder, overwrite_imgdata = TRUE) {
  # Initialize a vector to store status messages
  status_messages <- c()
  
  # --- helpers ---
  normalize_grading_names <- function(df) {
    # Map common misspellings -> canonical names
    name_map <- c(
      "body_length_measureability" = "body_length_measurability",
      "body_width_measureability"  = "body_width_measurability"
    )
    for (old in names(name_map)) {
      if (old %in% names(df)) {
        new <- name_map[[old]]
        # Only rename if target doesn't already exist
        if (!(new %in% names(df))) {
          names(df)[names(df) == old] <- new
        } else {
          # If both exist, merge data into the canonical column where canonical is NA
          idx_old <- names(df) == old
          idx_new <- names(df) == new
          na_new  <- is.na(df[[which(idx_new)]] )
          df[[which(idx_new)]][na_new] <- suppressWarnings(as.numeric(df[[which(idx_old)]][na_new]))
          # then drop the misspelled column
          df[[which(idx_old)]] <- NULL
        }
      }
    }
    df
  }
  
  as_num <- function(x) suppressWarnings(as.numeric(x))
  
  # robust file find: exact filename match in raw/derived media folders only
  find_image_paths <- function(platform_dir, filename, media_type = NA_character_, source_video_card = NA_character_) {
    # escape regex special chars in filename
    esc <- gsub("([.()\\^$|*+?{}\\[\\]\\\\])", "\\\\\\1", filename)
    roots <- if (identical(media_type, "video_frame")) {
      if (!is.na(source_video_card) && nzchar(source_video_card)) {
        card_root <- file.path(platform_dir, "video_frames", source_video_card)
        if (dir.exists(card_root)) card_root else file.path(platform_dir, "video_frames")
      } else {
        file.path(platform_dir, "video_frames")
      }
    } else {
      c(
        file.path(platform_dir, "jpg_corr"),
        file.path(platform_dir, "jpg")
      )
    }
    roots <- unique(roots[dir.exists(roots)])
    if (!length(roots)) return(character())

    unique(unlist(lapply(roots, function(root) {
      matches <- list.files(
        root,
        pattern = paste0("^", esc, "$"),
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      matches
    })))
  }
  
  # Core directory processor (APH, EVO, Astro)
  process_directory <- function(platform_dir) {
    platform_name <- basename(platform_dir)
    status_messages <<- c(status_messages, paste("Processing", platform_name, "directory..."))
    
    # Extract flight date from flight_day_folder
    flight_date <- basename(flight_day_folder)
    
    # Step 1: Create Photos_Measured folder
    photos_measured_dir <- file.path(platform_dir, "Photos_Measured")
    if (!dir.exists(photos_measured_dir)) {
      dir.create(photos_measured_dir, recursive = TRUE)
      status_messages <<- c(status_messages, paste("Created Photos_Measured folder in", platform_name))
    } else {
      status_messages <<- c(status_messages, paste("Photos_Measured folder already exists in", platform_name))
    }
    
    # Step 2: Locate metadata CSV files
    imgdata_files <- list.files(platform_dir, pattern = "_imgdata\\.csv$", full.names = TRUE, ignore.case = TRUE)
    video_frame_files <- list.files(platform_dir, pattern = "_video_frames\\.csv$", full.names = TRUE, ignore.case = TRUE)
    metadata_files <- c(
      if (length(imgdata_files)) imgdata_files[1] else character(),
      if (length(video_frame_files)) video_frame_files[1] else character()
    )
    if (length(metadata_files) == 0) {
      status_messages <<- c(status_messages, paste("No metadata CSV file found in", platform_name))
      return(invisible(NULL))
    }
    if (length(imgdata_files) > 1) {
      status_messages <<- c(status_messages, paste("Multiple _imgdata.csv files found in", platform_name, "- using the first one"))
    }
    if (length(video_frame_files) > 1) {
      status_messages <<- c(status_messages, paste("Multiple _video_frames.csv files found in", platform_name, "- using the first one"))
    }
    
    # Step 3: Read metadata files
    metadata_rows <- lapply(metadata_files, function(metadata_file) {
      data <- read.csv(metadata_file, stringsAsFactors = FALSE, check.names = FALSE)
      data$metadata_source_file <- metadata_file
      data$metadata_row_id <- seq_len(nrow(data))
      data$media_type <- if (grepl("_video_frames\\.csv$", basename(metadata_file), ignore.case = TRUE)) "video_frame" else "still_image"
      data
    })
    imgdata <- rbind_fill_data_frames(metadata_rows)
    
    # Normalize misspelled grading names so we don't create dup columns
    imgdata <- normalize_grading_names(imgdata)
    
    # Recompute photogram_quality as the sum of grading columns
    grading_cols <- c(
      "camera_focus",
      "body_straightness",
      "body_roll",
      "body_arch",
      "body_pitch",
      "body_length_measurability",
      "body_width_measurability"
    )
    
    # Ensure tracking columns exist but DO NOT create the three "problem" columns unless truly absent.
    for (col in grading_cols) {
      if (!col %in% names(imgdata)) {
        imgdata[[col]] <- NA_real_
      } else {
        # coerce to numeric if needed (quietly)
        imgdata[[col]] <- as_num(imgdata[[col]])
      }
    }
    
    # 2) Compute flag: any NA or zero?
    poor_qual <- apply(
      imgdata[grading_cols],
      1,
      function(x) any(is.na(x) | x == 0)
    )
    
    # 3) Sum scores (na.rm=TRUE to ignore NA if any)
    summed_scores <- rowSums(imgdata[grading_cols], na.rm = TRUE)
    
    # 4) Assign photogram_quality or NA
    imgdata$photogram_quality <- ifelse(poor_qual, NA, summed_scores)
    
    # --- IMPORTANT: Do NOT overwrite metadata unless requested ---
    if (isTRUE(overwrite_imgdata)) {
      for (metadata_file in unique(imgdata$metadata_source_file)) {
        source_rows <- imgdata[imgdata$metadata_source_file == metadata_file, , drop = FALSE]
        source_rows$metadata_source_file <- NULL
        source_rows$metadata_row_id <- NULL
        original_columns <- names(read.csv(metadata_file, nrows = 0, check.names = FALSE))
        output_columns <- unique(c(original_columns, grading_cols, "photogram_quality", "photogram_comments"))
        source_rows <- ensure_columns(source_rows, output_columns)
        source_rows <- source_rows[, output_columns, drop = FALSE]

        backup_file <- backup_existing_file(metadata_file)
        write.csv(strip_qa_warnings_column(source_rows), metadata_file, row.names = FALSE)
        status_messages <<- c(
          status_messages,
          if (!is.na(backup_file)) paste("Backed up existing metadata to", basename(backup_file)) else NULL,
          paste("Recomputed photogram_quality and overwrote", basename(metadata_file))
        )
      }
    } else {
      status_messages <<- c(
        status_messages,
        "Computed photogram_quality in-memory (did not overwrite original metadata CSVs)."
      )
    }
    
    # Filter rows with good quality
    filtered_data <- imgdata[!is.na(imgdata$photogram_quality) & imgdata$photogram_quality > 0, , drop = FALSE]
    
    if (nrow(filtered_data) == 0) {
      status_messages <<- c(status_messages, paste("No rows with photogram_quality > 0 in", platform_name))
      return(invisible(NULL))
    }
    
    # Ensure photogram_comments exists
    if (!"photogram_comments" %in% names(filtered_data)) {
      filtered_data$photogram_comments <- NA_character_
    }
    
    # Ensure EGNO is present & usable
    if (!"EGNO" %in% names(filtered_data)) {
      filtered_data$EGNO <- NA_character_
    }
    filtered_data$EGNO <- trimws(as.character(filtered_data$EGNO))
    
    # Output filtered CSV to Photos_Measured
    output_csv_file <- file.path(
      photos_measured_dir,
      paste0(flight_date, "_", platform_name, "_photos_measured.csv")
    )
    backup_pm_file <- backup_existing_file(output_csv_file)
    output_filtered_data <- filtered_data
    output_filtered_data$metadata_source_file <- NULL
    output_filtered_data$metadata_row_id <- NULL
    video_provenance_cols <- c(
      "media_type",
      "source_video_card",
      "source_video_flight",
      "source_video_file",
      "source_video_timecode",
      "source_video_time_s",
      "frame_number"
    )
    output_filtered_data[intersect(video_provenance_cols, names(output_filtered_data))] <- NULL
    write.csv(strip_qa_warnings_column(output_filtered_data), output_csv_file, row.names = FALSE)
    status_messages <<- c(
      status_messages,
      if (!is.na(backup_pm_file)) paste("Backed up existing photos_measured CSV to", basename(backup_pm_file)) else NULL,
      paste("Filtered data saved to", output_csv_file)
    )
    
    # Step 7: Identify unique EGNO values (non-empty)
    unique_egnos <- unique(filtered_data$EGNO)
    unique_egnos <- unique_egnos[!is.na(unique_egnos) & unique_egnos != ""]
    status_messages <<- c(status_messages, paste("Found", length(unique_egnos), "unique EGNOs in", platform_name))
    
    # Step 8: Create subfolders for each EGNO
    for (egno in unique_egnos) {
      egno_folder <- file.path(photos_measured_dir, egno)
      if (!dir.exists(egno_folder)) {
        dir.create(egno_folder, recursive = TRUE)
        status_messages <<- c(status_messages, paste("Created folder for EGNO", egno, "in", platform_name))
      }
    }
    
    # Step 9: Copy images to EGNO subfolders (robust recursive search for each FileName)
    for (egno in unique_egnos) {
      egno_folder <- file.path(photos_measured_dir, egno)
      egno_rows <- subset(filtered_data, EGNO == egno)
      file_rows <- unique(egno_rows[, intersect(c("FileName", "media_type", "source_video_card"), names(egno_rows)), drop = FALSE])
      
      for (row_i in seq_len(nrow(file_rows))) {
        filename <- file_rows$FileName[row_i]
        media_type <- if ("media_type" %in% names(file_rows)) file_rows$media_type[row_i] else NA_character_
        source_video_card <- if ("source_video_card" %in% names(file_rows)) file_rows$source_video_card[row_i] else NA_character_
        hits <- find_image_paths(platform_dir, filename, media_type, source_video_card)
        
        if (length(hits) > 0) {
          # Prefer the first match; you can change to pick newest if needed
          ok <- file.copy(hits[1], egno_folder, overwrite = FALSE)
          if (!ok) {
            status_messages <<- c(status_messages, paste("Failed to copy:", filename, "→", egno_folder))
          }
        } else {
          status_messages <<- c(
            status_messages,
            paste("Image file not found:", filename, "for EGNO", egno, "in", platform_name)
          )
        }
      }
      status_messages <<- c(status_messages, paste("Copied images for EGNO", egno, "in", platform_name))
    }
  }
  
  # ---- Find platform directories (top-level under the flight_day_folder) ----
  top_level <- list.dirs(flight_day_folder, recursive = FALSE, full.names = TRUE)
  
  evo_dirs   <- top_level[grepl("^EVO", basename(top_level), ignore.case = FALSE)]
  aph_dirs   <- top_level[grepl("^APH", basename(top_level), ignore.case = FALSE)]
  astro_dirs <- top_level[grepl("^Astro", basename(top_level), ignore.case = FALSE)]
  
  # Process them
  if (length(evo_dirs) > 0)  for (d in evo_dirs)   process_directory(d) else status_messages <- c(status_messages, "No EVO directories found.")
  if (length(aph_dirs) > 0)  for (d in aph_dirs)   process_directory(d) else status_messages <- c(status_messages, "No APH directories found.")
  if (length(astro_dirs) > 0)for (d in astro_dirs) process_directory(d) else status_messages <- c(status_messages, "No Astro directories found.")
  
  return(paste(status_messages, collapse = "\n"))
}
