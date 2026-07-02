astro_image_groups <- function(astro_directory, astro_image_source = "uncorrected") {
  astro_image_source <- match.arg(astro_image_source, c("uncorrected", "corrected"))
  legacy_flights_dir <- file.path(astro_directory, "flights")
  if (astro_image_source == "uncorrected" && dir.exists(legacy_flights_dir)) {
    flight_folders <- list.dirs(legacy_flights_dir, full.names = TRUE, recursive = FALSE)
    flight_folders <- flight_folders[grepl("^\\d+_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}$", basename(flight_folders))]
    if (length(flight_folders)) {
      numeric_id <- as.integer(sub("_.*", "", basename(flight_folders)))
      out <- data.frame(
        folder = flight_folders,
        numeric_id = numeric_id,
        stringsAsFactors = FALSE
      )
      out <- out[order(out$numeric_id), , drop = FALSE]
      out$flightnum_day <- seq_len(nrow(out))
      return(out)
    }
  }

  jpg_dir <- file.path(astro_directory, if (astro_image_source == "corrected") "jpg_corr" else "jpg")
  if (!dir.exists(jpg_dir)) {
    return(data.frame())
  }

  candidate_dirs <- list.dirs(jpg_dir, recursive = FALSE, full.names = TRUE)
  candidate_dirs <- candidate_dirs[vapply(
    candidate_dirs,
    function(path) length(list.files(path, pattern = "\\.jpe?g$", ignore.case = TRUE, full.names = TRUE)) > 0,
    logical(1)
  )]

  root_images <- list.files(jpg_dir, pattern = "\\.jpe?g$", ignore.case = TRUE, full.names = TRUE)
  if (length(root_images)) {
    candidate_dirs <- c(jpg_dir, candidate_dirs)
  }

  if (!length(candidate_dirs)) {
    return(data.frame())
  }

  card_number <- vapply(candidate_dirs, function(path) {
    match <- regexec("card#?\\s*(\\d+)", basename(path), ignore.case = TRUE)
    parts <- regmatches(basename(path), match)[[1]]
    if (length(parts)) suppressWarnings(as.integer(parts[2])) else NA_integer_
  }, integer(1))

  sort_key <- ifelse(is.na(card_number), seq_along(candidate_dirs) + 1000L, card_number)
  out <- data.frame(
    folder = candidate_dirs,
    numeric_id = sort_key,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$numeric_id), , drop = FALSE]
  out$flightnum_day <- seq_len(nrow(out))
  out
}

read_astro_photo_info_triggers <- function(astro_directory) {
  log_roots <- c(
    file.path(astro_directory, "log"),
    file.path(astro_directory, "logs")
  )
  log_roots <- unique(log_roots[dir.exists(log_roots)])
  if (!length(log_roots)) return(data.frame())

  photo_info_files <- unique(sort(unlist(lapply(log_roots, function(log_root) {
    list.files(
      log_root,
      pattern = "_photo_info\\.csv$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }))))
  if (!length(photo_info_files)) return(data.frame())

  rows <- lapply(photo_info_files, function(file_path) {
    data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    if (!"Image File" %in% names(data)) return(data.frame())

    image_file <- as.character(data[["Image File"]])
    has_image <- !is.na(image_file) & nzchar(image_file) & toupper(image_file) != "NA"
    data <- data[has_image, , drop = FALSE]
    if (!nrow(data)) return(data.frame())

    laser_alt_m <- suppressWarnings(as.numeric(data[["Camera Range (m)"]]))
    laser_alt_m[is.na(laser_alt_m) | laser_alt_m < VALID_ALTITUDE_RANGE_M[["min"]] | laser_alt_m > VALID_ALTITUDE_RANGE_M[["max"]]] <- NA_real_

    data.frame(
      trigger_time_ms = suppressWarnings(as.numeric(data[["Unix Time (ms) from Drone GPS"]])),
      logged_image_file = basename(data[["Image File"]]),
      Latitude = suppressWarnings(as.numeric(data[["Latitude"]])),
      Longitude = suppressWarnings(as.numeric(data[["Longitude"]])),
      laser_alt_m = laser_alt_m,
      barometric_alt_m = suppressWarnings(as.numeric(data[["Altitude (m above takeoff location)"]])),
      source_log_file = basename(file_path),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  if (!nrow(out)) return(out)
  out <- out[order(out$trigger_time_ms), , drop = FALSE]
  row.names(out) <- NULL
  out
}

is_astro_sd_card_group <- function(folder) {
  grepl("^card\\d+$", basename(folder), ignore.case = TRUE)
}

# Function to process Astro flights
process_astro <- function(astro_directory, species, pilot, permit, flight_date_directory, baro_offset_m = 0, astro_image_source = "uncorrected") {
  astro_image_source <- match.arg(astro_image_source, c("uncorrected", "corrected"))
  warning_msgs <- character()
  
  withProgress(message = "Processing Astro flight data...", value = 0, {
    
    incProgress(0.1, detail = "Locating Astro flight folders...")
    
    image_groups <- astro_image_groups(astro_directory, astro_image_source)
    if (!nrow(image_groups)) {
      warning_msgs <- c(warning_msgs, paste("Warning: No Astro", astro_image_source, "image folders found in", astro_directory))
      return(NULL)
    }
    if (astro_image_source == "corrected") {
      warning_msgs <- c(
        warning_msgs,
        "Warning: Corrected Astro still imagery selected. Corrected still pixel_dimension_mm is not configured yet; pixel_dimension_mm will be written as NA and must be updated before size calculations."
      )
    }
    
    # Collect all images from all flight folders in a single data frame
    astro_all <- data.frame()
    photo_info_triggers <- read_astro_photo_info_triggers(astro_directory)
    photo_info_cursor <- 1L
    
    incProgress(0.2, detail = "Reading EXIF for Astro image folders...")
    for (i in seq_len(nrow(image_groups))) {
      fnum <- image_groups$flightnum_day[i]
      fpath <- image_groups$folder[i]
      
      # List .jpg images in this flight folder
      image_files <- sort(list.files(fpath, pattern = "\\.jpe?g$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE))
      if (length(image_files) == 0) {
        warning_msgs <- c(warning_msgs, paste("Warning: No images found in Astro flight folder", fpath))
        next
      }
      
      # Read EXIF data including DistanceToSubject
      exif_data <- exif_read(image_files, tags = c(
        "FileName",
        "DateTimeOriginal",
        "GPSLatitude",
        "GPSLongitude",
        "GPSAltitude#",   
        "FocalLength",
        "ImageWidth",
        "ImageHeight",
        "DistanceToSubject"
      ), recursive = FALSE)
      
      if (nrow(exif_data) == 0) {
        warning_msgs <- c(warning_msgs, paste("Warning: EXIF read returned no data for folder", fpath))
        next
      }

      # Keep EXIF rows aligned with sorted filenames for order-based trigger matching.
      exif_data <- exif_data[match(basename(image_files), exif_data$FileName), , drop = FALSE]
      exif_data$photo_info_source_log <- NA_character_
      
      # Convert DateTimeOriginal to POSIX
      exif_data$dt <- ymd_hms(exif_data$DateTimeOriginal, tz = "UTC")  # or "America/New_York" if desired
      # We'll store it as 'datetime_utc'
      exif_data$datetime_utc <- exif_data$dt
      exif_data$justtime <- format(exif_data$datetime_utc, "%H:%M:%S")
      
      # For 'ImageNum', we want the last 10 chars of the filename (minus extension)
      # Example: "250117_191519_293.jpg" => "191519_293"
      exif_data$base_name <- tools::file_path_sans_ext(basename(exif_data$FileName))
      exif_data$ImageNum <- substring(exif_data$base_name, 
                                      nchar(exif_data$base_name) - 9, 
                                      nchar(exif_data$base_name))
      
      # 'corralt' = DistanceToSubject
      if ("DistanceToSubject" %in% names(exif_data)) {
        exif_data$corralt <- exif_data$DistanceToSubject
      } else {
        exif_data$corralt <- NA
      }
      
      # Set columns that are known to be NA or unknown for Astro
      exif_data$barometric_alt <- NA
      exif_data$laser_altitude_cm <- NA
      exif_data$tilt_deg <- NA
      exif_data$costilt <- NA
      
      # Flight number for the day
      exif_data$flightnum <- fnum
      
      # Platform is always "Astro"
      exif_data$platform <- "Astro"
      
      # Pilot, permit, species from user inputs
      exif_data$pilot <- pilot
      exif_data$permit <- permit
      exif_data$species <- species
      exif_data$whaleinfo <- NA
      
      # Focal length is read directly if available; sensor width for full-frame ~ 35.7 mm
      exif_data$SensorWidth_mm <- 35.7
      
      # Corrected imagery is cropped by lens-distortion correction; placeholder
      # NA prevents downstream size calculations from using an uncorrected pixel size.
      if (astro_image_source == "corrected") {
        exif_data$pixel_dimension_mm <- NA_real_
      } else {
        exif_data$pixel_dimension_mm <- with(exif_data, SensorWidth_mm / ImageWidth)
      }
      
      # For consistency with other drones:
      # rename "GPSAltitude#" to "gps_altitude_m" (if present)
      if ("GPSAltitude#" %in% names(exif_data)) {
        exif_data$gps_altitude_m <- exif_data$GPSAltitude#
      } else {
        exif_data$gps_altitude_m <- NA
      }

      if (is_astro_sd_card_group(fpath)) {
        idx <- seq(photo_info_cursor, length.out = nrow(exif_data))
        valid_idx <- idx[idx <= nrow(photo_info_triggers)]

        if (length(valid_idx) == nrow(exif_data)) {
          trigger_rows <- photo_info_triggers[valid_idx, , drop = FALSE]
          exif_data$GPSLatitude <- trigger_rows$Latitude
          exif_data$GPSLongitude <- trigger_rows$Longitude
          exif_data$corralt <- trigger_rows$laser_alt_m
          exif_data$laser_altitude_cm <- trigger_rows$laser_alt_m * 100
          exif_data$barometric_alt <- trigger_rows$barometric_alt_m
          exif_data$photo_info_source_log <- trigger_rows$source_log_file
          photo_info_cursor <- photo_info_cursor + nrow(exif_data)
        } else {
          warning_msgs <- c(
            warning_msgs,
            paste(
              "Warning: Not enough Drone Amplified photo_info trigger rows to assign all SD-card Astro images in",
              basename(fpath),
              "- found",
              max(0, nrow(photo_info_triggers) - photo_info_cursor + 1),
              "remaining trigger row(s) for",
              nrow(exif_data),
              "image(s)."
            )
          )
        }
      }
      
      # Additional columns for photogrammetry
      exif_data$photogram_quality <- 0
      exif_data$photogram_comments <- ""
      
      # Keep only the columns we need:
      # (Below is a union of your existing evo or aph columns, plus astro columns.)
      all_cols <- c("FileName", "ImageNum", "datetime_utc", "justtime", "flightnum", 
                    "platform", "pilot", "permit", "species", "whaleinfo",
                    "GPSLatitude", "GPSLongitude", "gps_altitude_m", 
                    "laser_altitude_cm", "tilt_deg", "costilt", "corralt",
                    "barometric_alt", "FocalLength", "ImageWidth", "ImageHeight", "SensorWidth_mm",
                    "pixel_dimension_mm", "photogram_quality", "photogram_comments")
      
      # Make sure columns exist (some may be missing)
      for (col in all_cols) {
        if (!col %in% names(exif_data)) {
          exif_data[[col]] <- NA
        }
      }
      
      exif_data <- exif_data[ , all_cols, drop = FALSE]
      
      # Bind to the master Astro data frame
      astro_all <- rbind(astro_all, exif_data)
    }
    
    astro_all <- astro_all %>%
      # 2. Compute all the "m" and "cm" fields to match your other outputs:
      mutate(
        gps_alt_m        = gps_altitude_m,
        raw_laser_alt_cm = ifelse(!is.na(laser_altitude_cm),
                                  laser_altitude_cm * 100,
                                  NA_real_),
        tilt_deg         = NA_real_,
        costilt          = NA_real_,
        laser_alt_m      = corralt,
        barometric_alt_m = apply_barometric_offset(barometric_alt, baro_offset_m),
        FocalLength_mm   = FocalLength,
        ImageWidth_px    = ImageWidth,
        ImageHeight_px   = ImageHeight,  
      ) %>%
      
      # 3. Reorder & select exactly the columns you listed:
      transmute(
        FileName,
        ImageNum,
        datetime_utc,
        justtime,
        flightnum,
        platform,
        pilot,
        permit,
        species,
        whaleinfo,
        Latitude       = GPSLatitude,
        Longitude      = GPSLongitude,
        gps_alt_m,
        raw_laser_alt_cm,
        tilt_deg,
        costilt,
        laser_alt_m,
        barometric_alt_m,
        FocalLength_mm,
        ImageWidth_px,
        ImageHeight_px,
        SensorWidth_mm,
        pixel_dimension_mm
      )
    
    # 4. Write out exactly the same schema:
    output_file <- file.path(
      astro_directory,
      paste0(basename(flight_date_directory), "_Astro_imgdata.csv")
    )
    astro_all <- add_imgdata_qa_warnings(astro_all, "Astro")
    if (astro_image_source == "corrected") {
      warning_text <- "corrected Astro still pixel_dimension_mm placeholder; update calibration before measurement conversion"
      if (!"qa_warnings" %in% names(astro_all)) astro_all$qa_warnings <- NA_character_
      astro_all$qa_warnings <- ifelse(
        is.na(astro_all$qa_warnings) | !nzchar(astro_all$qa_warnings),
        warning_text,
        paste(astro_all$qa_warnings, warning_text, sep = "; ")
      )
    }
    warning_msgs <- c(warning_msgs, imgdata_qa_status(astro_all, "Astro"))
    backup_file <- backup_existing_file(output_file)
    if (!is.na(backup_file)) {
      warning_msgs <- c(warning_msgs, paste("Info: Existing Astro imgdata backed up to", basename(backup_file)))
    }
    write.csv(astro_all, file = output_file, row.names = FALSE)
    incProgress(0.2, detail = "Astro processing completed!")
  })
  
  return(warning_msgs)
}


