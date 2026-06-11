process_evo_dual <- function(evo_directory, timeoff_dual, species, pilot, permit, flight_date_directory,
                             baro_offset_m = 0) {
  warning_msgs <- character()
  
  # Helper function to find the closest barometric reading
  find_closest_baro <- function(image_time, baro_log, timeoff) {
    if (is.null(baro_log) || nrow(baro_log) == 0) {
      return(NA)
    }
    
    # Convert 'datetime(utc)' to POSIXct and correct with timeoff
    baro_log$datetime_utc <- as.POSIXct(baro_log$'datetime(utc)', format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    baro_log$datetime_utc_corr <- baro_log$datetime_utc + timeoff
    
    # Compute time difference in seconds
    time_diffs <- abs(difftime(image_time, baro_log$datetime_utc_corr, units = "secs"))
    
    # Find the baro readings within +/- 2 seconds and with 'Picture taken' message
    within_window <- which(time_diffs <= 2 & grepl("Picture taken", baro_log$message))
    
    if (length(within_window) > 0) {
      closest_idx <- within_window[which.min(time_diffs[within_window])]
      closest_match <- baro_log$'height_above_takeoff(feet)'[closest_idx]
      closest_match <- closest_match * 0.3048  # Convert from feet to meters
    } else {
      closest_match <- NA
    }
    
    return(closest_match)
  }
  
  
  withProgress(message = "Processing EVO II Dual flight data...", value = 0, {
    # Step 1: Load EVO log files for barometric altitude values
    incProgress(0.1, detail = "Loading EVO log files...")
    baro_logs <- file.path(evo_directory, "EVO_logs")
    baro_files <- list.files(baro_logs, pattern = "\\.csv$", full.names = TRUE)
    if (length(baro_files) == 0) {
      warning_msgs <- c(warning_msgs, "Warning: No EVO log files found for EVO II Dual Barometric altitude will be set to NA.")
      baro_log <- NULL
    } else {
      baro_log_list <- lapply(baro_files, data.table::fread)
      baro_log <- data.table::rbindlist(baro_log_list, fill = TRUE)
    }
    
    # Step 2: Loading image EXIF metadata
    incProgress(0.1, detail = "Loading image EXIF metadata...")
    img_dir <- file.path(evo_directory, "jpg")
    if (!dir.exists(img_dir)) {
      warning_msgs <- c(warning_msgs, "Error: No image directory found in EVO II Dual folder.")
      return(NULL)
    }
    y <- exif_read(img_dir, tags = c("FileName", "DateTimeOriginal", "GPSLatitude", "GPSLongitude", "GPSAltitude#", "FocalLength", "ImageWidth"), recursive = TRUE)
    
    if ("GPSAltitude#" %in% names(y)) {
      y$gps_alt_m <- as.numeric(y[["GPSAltitude#"]]); y[["GPSAltitude#"]] <- NULL
    } else {
      y$exif_gps_m <- rep(NA_real_, nrow(y))
    }
    if (nrow(y) == 0) {
      warning_msgs <- c(warning_msgs, "Warning: No images found or no EXIF data in images for EVO II Dual")
      return(NULL)
    }
    y$dt           <- ymd_hms(y$DateTimeOriginal, tz = "America/New_York")
    y$dtGMT        <- with_tz(y$dt, "GMT")
    y$dtGMTcorr    <- y$dtGMT + timeoff_dual
    y$justtime     <- format(y$dtGMTcorr, "%H:%M:%S")
    y$species      <- species; y$whaleinfo <- NA; y$pilot <- pilot; y$permit <- permit
    
    # SensorWidth and pixel dimension for EVO II Dual
    y$ImageWidth_px      <- y$ImageWidth
    y$SensorWidth_mm     <- 13.2
    y$pixel_dimension_mm <- 13.2 / y$ImageWidth
    
    # Warn on unexpected widths
    valid_widths <- c(5472, 3840)
    unknown_widths <- y$ImageWidth[!y$ImageWidth %in% valid_widths]
    if (any(!is.na(unknown_widths))) {
      warning_msgs <- c(warning_msgs, paste("Warning: Found images with unexpected ImageWidth(s) in EVO II Dual:", paste(unique(unknown_widths), collapse = ", ")))
    }
    
    # Step 3: Laser Altimeter log
    incProgress(0.1, detail = "Processing Laser Altimeter log...")
    log_files <- dir(file.path(evo_directory, "log"), pattern = "\\.CSV$", full.names = TRUE)
    if (length(log_files) == 0) {
      warning_msgs <- c(warning_msgs, "Warning: No Laser Altimeter log files found for EVO II Dual")
      alllogs <- data.frame(gmt_time = y$justtime, laser_altitude_cm = NA, tilt_deg = NA, costilt = NA, corralt = NA)
    } else {
      log_data_list <- lapply(log_files, function(file) {
        first_line <- readLines(file, n = 1)
        if (startsWith(first_line, "# GPS")) data <- data.table::fread(file, skip = 2, sep = "\t")
        else data <- data.table::fread(file, sep = ",", header = TRUE)
        data
      })
      alllogs <- data.table::rbindlist(log_data_list, fill = TRUE)
      alllogs$gmt_time <- format(as.POSIXct(alllogs$gmt_time, format = "%H:%M:%S", tz = "GMT"), "%H:%M:%S")
      alllogs$costilt <- cos((alllogs$tilt_deg * pi) / 180)
      alllogs$corralt <- (alllogs$costilt * alllogs$laser_altitude_cm) / 100
      write.csv(alllogs, file.path(evo_directory, "log/ALLLOGS.csv"), row.names = FALSE)
    }
    
    # Step 4: Assign laser log measurements
    incProgress(0.1, detail = "Assigning Laser log measurements...")
    log_m <- dplyr::left_join(y, alllogs, by = c("justtime" = "gmt_time"))
    necessary_cols <- c("gps_alt_m", "laser_altitude_cm", "tilt_deg", "costilt", "corralt", "ImageWidth_px", "SensorWidth_mm")
    for (col in necessary_cols) if (!col %in% names(log_m)) log_m[[col]] <- NA
    log_m$gps_alt_m <- ifelse(!is.na(log_m$corralt), log_m$corralt, log_m$exif_gps_m)
    log_m$exif_gps_m <- NULL
    
    # Step 5: Barometric altitudes
    incProgress(0.1, detail = "Assigning barometric altitudes...")
    if (!is.null(baro_log)) {
      log_m$barometric_alt <- sapply(log_m$dtGMTcorr, find_closest_baro, baro_log = baro_log, timeoff = timeoff_dual)
    } else {
      log_m$barometric_alt <- NA
    }
    log_m$barometric_alt <- apply_barometric_offset(log_m$barometric_alt, baro_offset_m)
    
    # Step 6: Flight numbers
    incProgress(0.1, detail = "Assigning flight numbers...")
    if (length(baro_files) > 0) {
      flight_info <- data.frame(flightnum = integer(), start_time = as.POSIXct(character()), end_time = as.POSIXct(character()))
      for (i in seq_along(baro_files)) {
        evo_log <- data.table::fread(baro_files[i])
        evo_log$datetime_utc      <- as.POSIXct(evo_log$`datetime(utc)`, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        evo_log$datetime_utc_corr <- evo_log$datetime_utc + timeoff_dual
        flight_info <- rbind(flight_info, data.frame(flightnum = i,
                                                     start_time = min(evo_log$datetime_utc_corr, na.rm = TRUE),
                                                     end_time   = max(evo_log$datetime_utc_corr, na.rm = TRUE)))
      }
      flight_info <- flight_info[order(flight_info$start_time), ]; flight_info$flightnum <- 1:nrow(flight_info)
      log_m$flightnum <- sapply(log_m$dtGMTcorr, function(image_time) {
        row <- subset(flight_info, start_time <= image_time & end_time >= image_time)
        if (nrow(row)) row$flightnum[1] else NA
      })
    } else {
      warning_msgs <- c(warning_msgs, "Warning: No baro files found for EVO II Dual Flight numbers set to NA.")
      log_m$flightnum <- NA
    }
    
    # Step 7: Finalize metadata
    incProgress(0.1, detail = "Finalizing data...")
    log_m$platform <- "EVO II Dual"; log_m$photogram_quality <- 0; log_m$photogram_comments <- ""
    log_m$ImageNum <- sub("^(?:MAX_)?(\\d+)\\.JPG$", "\\1", basename(log_m$FileName), ignore.case = TRUE)
    log_m$datetime_utc <- log_m$dtGMTcorr
    
    # Step 8: Save
    incProgress(0.1, detail = "Saving data...")
    imgdata <- dplyr::transmute(log_m,
                                FileName, ImageNum, datetime_utc, justtime, flightnum,
                                platform, pilot, permit, species, whaleinfo,
                                Latitude = GPSLatitude, Longitude = GPSLongitude,
                                gps_alt_m, raw_laser_alt_cm = laser_altitude_cm,
                                tilt_deg, costilt, laser_alt_m = corralt,
                                barometric_alt_m = barometric_alt, FocalLength_mm = FocalLength,
                                ImageWidth_px, SensorWidth_mm, pixel_dimension_mm
    )
    output_file <- file.path(evo_directory, paste0(basename(dirname(evo_directory)), "_EVO_II_Dual_imgdata.csv"))
    write.csv(imgdata, output_file, row.names = FALSE)
    incProgress(0.1, detail = "EVO II Dual processing completed!")
  })
  
  return(warning_msgs)
}
