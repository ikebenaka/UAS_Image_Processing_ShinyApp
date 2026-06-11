# Main function to process flight data
process_flight_data <- function(flight_date_directory, timeoff_pro, timeoff_dual, permit, species, pilot, status_message) {
  
  # Initialize a vector to store warning messages
  warnings <- character()
  
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
  
  # Common function to process EVO drones (Pro and Dual)
  process_evo_generic <- function(evo_directory, drone_type, timeoff) {
    # Use Shiny's progress bar
    withProgress(message = paste("Processing", drone_type, "flight data..."), value = 0, {
      
      # Step 1: Load EVO log files for barometric altitude values
      incProgress(0.1, detail = "Loading EVO log files...")
      baro_logs <- file.path(evo_directory, "EVO_logs")
      baro_files <- list.files(baro_logs, pattern = "\\.csv$", full.names = TRUE)
      if (length(baro_files) == 0) {
        warnings <<- c(warnings, paste("Warning: No EVO log files found for", drone_type, ". Barometric altitude will be set to NA."))
        baro_log <- NULL
      } else {
        baro_log_list <- lapply(baro_files, fread)
        baro_log <- rbindlist(baro_log_list, fill = TRUE)
      }
      
      # Step 2: Loading image EXIF metadata
      incProgress(0.1, detail = "Loading image EXIF metadata...")
      img_dir <- file.path(evo_directory, "jpg")
      if (!dir.exists(img_dir)) {
        warnings <<- c(warnings, paste("Error: No image directory found in", drone_type, "folder."))
        return(NULL)
      }
      
      # Read EXIF data including 'ImageWidth' and 'FocalLength'
      y <- exif_read(img_dir, tags = c("FileName", "DateTimeOriginal", "GPSLatitude", "GPSLongitude", "GPSAltitude#", "FocalLength", "ImageWidth"), recursive = TRUE)
      
      if ("GPSAltitude#" %in% names(y)) {
        y$gps_alt_m    <- as.numeric(y[["GPSAltitude#"]])
        y[["GPSAltitude#"]] <- NULL
      } else {
        y$exif_gps_m <- rep(NA_real_, nrow(y))
      }
      
      
      if (nrow(y) == 0) {
        warnings <<- c(warnings, paste("Warning: No images found or no EXIF data in images for", drone_type))
        return(NULL)
      }
      
      y$dt <- ymd_hms(y$DateTimeOriginal, tz = "America/New_York")
      y$dtGMT <- with_tz(y$dt, "GMT")
      y$dtGMTcorr <- y$dtGMT + timeoff
      y$justtime <- format(y$dtGMTcorr, format = "%H:%M:%S")
      
      # Add user-input metadata fields
      y$species <- species
      y$whaleinfo <- NA
      y$pilot <- pilot
      y$permit <- permit
      
      # Assign ImageWidth in pixels and SensorWidth
      y$ImageWidth_px <- y$ImageWidth
      
      if (drone_type == "EVO II Pro") {
        y$SensorWidth_mm <- 13.2
      } else if (drone_type == "EVO II Dual") {
        y$SensorWidth_mm <- 6.4
      } else {
        warnings <<- c(warnings, paste("Error: Unknown drone type", drone_type))
        return(NULL)
      }
      
      # Step 2.5: Set 'pixel_dimension_mm' based on 'ImageWidth' and drone type
      if (drone_type == "EVO II Pro") {
        # EVO II Pro
        y$pixel_dimension_mm <- 13.2 / y$ImageWidth
      } 
      else if (drone_type == "EVO II Dual") {
        # EVO II Dual - Calculate based on sensor size and image width
        y$pixel_dimension_mm <- 6.4 / y$ImageWidth
      }
      
      # Warn if there are images with unexpected 'ImageWidth' values
      valid_widths <- if (drone_type == "EVO II Pro") c(5472, 3840) else c(8000, 4000)
      unknown_widths <- y$ImageWidth[!y$ImageWidth %in% valid_widths]
      if (any(!is.na(unknown_widths))) {
        warnings <<- c(warnings, paste("Warning: Found images with unexpected ImageWidth(s) in", drone_type, ":", paste(unique(unknown_widths), collapse = ", ")))
      }
      
      # Step 3: Processing Laser Altimeter log
      incProgress(0.1, detail = "Processing Laser Altimeter log...")
      log_files <- dir(file.path(evo_directory, "log"), pattern = "\\.CSV$", full.names = TRUE)
      if (length(log_files) == 0) {
        warnings <<- c(warnings, paste("Warning: No Laser Altimeter log files found for", drone_type, ". Altimeter data will be set to NA."))
        alllogs <- data.frame(
          gmt_time = y$justtime,
          laser_altitude_cm = NA,
          tilt_deg = NA,
          costilt = NA,
          corralt = NA,
          stringsAsFactors = FALSE
        )
      } else {
        log_data_list <- lapply(log_files, function(file) {
          # Read the first line to determine the format
          first_line <- readLines(file, n = 1)
          if (startsWith(first_line, "# GPS")) {
            # Standard format: skip first two lines
            data <- fread(file, skip = 2, sep = "\t")
          } else {
            # Modified format: read without skipping lines
            data <- fread(file, sep = ",", header = TRUE)
          }
          return(data)
        })
        
        alllogs <- rbindlist(log_data_list, fill = TRUE)
        alllogs$gmt_time <- format(as.POSIXct(alllogs$gmt_time, format = "%H:%M:%S", tz = "GMT"), format = "%H:%M:%S")
        alllogs$costilt <- cos((alllogs$tilt_deg * pi) / 180)
        alllogs$corralt <- (alllogs$costilt * alllogs$laser_altitude_cm) / 100
        
        write.csv(alllogs, file.path(evo_directory, "log/ALLLOGS.csv"), row.names = FALSE)
      }
      
      # Step 4: Assigning Laser log measurements to images
      incProgress(0.1, detail = "Assigning Laser log measurements to images...")
      log_m <- left_join(y, alllogs, by = c("justtime" = "gmt_time"))
      
      # Ensure all necessary columns are present
      necessary_cols <- c("gps_alt_m", "laser_altitude_cm", "tilt_deg", "costilt", "corralt", "ImageWidth_px", "SensorWidth_mm")
      for (col in necessary_cols) {
        if (!col %in% names(log_m)) {
          log_m[[col]] <- NA
        }
      }
      
      # fill any missing log‐cols
      for(col in necessary_cols) if (!col %in% names(log_m)) log_m[[col]] <- NA
      
      # prefer the laser‐corrected altitude:
      log_m$gps_alt_m <- ifelse(
        !is.na(log_m$corralt),
        log_m$corralt,
        log_m$exif_gps_m
      )
      
      log_m$exif_gps_m <- NULL
      
      # Step 5: Assigning barometric altitudes
      incProgress(0.1, detail = "Assigning barometric altitudes to images...")
      if (!is.null(baro_log)) {
        image_times <- log_m$dtGMTcorr
        log_m$barometric_alt <- sapply(image_times, find_closest_baro, baro_log = baro_log, timeoff = timeoff)
      } else {
        log_m$barometric_alt <- NA
      }
      
      # Step 6: Assign flight numbers
      incProgress(0.1, detail = "Assigning flight numbers to images...")
      if (length(baro_files) > 0) {
        flight_info <- data.frame(flightnum = integer(), start_time = as.POSIXct(character()), end_time = as.POSIXct(character()), stringsAsFactors = FALSE)
        
        for (i in seq_along(baro_files)) {
          evo_log <- fread(baro_files[i])
          evo_log$datetime_utc <- as.POSIXct(evo_log$'datetime(utc)', format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
          evo_log$datetime_utc_corr <- evo_log$datetime_utc + timeoff
          start_time <- min(evo_log$datetime_utc_corr, na.rm = TRUE)
          end_time <- max(evo_log$datetime_utc_corr, na.rm = TRUE)
          flight_info <- rbind(flight_info, data.frame(flightnum = i, start_time = start_time, end_time = end_time))
        }
        
        flight_info <- flight_info[order(flight_info$start_time), ]
        flight_info$flightnum <- 1:nrow(flight_info)
        
        # Assign flight numbers to images
        log_m$flightnum <- sapply(log_m$dtGMTcorr, function(image_time) {
          matching_flight <- flight_info[flight_info$start_time <= image_time & flight_info$end_time >= image_time, ]
          if (nrow(matching_flight) > 0) {
            return(matching_flight$flightnum[1])
          } else {
            later_flight <- flight_info[flight_info$start_time > image_time, ]
            if (nrow(later_flight) > 0) {
              return(later_flight$flightnum[1])
            } else {
              return(NA)
            }
          }
        })
        
      } else {
        warnings <<- c(warnings, paste("Warning: No baro files found for", drone_type, ". Flight numbers will be set to NA."))
        log_m$flightnum <- NA
      }
      
      # Step 7: Assigning platform and other metadata
      incProgress(0.1, detail = "Finalizing data...")
      log_m$platform <- drone_type
      log_m$photogram_quality <- 0
      log_m$ImageNum <- sub("^(?:MAX_)?(\\d+)\\.JPG$", "\\1", basename(log_m$FileName), ignore.case = TRUE)
      log_m$datetime_utc <- log_m$dtGMTcorr
      log_m$photogram_comments <- ""
      
      # Ensure all necessary columns are present before selecting
      all_cols <- c("FileName", "ImageNum", "datetime_utc", "justtime", "flightnum", "platform", "pilot", "permit", "species", "whaleinfo",
                    "GPSLatitude", "GPSLongitude", "gps_alt_m", "laser_altitude_cm", "tilt_deg", "costilt", "corralt",
                    "barometric_alt", "FocalLength", "ImageWidth_px", "SensorWidth_mm", "pixel_dimension_mm", "photogram_quality", "photogram_comments")
      for (col in all_cols) {
        if (!col %in% names(log_m)) {
          log_m[[col]] <- NA
        }
      }
      
      # Step 8: Save the cleaned CSV
      incProgress(0.1, detail = "Saving data...")
      
      # compute new columns
      log_m$barometric_alt_m  <- log_m$barometric_alt
      log_m$raw_laser_alt_cm  <- log_m$laser_altitude_cm
      log_m$laser_alt_m       <- log_m$corralt
      log_m$FocalLength_mm    <- log_m$FocalLength
      
      # build final dataframe in the exact desired order
      imgdata <- log_m %>% transmute(
        FileName,
        ImageNum,
        datetime_utc   = dtGMTcorr,
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
        SensorWidth_mm,
        pixel_dimension_mm
      )
      
      output_file <- file.path(evo_directory, paste0(basename(flight_date_directory), "_", gsub(" ", "_", drone_type), "_imgdata.csv"))
      
      write.csv(imgdata, output_file, row.names = FALSE)
      
      incProgress(0.1, detail = paste(drone_type, "processing completed!"))
    })
  }
  
  # Function to process APH flights
process_aph <- function(aph_directory) {
  withProgress(message = 'Processing APH flight data...', value = 0, {
    # 1. locate & read/clean the trigger files -------------------------------
    incProgress(0.1, detail = "Reading trigger files...")
    date_folder <- file.path(aph_directory, basename(flight_date_directory))
    if (!dir.exists(date_folder)) date_folder <- aph_directory
    gpx_directory <- file.path(date_folder, "GPX")
    if (!dir.exists(gpx_directory)) {
      warnings <<- c(warnings, paste("Error: GPX directory missing:", gpx_directory))
      return(NULL)
    }
    trigger_files <- list.files(gpx_directory, "\\.txt$", full.names=TRUE, ignore.case=TRUE)
    combined_trigger_data <- rbindlist(
      lapply(seq_along(trigger_files), function(i) {
        lines <- readLines(trigger_files[i])
        hdr   <- sub("^#", "", lines[1]) %>% strsplit(";", fixed=TRUE) %>% unlist()
        body  <- lines[!grepl("^#", lines)]
        dt    <- fread(text=body, sep=";", header=FALSE, col.names=hdr)
        dt$flightnum <- i
        dt
      }),
      fill = TRUE
    )
    # rename only the three altitude columns
    if ("GPSAltitude[m](raw)" %in% names(combined_trigger_data))
      setnames(combined_trigger_data, "GPSAltitude[m](raw)", "gps_alt_m")
    if ("BaroAltitude[m]"       %in% names(combined_trigger_data))
      setnames(combined_trigger_data, "BaroAltitude[m]",       "barometric_alt_m")
    if ("Laser[m]"              %in% names(combined_trigger_data))
      setnames(combined_trigger_data, "Laser[m]",              "laser_alt_raw_m")
    
    # compute seconds_since_midnight
    combined_trigger_data <- combined_trigger_data %>%
      mutate(
        Time                   = as.POSIXct(Time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
        time_only              = format(Time, "%H:%M:%S"),
        seconds_since_midnight = as.numeric(hms(time_only))
      )
    # save for debugging
    write.csv(combined_trigger_data,
              file.path(gpx_directory, "cleaned_trigger_data.csv"),
              row.names=FALSE)
  
    # 2. read EXIF from the JPGs ------------------------------------------------
    incProgress(0.1, detail = "Reading image EXIF...")
    image_directory <- list.files(aph_directory, "^(jpg|JPG|flight)", full.names=TRUE)
    image_files     <- list.files(image_directory, "\\.JPG$", full.names=TRUE)
    if (length(image_files) == 0) {
      warnings <<- c(warnings, "No APH images found.")
      return(NULL)
    }
    image_metadata <- exif_read(
      image_files,
      tags = c("FileName","DateTimeOriginal","FocalLength","ImageWidth","ImageHeight"),
      recursive = FALSE
    ) %>%
      mutate(
        DateTimeOriginal_EST     = as.POSIXct(DateTimeOriginal, format="%Y:%m:%d %H:%M:%S", tz="America/New_York"),
        DateTimeUTC = with_tz(DateTimeOriginal_EST, "UTC"),
        justtime            = format(DateTimeUTC, "%H:%M:%S"),
        seconds_since_midnight = as.numeric(hms(justtime)),
        ImageNum             = sub("^P(\\d+)\\.JPG$", "\\1", basename(FileName)),
        ImageWidth_px        = ImageWidth,
        ImageHeight_px       = ImageHeight,
        FocalLength_mm       = FocalLength,
        SensorWidth_mm       = 17.3
      )

  
    # 3. Match images → trigger rows -----------------------------------------
    incProgress(0.1, detail = "Matching images to triggers...")
    match_idx <- sapply(image_metadata$seconds_since_midnight, function(t) {
      if (is.na(t)) return(NA_integer_)
      diffs <- abs(combined_trigger_data$seconds_since_midnight - t)
      i     <- which.min(diffs)
      if (length(i) && diffs[i] <= 1) i else NA_integer_
    })
  
    # 4. build the final imgdata with exactly the columns you listed -----------
    incProgress(0.1, detail = "Finalizing data...")
    imgdata <- image_metadata %>%
      # pull in the trigger fields by index
      mutate(
        flightnum         = combined_trigger_data$flightnum[match_idx],
        Latitude          = combined_trigger_data$Latitude[match_idx],
        Longitude         = combined_trigger_data$Longitude[match_idx],
        gps_alt_m         = combined_trigger_data$gps_alt_m[match_idx],
        barometric_alt_m  = combined_trigger_data$barometric_alt_m[match_idx],
        laser_alt_m       = if ("laser_alt_raw_m" %in% names(combined_trigger_data))
                              combined_trigger_data$laser_alt_raw_m[match_idx]
                            else
                              NA_real_,
        raw_laser_alt_cm  = ifelse(!is.na(laser_alt_m), laser_alt_m * 100, NA_real_),
        platform          = "APH-22",
        pilot             = pilot,
        permit            = permit,
        species           = species,
        whaleinfo         = NA_character_,
        pixel_dimension_mm = 0.00375,
        tilt_deg          = NA_real_,
        costilt           = NA_real_
      ) %>%
      transmute(
        FileName,
        ImageNum,
        datetime_utc    = DateTimeUTC,
        justtime,
        flightnum,
        platform,
        pilot,
        permit,
        species,
        whaleinfo,
        Latitude,
        Longitude,
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
  
    # 5. save ------------------------------------------------------------------
    output_file <- file.path(
      aph_directory,
      paste0(basename(flight_date_directory), "_APH_imgdata.csv")
    )
    write.csv(imgdata, file = output_file, row.names = FALSE)
    incProgress(0.1, detail = "APH processing completed!")
  })
}


  
  # Function to process Astro flights
  process_astro <- function(astro_directory) {
    withProgress(message = "Processing Astro flight data...", value = 0, {
      
      incProgress(0.1, detail = "Locating Astro flight folders...")
      
      # Define the "flights" subfolder inside Astro
      astro_flights_dir <- file.path(astro_directory, "flights")
      
      # Check if the "flights" subfolder actually exists
      if (!dir.exists(astro_flights_dir)) {
        warnings <<- c(
          warnings, 
          paste("Warning:", astro_flights_dir, "does not exist. No Astro flight data processed.")
        )
        return(NULL)
      }
      
      # Now get the flight folders from the "flights" subfolder
      flight_folders <- list.dirs(astro_flights_dir, full.names = TRUE, recursive = FALSE)
      
      # Filter only those that match the pattern: XX_YYYY-MM-DD_HH-MM-SS
      flight_folders <- flight_folders[grepl("^\\d+_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}$", 
                                             basename(flight_folders))]
      
      if (length(flight_folders) == 0) {
        warnings <<- c(warnings, paste("Warning: No Astro flight folders found in", astro_directory))
        return(NULL)
      }
      
      # Parse the 'XX' from folder names, so we can rename them flight 1, flight 2, etc. for the day
      flight_info <- data.frame(
        folder = flight_folders,
        numeric_id = as.integer(sub("_.*", "", basename(flight_folders))),  # everything before first underscore
        stringsAsFactors = FALSE
      )
      # Sort by 'numeric_id' ascending
      flight_info <- flight_info[order(flight_info$numeric_id), ]
      flight_info$flightnum_day <- seq_len(nrow(flight_info))
      
      # Collect all images from all flight folders in a single data frame
      astro_all <- data.frame()
      
      incProgress(0.2, detail = "Reading EXIF for each flight folder...")
      for (i in seq_len(nrow(flight_info))) {
        fnum <- flight_info$flightnum_day[i]
        fpath <- flight_info$folder[i]
        
        # List .jpg images in this flight folder
        image_files <- list.files(fpath, pattern = "\\.jpe?g$", ignore.case = TRUE, full.names = TRUE)
        if (length(image_files) == 0) {
          warnings <<- c(warnings, paste("Warning: No images found in Astro flight folder", fpath))
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
          warnings <<- c(warnings, paste("Warning: EXIF read returned no data for folder", fpath))
          next
        }
        
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
        
        # pixel_dimension_mm = sensor_width / image_width
        exif_data$pixel_dimension_mm <- with(exif_data, SensorWidth_mm / ImageWidth)
        
        # For consistency with other drones:
        # rename "GPSAltitude#" to "gps_altitude_m" (if present)
        if ("GPSAltitude#" %in% names(exif_data)) {
          exif_data$gps_altitude_m <- exif_data$GPSAltitude#
        } else {
          exif_data$gps_altitude_m <- NA
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
          barometric_alt_m = barometric_alt,
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
      write.csv(astro_all, file = output_file, row.names = FALSE)
      incProgress(0.2, detail = "Astro processing completed!")
    })
  }
  
  # Get all directories in the flight date directory
  all_directories <- list.dirs(flight_date_directory, full.names = TRUE, recursive = FALSE)
  cat("Flight date directory:\n")
  cat(flight_date_directory, "\n")
  cat("Directories found:\n")
  cat(all_directories, sep = "\n")
  cat("Basenames of directories:\n")
  cat(basename(all_directories), sep = "\n")
  
  # Process EVO II Pro directories
  evo_pro_directories <- all_directories[grep("EVO II Pro", basename(all_directories), ignore.case = TRUE)]
  
  if (length(evo_pro_directories) > 0) {
    for (evo_directory in evo_pro_directories) {
      print(paste("Processing EVO II Pro directory:", evo_directory))
      process_evo_generic(evo_directory, "EVO II Pro", timeoff_pro)
    }
  } else {
    print("No EVO II Pro directories found to process.")
  }
  
  # Process EVO II Dual directories
  evo_dual_directories <- all_directories[grep("EVO II Dual", basename(all_directories), ignore.case = TRUE)]
  
  if (length(evo_dual_directories) > 0) {
    for (evo_directory in evo_dual_directories) {
      print(paste("Processing EVO II Dual directory:", evo_directory))
      process_evo_generic(evo_directory, "EVO II Dual", timeoff_dual)
    }
  } else {
    print("No EVO II Dual directories found to process.")
  }
  
  # Process APH directories
  aph_directories <- all_directories[grep("APH", basename(all_directories), ignore.case = TRUE)]
  
  if (length(aph_directories) > 0) {
    for (aph_directory in aph_directories) {
      print(paste("Processing APH directory:", aph_directory))
      process_aph(aph_directory)
    }
  } else {
    print("No APH directories found to process.")
  }
  
  # Process Astro directory
  astro_directories <- all_directories[grep("Astro$", basename(all_directories), ignore.case = TRUE)]
  
  if (length(astro_directories) > 0) {
    for (astro_dir in astro_directories) {
      cat("Processing Astro directory:", astro_dir, "\n")
      process_astro(astro_dir)
    }
  } else {
    cat("No Astro directories found to process.\n")
  }
  
  # Completion and warnings
  if (length(warnings) > 0) {
    status_message(paste("Processing completed with warnings:\n", paste(warnings, collapse = "\n")))
  } else {
    status_message("Processing completed without warnings!")
  }
}