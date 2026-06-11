# Function to process Astro flights
process_astro <- function(astro_directory, species, pilot, permit, flight_date_directory, baro_offset_m = 0) {
  warning_msgs <- character()
  
  withProgress(message = "Processing Astro flight data...", value = 0, {
    
    incProgress(0.1, detail = "Locating Astro flight folders...")
    
    # Define the "flights" subfolder inside Astro
    astro_flights_dir <- file.path(astro_directory, "flights")
    
    # Check if the "flights" subfolder actually exists
    if (!dir.exists(astro_flights_dir)) {
      warning_msgs <- c(
        warning_msgs, 
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
      warning_msgs <- c(warning_msgs, paste("Warning: No Astro flight folders found in", astro_directory))
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
    write.csv(astro_all, file = output_file, row.names = FALSE)
    incProgress(0.2, detail = "Astro processing completed!")
  })
  
  return(warning_msgs)
}


