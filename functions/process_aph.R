# Function to process APH flights
process_aph <- function(aph_directory, species, pilot, permit, flight_date_directory) {
  warning_msgs <- character()
  
  withProgress(message = 'Processing APH flight data...', value = 0, {
    # 1. locate & read/clean the trigger files -------------------------------
    incProgress(0.1, detail = "Reading trigger files...")
    date_folder <- file.path(aph_directory, basename(flight_date_directory))
    if (!dir.exists(date_folder)) date_folder <- aph_directory
    gpx_directory <- file.path(date_folder, "GPX")
    if (!dir.exists(gpx_directory)) {
      warning_msgs <- c(warning_msgs, paste("Error: GPX directory missing:", gpx_directory))
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
      warning_msgs <- c(warning_msgs, "No APH images found.")
      return(NULL)
    }
    image_metadata <- exiftoolr::exif_read(
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
      paste0(basename(flight_date_directory), "_APH-22_imgdata.csv")
    )
    write.csv(imgdata, file = output_file, row.names = FALSE)
    incProgress(0.1, detail = "APH processing completed!")
  })
  
  return(warning_msgs)
}