aph_match_trigger_times <- function(image_seconds, trigger_seconds, tolerance_seconds = 1) {
  trigger_seconds <- suppressWarnings(as.numeric(trigger_seconds))
  image_seconds <- suppressWarnings(as.numeric(image_seconds))

  vapply(image_seconds, function(t) {
    if (is.na(t)) return(NA_integer_)
    diffs <- abs(trigger_seconds - t)
    if (all(is.na(diffs))) return(NA_integer_)
    i <- which.min(diffs)
    if (length(i) && !is.na(diffs[i]) && diffs[i] <= tolerance_seconds) i else NA_integer_
  }, integer(1))
}

aph_time_candidate_score <- function(image_seconds, trigger_seconds, tolerance_seconds = 1) {
  match_idx <- aph_match_trigger_times(image_seconds, trigger_seconds, tolerance_seconds)
  list(match_idx = match_idx, matched = sum(!is.na(match_idx)))
}

choose_aph_exif_time_interpretation <- function(datetime_original, trigger_seconds, tolerance_seconds = 1) {
  camera_time_nyc <- as.POSIXct(datetime_original, format="%Y:%m:%d %H:%M:%S", tz="America/New_York")
  camera_time_utc <- as.POSIXct(datetime_original, format="%Y:%m:%d %H:%M:%S", tz="UTC")

  candidates <- list(
    nyc = list(label = "America/New_York", datetime_utc = with_tz(camera_time_nyc, "UTC")),
    utc = list(label = "UTC", datetime_utc = camera_time_utc)
  )

  for (name in names(candidates)) {
    candidates[[name]]$justtime <- format(candidates[[name]]$datetime_utc, "%H:%M:%S")
    candidates[[name]]$seconds_since_midnight <- as.numeric(hms(candidates[[name]]$justtime))
    score <- aph_time_candidate_score(candidates[[name]]$seconds_since_midnight, trigger_seconds, tolerance_seconds)
    candidates[[name]]$match_idx <- score$match_idx
    candidates[[name]]$matched <- score$matched
  }

  selected <- if (candidates$utc$matched > candidates$nyc$matched) "utc" else "nyc"
  candidates[[selected]]$selected_timezone <- candidates[[selected]]$label
  candidates[[selected]]$nyc_matches <- candidates$nyc$matched
  candidates[[selected]]$utc_matches <- candidates$utc$matched
  candidates[[selected]]
}

# Function to process APH flights
process_aph <- function(aph_directory, species, pilot, permit, flight_date_directory, baro_offset_m = 0) {
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
    )

    time_choice <- choose_aph_exif_time_interpretation(
      image_metadata$DateTimeOriginal,
      combined_trigger_data$seconds_since_midnight
    )
    warning_msgs <- c(
      warning_msgs,
      sprintf(
        "Info: APH EXIF time interpreted as %s (%d NYC matches, %d UTC matches).",
        time_choice$selected_timezone,
        time_choice$nyc_matches,
        time_choice$utc_matches
      )
    )

    image_metadata <- image_metadata %>%
      mutate(
        DateTimeUTC = time_choice$datetime_utc,
        justtime = time_choice$justtime,
        seconds_since_midnight = time_choice$seconds_since_midnight,
        ImageNum             = sub("^P(\\d+)\\.JPG$", "\\1", basename(FileName)),
        ImageWidth_px        = ImageWidth,
        ImageHeight_px       = ImageHeight,
        FocalLength_mm       = FocalLength,
        SensorWidth_mm       = 17.3
      )
    
    
    # 3. Match images → trigger rows -----------------------------------------
    incProgress(0.1, detail = "Matching images to triggers...")
    match_idx <- time_choice$match_idx
    
    # 4. build the final imgdata with exactly the columns you listed -----------
    incProgress(0.1, detail = "Finalizing data...")
    imgdata <- image_metadata %>%
      # pull in the trigger fields by index
      mutate(
        flightnum         = combined_trigger_data$flightnum[match_idx],
        Latitude          = combined_trigger_data$Latitude[match_idx],
        Longitude         = combined_trigger_data$Longitude[match_idx],
        gps_alt_m         = combined_trigger_data$gps_alt_m[match_idx],
        barometric_alt_m  = apply_barometric_offset(combined_trigger_data$barometric_alt_m[match_idx], baro_offset_m),
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
    imgdata <- add_imgdata_qa_warnings(imgdata, "APH-22")
    warning_msgs <- c(warning_msgs, imgdata_qa_status(imgdata, "APH-22"))
    output_file <- file.path(
      aph_directory,
      paste0(basename(flight_date_directory), "_APH-22_imgdata.csv")
    )
    backup_file <- backup_existing_file(output_file)
    if (!is.na(backup_file)) {
      warning_msgs <- c(warning_msgs, paste("Info: Existing APH-22 imgdata backed up to", basename(backup_file)))
    }
    write.csv(imgdata, file = output_file, row.names = FALSE)
    incProgress(0.1, detail = "APH processing completed!")
  })
  
  return(warning_msgs)
}
