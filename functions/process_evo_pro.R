process_evo_pro <- function(evo_directory, timeoff_pro, species, pilot, permit, flight_date_directory,
                            gps_clock_missing = F, baro_offset_m = 0) {
  warning_msgs <- character()
  
  # ---------- small helpers ----------
  nz_or0 <- function(df) if (nrow(df)) nrow(df) else 0
  ensure_col <- function(df, name, n, val = NA) {
    if (!name %in% names(df)) df[[name]] <- rep(val, n)
    df
  }
  # coerce (if present) or create as NA_real_ of right length
  safe_numeric_from <- function(df, src, out) {
    n <- nrow(df)
    if (src %in% names(df)) {
      df[[out]] <- suppressWarnings(as.numeric(df[[src]]))
      df[[src]] <- NULL
    } else {
      df[[out]] <- rep(NA_real_, n)
    }
    df
  }
  
  # Helper: closest baro reading
  find_closest_baro <- function(image_time, baro_log, timeoff) {
    if (is.null(baro_log) || nrow(baro_log) == 0) return(NA_real_)
    baro_log$datetime_utc <- as.POSIXct(baro_log$`datetime(utc)`, format="%Y-%m-%d %H:%M:%S", tz="GMT")
    baro_log$datetime_utc_corr <- baro_log$datetime_utc + timeoff
    time_diffs <- abs(difftime(image_time, baro_log$datetime_utc_corr, units = "secs"))
    within_window <- which(time_diffs <= 2 & grepl("Picture taken", baro_log$message))
    if (length(within_window) > 0) {
      closest_idx <- within_window[which.min(time_diffs[within_window])]
      return(as.numeric(baro_log$`height_above_takeoff(feet)`[closest_idx]) * 0.3048)
    }
    NA_real_
  }
  
  withProgress(message = "Processing EVO II Pro flight data...", value = 0, {
    # Step 1: EVO baro logs
    incProgress(0.1, detail = "Loading EVO log files...")
    baro_logs  <- file.path(evo_directory, "EVO_logs")
    baro_files <- list.files(baro_logs, pattern = "\\.csv$", full.names = TRUE)
    if (length(baro_files) == 0) {
      warning_msgs <- c(warning_msgs, "Warning: No EVO log files found. Barometric altitude and flight numbers may be NA.")
      baro_log <- NULL
    } else {
      baro_log_list <- lapply(baro_files, data.table::fread)
      baro_log <- data.table::rbindlist(baro_log_list, fill = TRUE)
    }
    # flight windows container (may remain empty)
    flight_info <- data.frame(
      flightnum  = integer(),
      start_time = as.POSIXct(character()),
      end_time   = as.POSIXct(character())
    )
    
    # Step 2: Image EXIF
    incProgress(0.1, detail = "Loading image EXIF metadata...")
    img_dir <- file.path(evo_directory, "jpg")
    if (!dir.exists(img_dir)) {
      warning_msgs <- c(warning_msgs, "Error: No 'jpg' directory in EVO II Pro folder.")
      return(NULL)
    }
    
    y <- exiftoolr::exif_read(
      img_dir,
      tags = c("FileName","SourceFile","DateTimeOriginal","GPSLatitude","GPSLongitude",
               "GPSAltitude#","FocalLength","ImageWidth","ImageHeight"),
      recursive = TRUE
    )
    
    if (!nrow(y)) {
      warning_msgs <- c(warning_msgs, "Warning: No images found or no EXIF data in images.")
      return(NULL)
    }
    
    # Ensure core columns exist even if EXIF omitted them
    nimg <- nrow(y)
    if (!"FileName" %in% names(y)) {
      if ("SourceFile" %in% names(y)) {
        y$FileName <- basename(y$SourceFile)
      } else {
        y$FileName <- sprintf("IMG_%04d.JPG", seq_len(nimg))
      }
    }
    y <- ensure_col(y, "DateTimeOriginal", nimg, NA_character_)
    y <- ensure_col(y, "GPSLatitude",      nimg, NA_real_)
    y <- ensure_col(y, "GPSLongitude",     nimg, NA_real_)
    y <- ensure_col(y, "FocalLength",      nimg, NA_real_)
    y <- ensure_col(y, "ImageWidth",       nimg, NA_real_)
    y <- ensure_col(y, "ImageHeight",      nimg, NA_real_)
    
    # Safe GPS altitude extraction
    y <- safe_numeric_from(y, "GPSAltitude#", "exif_gps_m")
    
    # Times (robust to missing DateTimeOriginal)
    y$dt    <- suppressWarnings(lubridate::ymd_hms(y$DateTimeOriginal, tz = "America/New_York"))
    y$dtGMT <- lubridate::with_tz(y$dt, "GMT")
    y$dtGMTcorr <- y$dtGMT + timeoff_pro
    y$justtime  <- format(y$dtGMTcorr, "%H:%M:%S")
    
    # Static fields
    y$species  <- species
    y$whaleinfo <- NA
    y$pilot    <- pilot
    y$permit   <- permit
    
    # Dimensions/sensor (safe coercions)
    y$ImageWidth_px  <- suppressWarnings(as.numeric(y$ImageWidth))
    y$ImageHeight_px <- suppressWarnings(as.numeric(y$ImageHeight))
    y$SensorWidth_mm <- 13.2
    y$pixel_dimension_mm <- 13.2 / y$ImageWidth_px
    
    # -------------------------------
    # Step 3: Laser-altimeter logs
    # -------------------------------
    incProgress(0.1, detail = "Processing Laser Altimeter log...")
    
    # Tunables
    tol_sec <- 2L      # time tolerance (± seconds)
    alt_min <- 10.0    # meters inclusive
    alt_max <- 60.0    # meters inclusive
    
    log_files <- dir(file.path(evo_directory, "log"), pattern = "\\.CSV$", full.names = TRUE)
    
    if (length(log_files) > 0) {
      # Flexible read for both log formats
      log_data_list <- lapply(log_files, function(file) {
        first_line <- readLines(file, n = 1, warn = FALSE)
        if (!is.na(first_line) && startsWith(first_line, "# GPS")) {
          data.table::fread(file, skip = 2, sep = "\t")
        } else {
          data.table::fread(file, sep = ",", header = TRUE)
        }
      })
      alllogs <- data.table::rbindlist(log_data_list, fill = TRUE)
      
      # ---- time normalization: seconds since midnight (UTC) ----
      if (!"gmt_time" %in% names(alllogs)) alllogs[, gmt_time := NA_character_]
      # parse to seconds; NA-safe
      parse_ok <- !is.na(alllogs$gmt_time)
      alllogs[parse_ok, time_seconds := as.integer(
        as.numeric(strptime(gmt_time, "%H:%M:%S", tz = "GMT")) %% (24*3600)
      )]
      alllogs[!parse_ok, time_seconds := NA_integer_]
      
      # ---- tilt & corrected altitude (meters); camera offset +0.072 m ----
      alllogs[, tilt_deg := suppressWarnings(as.numeric(tilt_deg))]
      alllogs[, laser_altitude_cm := suppressWarnings(as.numeric(laser_altitude_cm))]
      alllogs[, costilt := cos((tilt_deg * pi) / 180)]
      alllogs[, corralt := (costilt * laser_altitude_cm) / 100 + 0.072]
      
      # write raw concat for debugging (kept from your original)
      utils::write.csv(as.data.frame(alllogs), file.path(evo_directory, "log/ALLLOGS.csv"), row.names = FALSE)
      
      # cleaned view for QA file (kept from your original)
      alllogs_cleaned <- as.data.frame(alllogs) |>
        dplyr::rename(converted = costilt, Laser_Alt = corralt) |>
        dplyr::mutate(
          CorrDT = format(
            suppressWarnings(lubridate::mdy(`#gmt_date`) + lubridate::hms(gmt_time)),
            "%Y-%m-%d %H:%M:%S"
          )
        ) |>
        dplyr::select(`#gmt_date`, gmt_time, num_sats, longitude, latitude, gps_altitude_m,
                      SOG_kt, COG, HDOP, laser_altitude_cm, tilt_deg, accel_x, accel_y, accel_z,
                      gyro_x, gyro_y, gyro_z, converted, Laser_Alt, CorrDT)
      
      video_dir <- file.path(evo_directory, "video")
      if (!dir.exists(video_dir)) dir.create(video_dir, recursive = TRUE)
      cleaned_file <- file.path(video_dir, paste0(basename(flight_date_directory), "_video_CleanedLidar.csv"))
      utils::write.csv(alllogs_cleaned, cleaned_file, row.names = FALSE)
      
      # --- Build per-image tolerance windows & aggregate good samples ---
      # Prepare images (already have y$dtGMTcorr)
      img_dt <- data.table::as.data.table(y)[,
                                             .(FileName, dt_img = dtGMTcorr,
                                               day_seconds = as.integer(as.numeric(dtGMTcorr) %% (24*3600)))
      ]
      # intervals [t - tol, t + tol], clamped inside day
      img_dt[, start := pmax(day_seconds - tol_sec, 0L)]
      img_dt[, end   := pmin(day_seconds + tol_sec, 24L*3600L - 1L)]
      
      # logs as point-intervals [t, t]
      logs_pts <- alllogs[!is.na(time_seconds),
                          .(time_seconds, corralt, tilt_deg, costilt, laser_altitude_cm)]
      logs_iv <- logs_pts[, .(start = time_seconds, end = time_seconds,
                              time_seconds, corralt, tilt_deg, costilt, laser_altitude_cm)]
      
      # keys for non-equi join
      data.table::setkey(logs_iv, start, end)
      img_iv <- img_dt[, .(FileName, start, end, day_seconds)]
      data.table::setkey(img_iv, start, end)
      
      # overlap join: logs within each image window
      ov <- data.table::foverlaps(logs_iv, img_iv, nomatch = 0L)
      
      # QC: keep only realistic altitudes (10–40 m) after correction
      ov <- ov[!is.na(corralt) & corralt >= alt_min & corralt <= alt_max]
      
      # aggregate by image using robust median; count samples
      agg <- ov[, .(
        laser_alt_m      = median(corralt, na.rm = TRUE),
        tilt_deg         = median(tilt_deg, na.rm = TRUE),
        costilt          = median(costilt, na.rm = TRUE),
        raw_laser_alt_cm = median(laser_altitude_cm, na.rm = TRUE),
        laser_samples_n  = .N
      ), by = FileName]
      
      # Merge back into y (left join): images with no valid samples remain with NAs
      y <- dplyr::left_join(y, as.data.frame(agg), by = "FileName")
      # helpful QC flag (not exported)
      y$laser_match_ok <- !is.na(y$laser_alt_m) & !is.na(y$tilt_deg)
      
    } else {
      warning_msgs <- c(warning_msgs, "Warning: Skipping laser-altitude matching (no logs).")
      y$laser_alt_m       <- NA_real_
      y$tilt_deg          <- NA_real_
      y$costilt           <- NA_real_
      y$raw_laser_alt_cm  <- NA_real_
      y$laser_samples_n   <- NA_integer_
      y$laser_match_ok    <- FALSE
    }
    
    # --------------------------------
    # Step 4: Prepare joined dataset
    # --------------------------------
    # Keep your GPS EXIF behavior; do not drop images
    log_m <- y
    log_m$gps_alt_m  <- log_m$exif_gps_m
    log_m$exif_gps_m <- NULL
    
    # Step 5: Barometric altitude
    incProgress(0.1, detail = "Assigning barometric altitudes...")
    if (!is.null(baro_log) && nrow(baro_log)) {
      baro_log$datetime_utc <- as.POSIXct(baro_log$`datetime(utc)`, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      if (isTRUE(gps_clock_missing)) {
        # match using y$dtGMTcorr (already a vector aligned to rows in log_m)
        log_m$barometric_alt <- sapply(
          y$dtGMTcorr,
          function(img_time) {
            time_diffs <- abs(difftime(img_time, baro_log$datetime_utc, units = "secs"))
            within_window <- which(time_diffs <= 2 & grepl("Picture taken", baro_log$message))
            if (length(within_window)) {
              idx <- within_window[which.min(time_diffs[within_window])]
              as.numeric(baro_log$`height_above_takeoff(feet)`[idx]) * 0.3048
            } else NA_real_
          }
        )
        warning("Baro–EXIF alignment may drift—timestamps aren’t GPS-sync’d.")
      } else {
        log_m$barometric_alt <- sapply(log_m$dtGMTcorr, find_closest_baro, baro_log = baro_log, timeoff = timeoff_pro)
      }
    } else {
      log_m$barometric_alt <- NA_real_
    }
    log_m$barometric_alt <- apply_barometric_offset(log_m$barometric_alt, baro_offset_m)
    
    # Step 6: Flight numbers
    incProgress(0.1, detail = "Assigning flight numbers...")
    if (length(baro_files) > 0) {
      for (i in seq_along(baro_files)) {
        evo_log <- data.table::fread(baro_files[i])
        evo_log$datetime_utc      <- as.POSIXct(evo_log$`datetime(utc)`, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        evo_log$datetime_utc_corr <- evo_log$datetime_utc + timeoff_pro
        flight_info <- rbind(
          flight_info,
          data.frame(
            flightnum  = i,
            start_time = min(evo_log$datetime_utc_corr, na.rm = TRUE),
            end_time   = max(evo_log$datetime_utc_corr, na.rm = TRUE)
          )
        )
      }
      flight_info <- flight_info[order(flight_info$start_time), ]
      flight_info$flightnum <- seq_len(nrow(flight_info))
      log_m$flightnum <- sapply(log_m$dtGMTcorr, function(image_time) {
        row <- subset(flight_info, start_time <= image_time & end_time >= image_time)
        if (nrow(row)) row$flightnum[1] else NA_integer_
      })
    } else {
      warning_msgs <- c(warning_msgs, "Warning: No baro files; flight numbers set to NA.")
      log_m$flightnum <- NA_integer_
    }
    
    # Step 6.5: Video metadata (optional)
    video_dir   <- file.path(evo_directory, "video")
    video_files <- list.files(video_dir, pattern = "\\.MP4$", full.names = TRUE, ignore.case = TRUE)
    
    if (length(video_files) > 0) {
      vid_meta <- exiftoolr::exif_read(
        video_files,
        tags = c("SourceFile","FileModifyDate","Duration","VideoFrameRate","ImageWidth","ImageHeight")
      ) %>%
        dplyr::rename(FilePath = SourceFile, FileModify = FileModifyDate) %>%
        dplyr::mutate(
          FileName      = basename(FilePath),
          dt_raw        = suppressWarnings(lubridate::ymd_hms(FileModify, tz = "GMT")),
          datetime_utc  = dt_raw + timeoff_pro,
          endtime       = format(datetime_utc, "%H:%M:%S"),
          duration      = suppressWarnings(as.numeric(Duration)),
          starttime     = format(datetime_utc - lubridate::seconds(duration), "%H:%M:%S"),
          fps           = suppressWarnings(as.numeric(VideoFrameRate)),
          VideoNum      = dplyr::row_number(),
          flightnum     = sapply(datetime_utc, function(dt) {
            row <- subset(flight_info, start_time <= dt & end_time >= dt)
            if (nrow(row)) row$flightnum[1] else NA_integer_
          }),
          platform      = "EVO II Pro",
          pilot         = pilot,
          permit        = permit,
          species       = species,
          whaleinfo     = NA,
          FocalLength_mm = 10.6,
          ImageWidth_px  = suppressWarnings(as.numeric(ImageWidth)),
          ImageHeight_px = suppressWarnings(as.numeric(ImageHeight))
        ) %>%
        dplyr::mutate(
          pixel_dimension_mm = dplyr::case_when(
            ImageWidth_px == 3840 & ImageHeight_px == 2160 & fps < 31  ~ 0.003507,
            ImageWidth_px == 3840 & ImageHeight_px == 2160 & fps > 30  ~ 0.002795,
            ImageWidth_px == 2720 & ImageHeight_px == 1528 & fps < 31  ~ 0.00495,
            ImageWidth_px == 2720 & ImageHeight_px == 1528 & fps > 30  ~ 0.00395,
            ImageWidth_px == 1920 & ImageHeight_px == 1080 & fps == 120 ~ 0.006975,
            ImageWidth_px == 1920 & ImageHeight_px == 1080 & fps > 30  ~ 0.005593,
            ImageWidth_px == 1920 & ImageHeight_px == 1080 & fps < 31  ~ 0.0070,
            TRUE ~ 13.2 / pmax(1, ImageWidth_px)
          ),
          SensorWidth_mm = pixel_dimension_mm * ImageWidth_px
        ) %>%
        dplyr::select(
          FileName, VideoNum, datetime_utc,
          starttime, endtime, duration, fps, flightnum,
          platform, pilot, permit, species, whaleinfo,
          FocalLength_mm, ImageWidth_px, ImageHeight_px, SensorWidth_mm, pixel_dimension_mm
        )
      
      # write metadata
      meta_out <- file.path(video_dir, paste0(basename(flight_date_directory), "_video_metadata.csv"))
      utils::write.csv(vid_meta, meta_out, row.names = FALSE)
      
      # Build Video GPS Time table (guarded)
      if (nrow(vid_meta)) {
        video_gps <- vid_meta %>%
          dplyr::filter(!is.na(flightnum)) %>%
          dplyr::group_by(flightnum) %>%
          dplyr::slice_min(order_by = datetime_utc, n = 1) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            Image     = paste0(tools::file_path_sans_ext(FileName), "_00_00_00.png"),
            VideoTime = "00:00:00",
            GPS_Time  = starttime,
            GPS_Date  = format(as.Date(datetime_utc), "%y%m%d")
          ) %>%
          dplyr::select(Image, VideoTime, GPS_Time, GPS_Date)
        
        gps_out <- file.path(video_dir, paste0(basename(flight_date_directory), "_Video_GPS_Time.csv"))
        utils::write.csv(video_gps, gps_out, row.names = FALSE)
      }
      
      # Rename videos to append _f<flightnum>
      for (i in seq_len(nrow(vid_meta))) {
        old_name  <- vid_meta$FileName[i]
        fnum      <- vid_meta$flightnum[i]
        if (is.na(fnum)) next
        base_name <- tools::file_path_sans_ext(old_name)
        ext       <- tools::file_ext(old_name)
        if (grepl("_f\\d+$", base_name)) next
        old_path <- file.path(video_dir, old_name)
        if (!file.exists(old_path)) next
        new_name <- paste0(base_name, "_f", fnum, ".", ext)
        new_path <- file.path(video_dir, new_name)
        if (!file.exists(new_path)) file.rename(old_path, new_path)
      }
      
      # Dynamic pixel_dim / SensorWidth only if we have vid_meta
      if (exists("vid_meta") && nrow(vid_meta) > 0) {
        vid_meta <- vid_meta %>%
          dplyr::mutate(
            dt_end   = datetime_utc,
            dt_start = datetime_utc - lubridate::seconds(duration)
          )
        video_windows <- vid_meta %>%
          dplyr::select(flightnum, dt_start, dt_end, fps, ImageWidth_px, ImageHeight_px)
        
        log_m <- log_m %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            this_win = list(dplyr::filter(video_windows,
                                          flightnum == flightnum,
                                          dt_start <= dtGMTcorr,
                                          dt_end   >= dtGMTcorr)),
            is_video_still = (length(this_win$fps) > 0),
            this_fps       = ifelse(is_video_still, this_win$fps[1], NA_real_),
            pixel_dimension_mm = dplyr::case_when(
              ImageWidth_px == 5472 & ImageHeight_px == 3076 ~ 0.002458,
              ImageWidth_px == 5472 & ImageHeight_px == 3648 ~ 0.002489,
              ImageWidth_px == 3840 & ImageHeight_px == 2160 & !is_video_still ~ 0.002412,
              ImageWidth_px == 3840 & ImageHeight_px == 2160 & is_video_still & this_fps > 30  ~ 0.002795,
              ImageWidth_px == 3840 & ImageHeight_px == 2160 & is_video_still & this_fps < 31  ~ 0.003507,
              ImageWidth_px == 2720 & ImageHeight_px == 1528 & is_video_still & this_fps > 30  ~ 0.00395,
              ImageWidth_px == 2720 & ImageHeight_px == 1528 & is_video_still & this_fps < 31  ~ 0.00495,
              ImageWidth_px == 1920 & ImageHeight_px == 1080 & is_video_still & this_fps == 120 ~ 0.006975,
              ImageWidth_px == 1920 & ImageHeight_px == 1080 & is_video_still & this_fps > 30  ~ 0.005593,
              ImageWidth_px == 1920 & ImageHeight_px == 1080 & is_video_still & this_fps < 31  ~ 0.0070,
              TRUE ~ pixel_dimension_mm  # keep what was computed earlier
            ),
            SensorWidth_mm = pixel_dimension_mm * ImageWidth_px
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-this_win, -is_video_still, -this_fps)
      }
    } # end video section
    
    # Step 7: Finalize
    incProgress(0.1, detail = "Finalizing data...")
    log_m$platform <- "EVO II Pro"
    log_m$photogram_quality  <- 0
    log_m$photogram_comments <- ""
    log_m$ImageNum <- sub("^(?:MAX_)?(\\d+)\\.JPG$", "\\1", basename(log_m$FileName), ignore.case = TRUE)
    log_m$datetime_utc <- log_m$dtGMTcorr
    
    # Step 8: Save
    incProgress(0.1, detail = "Saving data...")
    # ensure columns used in transmute exist
    for (nm in c("GPSLatitude","GPSLongitude","FocalLength","ImageWidth_px","ImageHeight_px","SensorWidth_mm","pixel_dimension_mm"))
      if (!nm %in% names(log_m)) log_m[[nm]] <- NA_real_
    
    imgdata <- dplyr::transmute(
      log_m,
      FileName, ImageNum, datetime_utc, justtime, flightnum,
      platform, pilot, permit, species, whaleinfo,
      Latitude = GPSLatitude, Longitude = GPSLongitude, gps_alt_m,
      raw_laser_alt_cm,
      tilt_deg, costilt, laser_alt_m,
      barometric_alt_m = barometric_alt, FocalLength_mm = FocalLength,
      ImageWidth_px, ImageHeight_px, SensorWidth_mm, pixel_dimension_mm
    )
    
    output_file <- file.path(evo_directory, paste0(basename(dirname(evo_directory)), "_EVO_II_Pro_imgdata.csv"))
    backup_file <- backup_existing_file(output_file)
    if (!is.na(backup_file)) {
      warning_msgs <- c(warning_msgs, paste("Info: Existing EVO II Pro imgdata backed up to", basename(backup_file)))
    }
    utils::write.csv(imgdata, output_file, row.names = FALSE)
    incProgress(0.1, detail = "EVO II Pro processing completed!")
  })
  
  return(warning_msgs)
}
