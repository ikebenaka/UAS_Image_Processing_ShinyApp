# ─────────────────────────  HELPER FUNCTIONS  ────────────────────────────
.exif_time_to_utc <- function(df) {
  n <- nrow(df); if (!n) return(as.POSIXct(character(), tz = "UTC"))
  out <- rep(as.POSIXct(NA, tz = "UTC"), n)
  
  # 1) DateTimeOriginal + OffsetTimeOriginal
  idx <- which(!is.na(df$DateTimeOriginal) & !is.na(df$OffsetTimeOriginal))
  if (length(idx)) {
    dto <- sub("^([0-9]{4}):([0-9]{2}):([0-9]{2})", "\\1-\\2-\\3",
               df$DateTimeOriginal[idx])
    out[idx] <- lubridate::ymd_hms(paste0(dto, df$OffsetTimeOriginal[idx]), tz = "UTC")
  }
  # 2) GPSDateStamp + GPSTimeStamp (already UTC)
  idx <- which(!is.na(df$GPSDateStamp) & !is.na(df$GPSTimeStamp))
  if (length(idx)) out[idx] <- lubridate::ymd_hms(
    paste(df$GPSDateStamp[idx], df$GPSTimeStamp[idx]), tz = "UTC")
  # 3) DateTimeOriginal alone  → assume UTC
  idx <- which(is.na(out) & !is.na(df$DateTimeOriginal))
  if (length(idx)) {
    dto <- sub("^([0-9]{4}):([0-9]{2}):([0-9]{2})", "\\1-\\2-\\3",
               df$DateTimeOriginal[idx])
    out[idx] <- lubridate::ymd_hms(dto, tz = "UTC")
  }
  out
}

.compute_offset_us <- function(cap_dt) {
  v <- cap_dt[!is.na(timestamp_utc) & timestamp_utc > 0 & !is.na(timestamp)]
  if (!nrow(v)) return(NA_real_)
  stats::median(v$timestamp_utc - v$timestamp, na.rm = TRUE)
}

.avg_lidar_one_second <- function(ds_dt, t,
                                  tol5 = 0.8, tol3 = 0.4) {          # metres
  if (!nrow(ds_dt) || is.na(t)) return(list(value = NA_real_, ok = FALSE))
  t0  <- lubridate::floor_date(t, "second")
  bin <- ds_dt[utc >= t0 & utc < (t0 + lubridate::seconds(1)), current_distance]
  w5  <- ds_dt[utc >= (t - lubridate::seconds(2.5)) &
                 utc <= (t + lubridate::seconds(2.5)), current_distance]
  w3  <- ds_dt[utc >= (t - lubridate::seconds(1.5)) &
                 utc <= (t + lubridate::seconds(1.5)), current_distance]
  
  if (length(w5) >= 2 && (max(w5) - min(w5)) > tol5) return(list(value = NA, ok = FALSE))
  if (length(w3) >= 2 && (max(w3) - min(w3)) > tol3) return(list(value = NA, ok = FALSE))
  if (!length(bin)) return(list(value = NA, ok = FALSE))
  list(value = mean(bin), ok = TRUE)
}

.derive_sensor_width <- function(model, img_w, fp_x_res, res_unit) {
  if (!is.na(model) && grepl("ILCE-7RM4", model, ignore.case = TRUE)) return(35.7)
  if (!is.na(fp_x_res) && !is.na(res_unit) && !is.na(img_w)) {
    unit <- tolower(as.character(res_unit))
    px_per_mm <- switch(unit,
                        "inch" = as.numeric(fp_x_res) / 25.4,
                        "inches" = as.numeric(fp_x_res) / 25.4,
                        "2" = as.numeric(fp_x_res) / 25.4,
                        "cm" = as.numeric(fp_x_res) / 10,
                        "centimeter" = as.numeric(fp_x_res) / 10,
                        "3" = as.numeric(fp_x_res) / 10,
                        NA)
    if (is.finite(px_per_mm) && px_per_mm > 0) return(img_w / px_per_mm)
  }
  NA_real_
}

.read_ulog_csvs <- function(log_dir) {
  cap_files <- list.files(log_dir, pattern = "camera_capture_\\d+\\.csv$",
                          full.names = TRUE)
  if (!length(cap_files)) return(list())
  
  cap <- data.table::rbindlist(lapply(cap_files, data.table::fread), fill = TRUE)
  cap[, `:=`(timestamp      = as.numeric(timestamp),
             timestamp_utc  = as.numeric(timestamp_utc))]
  cap[, utc := lubridate::as_datetime(timestamp_utc/1e6, tz = "UTC")]
  data.table::setkey(cap, utc)
  offset_us <- .compute_offset_us(cap)
  
  ds_files <- list.files(log_dir, pattern = "distance_sensor_\\d+\\.csv$", full.names = TRUE)
  ds <- if (length(ds_files)) {
    tmp <- data.table::rbindlist(lapply(ds_files, data.table::fread), fill = TRUE)
    tmp[, timestamp := as.numeric(timestamp)]
    tmp[, utc := lubridate::as_datetime((timestamp + offset_us)/1e6, tz = "UTC")]
    #if (all(c("min_distance","max_distance") %in% names(tmp))) tmp <- tmp[current_distance >= min_distance & current_distance <= max_distance]
    #if ("signal_quality" %in% names(tmp)) tmp <- tmp[signal_quality != 0]
    data.table::setkey(tmp, utc); tmp
  } else NULL
  
  air_files <- list.files(log_dir, pattern = "vehicle_air_data_\\d+\\.csv$", full.names = TRUE)
  air <- if (length(air_files)) {
    tmp <- data.table::rbindlist(lapply(air_files, data.table::fread), fill = TRUE)
    tmp[, timestamp := as.numeric(timestamp)]
    tmp[, utc := lubridate::as_datetime((timestamp + offset_us)/1e6, tz = "UTC")]
    data.table::setkey(tmp, utc); tmp
  } else NULL
  
  list(cap = cap, ds = ds, air = air, offset_us = offset_us)
}

# ─────────────────────────  MAIN WORKFLOW  ────────────────────────────────
process_astro <- function(astro_directory,
                          species, pilot, permit,
                          flight_date_directory,
                          prefer_logs = TRUE) {
  
  warning_msgs <- character()
  
  shiny::withProgress(message = "Processing Astro flight data...", value = 0, {
    
    #── 1. discover image layout ------------------------------------------------
    flights_root <- file.path(astro_directory, "flights")   # thumb‑drive structure
    jpg_root     <- file.path(astro_directory, "jpg")       # flat SD‑card dump
    
    if (dir.exists(flights_root)) {
      # ── thumb‑drive mode ──
      thumb_mode   <- TRUE
      flight_dirs  <- list.dirs(flights_root, full.names = TRUE, recursive = FALSE)
      flight_dirs  <- flight_dirs[
        grepl("^\\d+_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}$",
              basename(flight_dirs))]
      if (!length(flight_dirs))
        stop("No flight folders found under ", flights_root)
      
      flight_info <- data.table::data.table(
        folder      = flight_dirs,
        numeric_id  = as.integer(sub("_.*", "", basename(flight_dirs)))
      )[order(numeric_id)][, flightnum_day := .I]
      
    } else if (dir.exists(jpg_root)) {
      # ── flat SD‑card mode ──
      thumb_mode  <- FALSE
      flight_info <- data.table::data.table(
        folder        = jpg_root,
        numeric_id    = 1L,
        flightnum_day = 1L
      )
      
    } else {
      stop("Neither 'flights' nor 'jpg' folder found in ", astro_directory)
    }
    
    #── 2. locate ULog CSVs -----------------------------------------------------
    log_dir <- file.path(astro_directory, "Astro", "log")
    if (!dir.exists(log_dir)) {
      cand <- list.dirs(astro_directory, recursive = TRUE, full.names = TRUE)
      cand <- cand[grepl("(/|\\\\)Astro(/|\\\\)log$", cand, ignore.case = TRUE)]
      log_dir <- if (length(cand)) cand[1] else NA_character_
    }
    
    ulog <- NULL
    if (prefer_logs && !is.na(log_dir) && dir.exists(log_dir)) {
      shiny::incProgress(0.05, detail = "Loading ULog CSVs…")
      ulog <- .read_ulog_csvs(log_dir)
      if (is.null(ulog$cap) || !nrow(ulog$cap) || is.na(ulog$offset_us))
        ulog <- NULL
    }
    
    #── 3. iterate flights / folders ------------------------------------------
    astro_all     <- data.table::data.table()
    unmatched_all <- data.table::data.table()
    
    shiny::incProgress(0.15, detail = "Processing images…")
    
    for (ii in seq_len(nrow(flight_info))) {
      
      fnum  <- flight_info$flightnum_day[ii]
      fpath <- flight_info$folder[ii]
      
      jpeg_files <- list.files(fpath, pattern = "\\.(jpe?g)$",
                               ignore.case = TRUE, full.names = TRUE)
      if (!length(jpeg_files)) {
        warning_msgs <- c(warning_msgs, paste("No JPEGs in", fpath))
        next
      }
      
      tags <- c("FileName","Model","FocalLength",
                "ExifImageWidth","ExifImageHeight","ImageWidth","ImageHeight",
                "FocalPlaneXResolution","FocalPlaneResolutionUnit",
                "GPSLatitude","GPSLongitude","GPSAltitude#","DistanceToSubject")
      ex <- exifr::read_exif(jpeg_files, tags = tags, recursive = FALSE, quiet = TRUE)
      data.table::setDT(ex)
      
      ## 1.  ensure optional cols exist so mapply() never fails
      if (!"FocalPlaneXResolution"    %in% names(ex))
        ex[, FocalPlaneXResolution := NA_real_]
      if (!"FocalPlaneResolutionUnit" %in% names(ex))
        ex[, FocalPlaneResolutionUnit := NA_character_]
      
      ## 2.  build ImageWidth_px / ImageHeight_px FIRST
      ex[, ImageWidth_px  := fifelse(!is.na(ExifImageWidth),  ExifImageWidth,  ImageWidth)]
      ex[, ImageHeight_px := fifelse(!is.na(ExifImageHeight), ExifImageHeight, ImageHeight)]
      
      ## 3.  now safe to derive sensor width
      ex[, SensorWidth_mm := mapply(.derive_sensor_width, Model,
                                    ImageWidth_px,
                                    FocalPlaneXResolution,
                                    FocalPlaneResolutionUnit)]
      
      ## 4.  pixel size
      ex[, pixel_dimension_mm := SensorWidth_mm / ImageWidth_px]
      
      out <- data.table::data.table(
        FileName   = ex$FileName,
        ImageNum   = ex$ImageNum,
        datetime_utc = as.POSIXct(NA, tz = "UTC"),
        justtime     = NA_character_,
        flightnum    = fnum,
        platform     = "Astro",
        pilot        = pilot, permit = permit, species = species,
        whaleinfo    = NA_character_,
        Latitude     = NA_real_, Longitude = NA_real_, gps_alt_m = NA_real_,
        laser_alt_m  = NA_real_, raw_laser_alt_cm = NA_real_,
        barometric_alt_m = NA_real_,
        FocalLength_mm = as.numeric(ex$FocalLength),
        ImageWidth_px  = as.numeric(ex$ImageWidth_px),
        ImageHeight_px = as.numeric(ex$ImageHeight_px),
        SensorWidth_mm = as.numeric(ex$SensorWidth_mm),
        pixel_dimension_mm = as.numeric(ex$pixel_dimension_mm)
      )
      
      #──────── 3a. use ULog match if we have logs ───────────────────────
      if (!is.null(ulog)) {
        cap <- copy(ulog$cap)[order(seq, utc)]
        n_img <- nrow(out); n_cap <- nrow(cap); n_map <- min(n_img, n_cap)
        
        if (n_img != n_cap)
          warning_msgs <- c(warning_msgs,
                            sprintf("Folder %s: images=%d captures=%d mapping=%d",
                                    basename(fpath), n_img, n_cap, n_map))
        
        if (n_img > n_cap)
          unmatched_all <- rbind(unmatched_all,
                                 out[(n_cap+1):n_img, .(Type="image_without_capture",
                                                        FileName, ImageNum, flightnum=fnum)])
        if (n_cap > n_img)
          unmatched_all <- rbind(unmatched_all,
                                 cap[(n_img+1):n_cap, .(Type="capture_without_image",
                                                        seq, utc, lat, lon, alt, flightnum=fnum)])
        
        if (n_map) {
          # copy fields
          out$datetime_utc[1:n_map] <- cap$utc[1:n_map]
          out$justtime[1:n_map]     <- format(cap$utc[1:n_map], "%H:%M:%S", tz="UTC")
          out$Latitude[1:n_map]     <- cap$lat[1:n_map]
          out$Longitude[1:n_map]    <- cap$lon[1:n_map]
          out$gps_alt_m[1:n_map]    <- cap$alt[1:n_map]
          
          # lidar average
          if (!is.null(ulog$ds)) {
            for (k in seq_len(n_map)) {
              res <- .avg_lidar_one_second(ulog$ds, out$datetime_utc[k])
              out$laser_alt_m[k]      <- res$value
              out$raw_laser_alt_cm[k] <- res$value * 100
            }
          }
          
          # baro
          if (!is.null(ulog$air)) {
            baro <- ulog$air[,.(utc,baro_alt_meter)]; data.table::setkey(baro, utc)
            out$barometric_alt_m[1:n_map] <-
              baro[.(out$datetime_utc[1:n_map]), roll="nearest"]$baro_alt_meter
          }
        }
        
      } else {  #──────── 3b. EXIF‑only fallback ──────────────────────────
        out$Latitude       <- as.numeric(ex$GPSLatitude)
        out$Longitude      <- as.numeric(ex$GPSLongitude)
        out$gps_alt_m      <- as.numeric(ex$`GPSAltitude#`)
        out$laser_alt_m    <- as.numeric(ex$DistanceToSubject)
        out$raw_laser_alt_cm <- out$laser_alt_m * 100
      }
      
      astro_all <- rbind(astro_all, out, fill = TRUE)
    }  # flight loop end
    
    #── 4. write outputs -------------------------------------------------
    stub <- basename(flight_date_directory)
    data.table::fwrite(astro_all,
                       file.path(astro_directory,
                                 paste0(stub, "_Astro_imgdata.csv")))
    
    if (nrow(unmatched_all)) {
      data.table::fwrite(unmatched_all,
                         file.path(astro_directory,
                                   paste0(stub, "_Astro_imgdata_unmatched.csv")))
      warning_msgs <- c(warning_msgs,
                        sprintf("Unmatched rows written (%d)", nrow(unmatched_all)))
    } else warning_msgs <- c(warning_msgs, "All images matched to captures")
    
    shiny::incProgress(1, detail = "Done")
  })
  
  warning_msgs
}
