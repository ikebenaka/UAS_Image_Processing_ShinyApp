# associate_measurements.R — logging + diagnostics; skip non-measurement CSVs; ignore "_" folders
# Pixels merged from EGNO CSVs into per-day photos_measured.csv; season rollups (meters only when laser_alt_m present)

associate_measurements <- function(
    season_directory,
    measurement_status_message = NULL,   # optional; still called if supplied
    debug_exports = TRUE,                # write diagnostics CSVs under Photos_Measured/_diagnostics
    write_season_outputs = TRUE
) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(tidyr); library(stringr)
    library(purrr); library(tibble); library(lubridate)
  })
  options(warn = 1)
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  now_stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # --------- Canonical schema ---------
  tl_w_names <- sprintf("TL_w%05.2f", seq(5, 95, by = 5))   # TL_w05.00 ... TL_w95.00
  MEAS_ORDER   <- c("TL", "L.to.blowhole", "L.to.start.of.fluke", tl_w_names, "W.at.eyes", "W.fluke")
  MEAS_ALLOWED <- MEAS_ORDER
  
  META_ORDER <- c(
    "FileName","ImageNum","datetime_utc","justtime","flightnum","platform",
    "pilot","permit","species","whaleinfo","Latitude","Longitude",
    "gps_alt_m","raw_laser_alt_cm","tilt_deg","costilt","laser_alt_m","barometric_alt_m",
    "FocalLength_mm","ImageWidth_px","ImageHeight_px","SensorWidth_mm","pixel_dimension_mm",
    "EGNO","camera_focus","body_straightness","body_roll","body_arch","body_pitch",
    "body_length_measurability","body_width_measurability","photogram_quality","photogram_comments"
  )
  
  # --------- Logger ---------
  season_dir_parts <- str_split(season_directory, "/|\\\\")[[1]]
  season_name <- tail(season_dir_parts, 1)
  year        <- tail(season_dir_parts, 2)[1]
  
  logs_dir <- file.path(season_directory, "_logs")
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(logs_dir, sprintf("%s_%s_associate_measurements_%s.log", season_name, year, now_stamp()))
  
  log_line <- function(txt) {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ln  <- paste0("[", ts, "] ", txt)
    cat(ln, "\n"); flush.console()
    try(write(ln, file = log_file, append = TRUE), silent = TRUE)
    if (!is.null(measurement_status_message)) try(measurement_status_message(ln), silent = TRUE)
    invisible(NULL)
  }
  
  # --------- Helpers ---------
  immediate_subdirs <- function(path) {
    kids <- list.files(path, full.names = TRUE, recursive = FALSE, include.dirs = TRUE, no.. = TRUE)
    if (!length(kids)) return(character())
    kids[file.info(kids)$isdir %in% TRUE]
  }
  # Exclude helper: folders we should never treat as flight days or EGNO dirs
  is_excluded_dirname <- function(x) {
    x <- basename(x)
    startsWith(x, "_") | x %in% c("Archive","Meters","Pixels","Diagnostics","diagnostics","_diagnostics","_logs")
  }
  # Keep only date-like folders for flight days (e.g., 20240408)
  is_date_like <- function(x) grepl("^\\d{8}$", basename(x))
  
  normalize_measure_name <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (x %in% c("TL","L.to.blowhole","L.to.start.of.fluke","W.at.eyes","W.fluke")) return(x)
    if (str_detect(x, regex("^TL[_\\.]?w", ignore_case = TRUE))) {
      num <- str_extract(x, "(?i)(?<=TL[_\\.]?w)[0-9]+(?:\\.[0-9]+)?")
      if (!is.na(num)) return(sprintf("TL_w%05.2f", as.numeric(num)))
    }
    x
  }
  # Robust numeric parser (ASCII-safe): strip everything except digits , . -
  as_numeric_na_zero <- function(x) {
    x_chr <- as.character(x)
    x_chr <- trimws(x_chr)
    # Remove all characters except digits, comma, dot, minus
    x_chr <- gsub("[^0-9,\\.\\-]+", "", x_chr)
    # First pass: decimal ".", grouping ","
    y1 <- suppressWarnings(readr::parse_number(
      x_chr, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ))
    # Fallback: decimal ",", grouping "."
    if (sum(!is.na(y1)) < ceiling(length(x_chr) * 0.2)) {
      y2 <- suppressWarnings(readr::parse_number(
        x_chr, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
      ))
      y <- if (sum(!is.na(y2)) > sum(!is.na(y1))) y2 else y1
    } else {
      y <- y1
    }
    y[is.na(y) | y == 0] <- NA_real_
    y
  }
  ensure_single_egno <- function(vec, file_path) {
    bad <- which(str_detect(vec %||% "", ","))
    if (length(bad)) {
      msg <- sprintf("ERROR: Multiple EGNOs in a single cell in %s (first offending row index: %s)", file_path, bad[1])
      log_line(msg); stop(msg, call. = FALSE)
    }
    invisible(TRUE)
  }
  find_col <- function(nms, candidates) {
    norm <- function(s) gsub("[ _]", "", tolower(s))
    nn <- norm(nms); cc <- norm(candidates)
    hit <- match(cc, nn, nomatch = 0)
    idx <- which(hit > 0)
    if (length(idx)) nms[hit[idx[1]]] else NA_character_
  }
  has_measurement_values <- function(df, meas_cols = MEAS_ALLOWED) {
    present <- intersect(meas_cols, names(df))
    if (!length(present)) return(rep(FALSE, nrow(df)))
    rowSums(!is.na(df[, present, drop = FALSE])) > 0
  }
  missing_meter_inputs <- function(df) {
    required <- c("laser_alt_m", "FocalLength_mm")
    present_required <- required[required %in% names(df)]
    missing_core <- rep(FALSE, nrow(df))
    if (length(present_required)) {
      missing_core <- rowSums(is.na(df[, present_required, drop = FALSE])) > 0
    } else {
      missing_core <- rep(TRUE, nrow(df))
    }
    has_pixel_dim <- if ("pixel_dimension_mm" %in% names(df)) {
      !is.na(df$pixel_dimension_mm)
    } else {
      rep(FALSE, nrow(df))
    }
    has_sensor_fallback <- if (all(c("SensorWidth_mm", "ImageWidth_px") %in% names(df))) {
      !is.na(df$SensorWidth_mm) & !is.na(df$ImageWidth_px) & df$ImageWidth_px != 0
    } else {
      rep(FALSE, nrow(df))
    }
    missing_core | !(has_pixel_dim | has_sensor_fallback)
  }
  append_warning_column <- function(df, column, mask, message) {
    if (!column %in% names(df)) df[[column]] <- NA_character_
    mask[is.na(mask)] <- FALSE
    df[[column]][mask] <- ifelse(
      is.na(df[[column]][mask]) | !nzchar(df[[column]][mask]),
      message,
      paste(df[[column]][mask], message, sep = "; ")
    )
    df
  }
  
  # --- Read one EGNO measurement CSV (skip non-measurement CSVs gracefully) ---
  read_egno_measure_csv <- function(csv_path, egno_name) {
    df <- tryCatch(suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE)),
                   error = function(e) { log_line(sprintf("WARN: Failed to read %s (%s)", csv_path, e$message)); return(NULL) })
    if (is.null(df) || !nrow(df)) return(NULL)
    
    nms <- names(df)
    obj_col  <- find_col(nms, c("Object","Object Name"))
    val_col  <- find_col(nms, c("Value","Measurement Value"))
    unit_col <- find_col(nms, c("Value_unit","Value Unit","Unit"))
    # If this is not a measurement CSV, SKIP (don't error)
    if (any(is.na(c(obj_col,val_col,unit_col)))) {
      log_line(sprintf("SKIP: Non-measurement CSV (schema mismatch): %s", csv_path))
      return(NULL)
    }
    
    img_row <- df %>% filter(str_trim(str_to_lower(.data[[obj_col]])) == "image path") %>% slice(1)
    if (!nrow(img_row)) {
      log_line(sprintf("SKIP: No 'Image Path' row: %s", csv_path))
      return(NULL)
    }
    file_name <- basename(str_trim(as.character(img_row[[val_col]][1])))
    if (!nzchar(file_name)) {
      log_line(sprintf("SKIP: Empty Image Path value: %s", csv_path))
      return(NULL)
    }
    
    df_pix <- df %>%
      mutate(
        .unit = str_to_lower(trimws(.data[[unit_col]])),
        .unit_norm = gsub("[^a-z]", "", .unit)   # normalize to letters only
      ) %>%
      filter(.unit_norm %in% c("pixel","pixels")) %>%   # pixels only, tolerant to spaces/parens/etc.
      mutate(measure = vapply(.data[[obj_col]], normalize_measure_name, character(1)),
             value   = as_numeric_na_zero(.data[[val_col]])) %>%
      transmute(FileName = file_name, EGNO = as.character(egno_name), measure, value)
    
    # If no pixel rows, just skip silently (or log)
    if (!nrow(df_pix)) {
      log_line(sprintf("SKIP: No 'Pixels' rows in %s", csv_path))
      return(NULL)
    }
    
    # Validate whitelist (still a hard error if names are non-standard)
    bad_meas <- setdiff(unique(df_pix$measure), MEAS_ALLOWED)
    if (length(bad_meas)) {
      msg <- sprintf("ERROR: Unknown measurement name(s) in %s: %s\nAllowed: %s",
                     csv_path, paste(bad_meas, collapse = ", "), paste(MEAS_ALLOWED, collapse = ", "))
      log_line(msg); stop(msg, call. = FALSE)
    }
    
    df_pix <- df_pix %>% mutate(FileName_key = tolower(str_trim(FileName)),
                                EGNO_key     = tolower(str_trim(EGNO)))
    dup_check <- df_pix %>% count(FileName_key, EGNO_key, measure, name = "n") %>% filter(n > 1)
    if (nrow(dup_check)) {
      msg <- sprintf("ERROR: Duplicate (FileName, EGNO, measure) in %s (e.g., %s, %s, %s).",
                     csv_path, dup_check$FileName_key[1], dup_check$EGNO_key[1], dup_check$measure[1])
      log_line(msg); stop(msg, call. = FALSE)
    }
    df_pix
  }
  
  # --------- Traverse season ---------
  warnings <- character()
  all_updated_day_tables <- list()
  
  # filter out folders starting "_" and require date-like names; if the selected
  # directory is itself a flight day, process only that one day.
  if (is_date_like(season_directory)) {
    flight_date_directories <- season_directory
    log_line(sprintf("Flight day selected: %s", basename(season_directory)))
  } else {
    flight_date_directories <- immediate_subdirs(season_directory)
    flight_date_directories <- flight_date_directories[!is_excluded_dirname(flight_date_directories)]
    flight_date_directories <- flight_date_directories[is_date_like(flight_date_directories)]
    log_line(sprintf("Season: %s/%s | Flight days found: %d", season_name, year, length(flight_date_directories)))
  }
  
  for (flight_dir in flight_date_directories) {
    flight_date <- basename(flight_dir)
    # platforms: immediate subdirs not starting "_" and not special bins
    platform_directories <- immediate_subdirs(flight_dir)
    platform_directories <- platform_directories[!is_excluded_dirname(platform_directories)]
    log_line(sprintf("→ Date %s: platforms=%d", flight_date, length(platform_directories)))
    
    for (platform_dir in platform_directories) {
      platform_name <- basename(platform_dir)
      platform_name_clean <- gsub(" ", "_", platform_name)
      log_line(sprintf("   • Platform: %s", platform_name))
      
      photos_measured_dir <- file.path(platform_dir, "Photos_Measured")
      if (!dir.exists(photos_measured_dir)) {
        msg <- sprintf("No Photos_Measured for %s on %s", platform_name, flight_date)
        log_line(paste("WARN:", msg)); warnings <- c(warnings, msg); next
      }
      
      # ---- Locate per-day photos_measured.csv (filename-only) ----
      date_prefix <- paste0("^", flight_date)
      candidates <- list.files(photos_measured_dir, pattern = "_photos_measured\\.csv$", full.names = TRUE)
      candidates <- candidates[basename(candidates) %>% str_detect(date_prefix)]
      
      normalize_token <- function(s) { s <- tolower(str_trim(s %||% "")); gsub("[ _]+", "_", s) }
      plat_tok <- normalize_token(platform_name)
      
      ranked <- candidates[vapply(candidates, function(f) {
        fn <- normalize_token(basename(f)); grepl(plat_tok, fn, fixed = TRUE)
      }, logical(1))]
      
      pm_file <- NA_character_
      if (length(ranked) == 0L) {
        if (length(candidates) == 1L) {
          pm_file <- candidates[1]
        } else if (length(candidates) > 1L) {
          msg <- sprintf(
            "Multiple per-day CSVs match date %s but none match platform token '%s'. Candidates: %s",
            flight_date, plat_tok, paste(basename(candidates), collapse = ", ")
          )
          log_line(paste("ERROR:", msg)); stop(msg, call. = FALSE)
        } else {
          msg <- sprintf("No per-day photos_measured.csv with date prefix for %s on %s", platform_name, flight_date)
          log_line(paste("WARN:", msg)); warnings <- c(warnings, msg); next
        }
      } else pm_file <- ranked[1]
      log_line(sprintf("      per-day CSV: %s", basename(pm_file)))
      
      # Read base and enforce single EGNO per row
      base <- suppressMessages(readr::read_csv(pm_file, show_col_types = FALSE))
      if (!"EGNO" %in% names(base)) base$EGNO <- NA_character_
      ensure_single_egno(base$EGNO, pm_file)
      
      # Normalize ImageNum type across all days
      if ("ImageNum" %in% names(base)) {
        base$ImageNum <- as.character(base$ImageNum)
      }
      
      # Ensure measurement columns exist and are numeric
      for (m in MEAS_ALLOWED) {
        if (!m %in% names(base)) base[[m]] <- NA_real_
        base[[m]] <- suppressWarnings(as.numeric(base[[m]]))
      }
      
      base2 <- base %>%
        mutate(FileName_key = tolower(str_trim(as.character(FileName %||% ""))),
               EGNO_key     = tolower(str_trim(as.character(EGNO %||% ""))))
      duplicate_base_keys <- base2 %>%
        count(FileName_key, EGNO_key, name = "n") %>%
        filter(n > 1)
      if (nrow(duplicate_base_keys)) {
        ex <- paste(
          head(sprintf("[%s, %s] x%s", duplicate_base_keys$FileName_key, duplicate_base_keys$EGNO_key, duplicate_base_keys$n), 5),
          collapse = "; "
        )
        msg <- sprintf(
          "Duplicate FileName + EGNO keys in %s. Matching measurements may be applied to multiple rows: %s",
          basename(pm_file), ex
        )
        log_line(paste("WARN:", msg)); warnings <- c(warnings, msg)
      }
      
      # ---- Harvest pixel measurements from EGNO folders ----
      egno_dirs <- immediate_subdirs(photos_measured_dir)
      egno_dirs <- egno_dirs[!is_excluded_dirname(egno_dirs)]  # <-- exclude _diagnostics and friends
      log_line(sprintf("      EGNO folders detected: %d (%s)",
                       length(egno_dirs), paste(basename(egno_dirs), collapse = ", ")))
      
      per_platform_meas_long <- list()
      csv_count <- 0L; pix_rows <- 0L
      
      if (length(egno_dirs)) {
        for (egdir in egno_dirs) {
          egno_name <- str_trim(basename(egdir))
          csvs <- list.files(egdir, pattern = "\\.csv$", full.names = TRUE)
          if (!length(csvs)) next
          for (cf in csvs) {
            csv_count <- csv_count + 1L
            df_pix <- read_egno_measure_csv(cf, egno_name)
            if (!is.null(df_pix) && nrow(df_pix)) {
              pix_rows <- pix_rows + nrow(df_pix)
              per_platform_meas_long[[length(per_platform_meas_long)+1]] <- df_pix
            }
          }
        }
      } else {
        msg <- sprintf("No EGNO folders under Photos_Measured for %s on %s", platform_name, flight_date)
        log_line(paste("WARN:", msg)); warnings <- c(warnings, msg)
      }
      log_line(sprintf("      EGNO CSVs read=%d | pixel rows harvested=%d", csv_count, pix_rows))
      
      # Merge + diagnostics
      n_measures <- 0L; n_meas_keys <- 0L; matched_rows <- 0L
      unmatched_measurement_keys <- 0L
      unmeasured_base_rows <- 0L
      diag_dir <- file.path(photos_measured_dir, "_diagnostics")
      if (debug_exports && !dir.exists(diag_dir)) dir.create(diag_dir, showWarnings = FALSE)
      
      if (length(per_platform_meas_long)) {
        all_meas_long <- bind_rows(per_platform_meas_long)
        dup_all <- all_meas_long %>% count(FileName_key, EGNO_key, measure, name = "n") %>% filter(n > 1)
        if (nrow(dup_all)) {
          msg <- sprintf("ERROR: Duplicate (FileName, EGNO, measure) across EGNO CSVs (e.g., %s, %s, %s).",
                         dup_all$FileName_key[1], dup_all$EGNO_key[1], dup_all$measure[1])
          log_line(msg); stop(msg, call. = FALSE)
        }
        
        n_measures  <- nrow(all_meas_long)
        meas_wide   <- all_meas_long %>%
          tidyr::pivot_wider(id_cols = c(FileName_key, EGNO_key), names_from = measure, values_from = value)
        
        # count non-NA measurement cells (parsing sanity check)
        meas_cols <- setdiff(names(meas_wide), c("FileName_key","EGNO_key"))
        non_na_cells <- if (length(meas_cols)) sum(colSums(!is.na(meas_wide[meas_cols]))) else 0
        log_line(sprintf("      Measurement value cells (non-NA) = %d", non_na_cells))
        n_meas_keys <- nrow(meas_wide %>% distinct(FileName_key, EGNO_key))
        
        base_keys <- base2 %>% distinct(FileName_key, EGNO_key)
        meas_keys <- meas_wide %>% distinct(FileName_key, EGNO_key)
        
        if (debug_exports) {
          tok <- gsub("[^A-Za-z0-9_]+", "_", paste0(flight_date, "_", platform_name))
          suppressWarnings({
            write_csv(base_keys, file.path(diag_dir, paste0(tok, "_base_keys.csv")))
            write_csv(meas_keys, file.path(diag_dir, paste0(tok, "_meas_keys.csv")))
          })
        }
        
        meas_not_in_base <- anti_join(meas_keys, base_keys, by = c("FileName_key","EGNO_key"))
        base_not_in_meas <- anti_join(base_keys, meas_keys, by = c("FileName_key","EGNO_key"))
        
        if (debug_exports) {
          tok <- gsub("[^A-Za-z0-9_]+", "_", paste0(flight_date, "_", platform_name))
          suppressWarnings({
            write_csv(meas_not_in_base, file.path(diag_dir, paste0(tok, "_unmatched_meas_no_base.csv")))
            write_csv(base_not_in_meas, file.path(diag_dir, paste0(tok, "_unmatched_base_no_meas.csv")))
          })
        }
        
        if (nrow(meas_not_in_base)) {
          unmatched_measurement_keys <- nrow(meas_not_in_base)
          ex <- paste(head(sprintf("[%s, %s]", meas_not_in_base$FileName_key, meas_not_in_base$EGNO_key), 5), collapse = "; ")
          log_line(sprintf("      WARN: No base match for %d measurement key(s): %s", nrow(meas_not_in_base), ex))
        }
        if (nrow(base_not_in_meas)) {
          unmeasured_base_rows <- nrow(base_not_in_meas)
          ex <- paste(head(sprintf("[%s, %s]", base_not_in_meas$FileName_key, base_not_in_meas$EGNO_key), 5), collapse = "; ")
          log_line(sprintf("      INFO: %d base row(s) have no pixel measurements yet: %s", nrow(base_not_in_meas), ex))
        }
        
        # ---- JOIN with explicit suffix and coalesce into base ----
        joined <- dplyr::left_join(
          base2, meas_wide,
          by = c("FileName_key","EGNO_key"),
          suffix = c("", "_px")            # <- explicit suffix for measurement side
        )
        
        # Count rows that actually have any pixel values (*_px columns)
        px_cols <- paste0(MEAS_ALLOWED, "_px")
        available_px <- intersect(px_cols, names(joined))
        got_any <- if (length(available_px)) {
          rowSums(!is.na(joined[, available_px, drop = FALSE])) > 0
        } else {
          rep(FALSE, nrow(joined))
        }
        matched_rows <- sum(got_any, na.rm = TRUE)
        
        # Copy pixel values into the base columns, 0 -> NA, preserving existing base values if pixel is NA
        for (m in MEAS_ALLOWED) {
          m_px <- paste0(m, "_px")
          if (m_px %in% names(joined)) {
            base[[m]] <- dplyr::coalesce(joined[[m_px]], base[[m]])
            base[[m]][base[[m]] == 0] <- NA_real_
          }
        }
      } else {
        log_line("      No pixel measurements harvested.")
        unmeasured_base_rows <- nrow(base)
      }

      measured_mask <- has_measurement_values(base)
      missing_conversion_mask <- measured_mask & missing_meter_inputs(base)
      if (any(missing_conversion_mask, na.rm = TRUE)) {
        base <- append_warning_column(
          base,
          "measurement_qa_warnings",
          missing_conversion_mask,
          "measurement values present but altitude/sensor fields are missing for meter conversion"
        )
        log_line(sprintf(
          "      WARN: %d measured row(s) are missing altitude/sensor fields needed for meter conversion.",
          sum(missing_conversion_mask, na.rm = TRUE)
        ))
      }
      
      # Column order & write
      missing_meta <- setdiff(META_ORDER, names(base)); if (length(missing_meta)) base[missing_meta] <- NA
      missing_meas <- setdiff(MEAS_ORDER, names(base)); if (length(missing_meas)) base[missing_meas] <- NA_real_
      base <- base %>% select(any_of(META_ORDER), any_of(MEAS_ORDER), everything())
      
      backup_path <- file.path(photos_measured_dir, sprintf("%s_%s_photos_measured.backup-%s.csv",
                                                            flight_date, platform_name_clean, now_stamp()))
      readr::write_csv(suppressMessages(readr::read_csv(pm_file, show_col_types = FALSE)), backup_path)
      readr::write_csv(strip_qa_warnings_column(base), pm_file)
      
      log_line(sprintf(
        "      Summary: base rows=%s; measured rows=%s; unmeasured rows=%s; EGNO CSVs read=%s; pixel rows=%s; unique measurement keys=%s; matched base rows=%s; unmatched measurement keys=%s",
        nrow(base), sum(measured_mask, na.rm = TRUE), sum(!measured_mask, na.rm = TRUE),
        csv_count, pix_rows, n_meas_keys, matched_rows, unmatched_measurement_keys
      ))
      log_line(sprintf("      Updated %s", basename(pm_file)))
      
      # collect for season combine
      all_updated_day_tables[[length(all_updated_day_tables)+1]] <- base
    }
  }
  
  # --------- Season-level combined outputs ---------
  if (!length(all_updated_day_tables)) {
    log_line("No per-day photos_measured.csv files were updated; season-level outputs skipped.")
    log_line(sprintf("LOG saved to: %s", log_file))
    return(invisible(NULL))
  }

  if (!write_season_outputs) {
    log_line("Flight-day measurement collation complete; season-level combined outputs skipped.")
    log_line(sprintf("LOG saved to: %s", log_file))
    return(invisible(all_updated_day_tables))
  }
  
  combined_pixels <- bind_rows(all_updated_day_tables)
  combined_pixels_file <- file.path(season_directory, sprintf("%s_%s_Combined_Photos_Measured.csv", season_name, year))
  readr::write_csv(strip_qa_warnings_column(combined_pixels), combined_pixels_file)
  log_line(paste("Wrote:", basename(combined_pixels_file)))
  
  # meters only when laser_alt_m present
  cm <- combined_pixels
  to_num <- unique(c("laser_alt_m","FocalLength_mm","pixel_dimension_mm","SensorWidth_mm","ImageWidth_px", MEAS_ALLOWED))
  for (cc in to_num) if (cc %in% names(cm)) cm[[cc]] <- suppressWarnings(as.numeric(cm[[cc]]))
  
  n <- nrow(cm); mpp <- rep(NA_real_, n)
  laser_mask <- !is.na(cm$laser_alt_m)
  
  primary_mask <- laser_mask & !is.na(cm$pixel_dimension_mm) & !is.na(cm$FocalLength_mm) & (cm$FocalLength_mm != 0)
  mpp[primary_mask] <- cm$laser_alt_m[primary_mask] * cm$pixel_dimension_mm[primary_mask] / cm$FocalLength_mm[primary_mask]
  
  fallback_mask <- laser_mask & is.na(mpp) &
    !is.na(cm$SensorWidth_mm) & !is.na(cm$FocalLength_mm) & !is.na(cm$ImageWidth_px) &
    (cm$FocalLength_mm != 0) & (cm$ImageWidth_px != 0)
  mpp[fallback_mask] <- cm$laser_alt_m[fallback_mask] * cm$SensorWidth_mm[fallback_mask] /
    (cm$FocalLength_mm[fallback_mask] * cm$ImageWidth_px[fallback_mask])
  
  combined_meters <- cm
  for (m in MEAS_ALLOWED) if (m %in% names(combined_meters))
    combined_meters[[m]] <- ifelse(laser_mask, combined_meters[[m]] * mpp, NA_real_)

  measured_pixel_rows <- has_measurement_values(cm)
  missing_mpp_rows <- measured_pixel_rows & is.na(mpp)
  if (any(missing_mpp_rows, na.rm = TRUE)) {
    combined_meters <- append_warning_column(
      combined_meters,
      "measurement_qa_warnings",
      missing_mpp_rows,
      "measurement values present but meter conversion inputs are missing"
    )
    log_line(sprintf("WARN: %d measured row(s) could not be converted to meters because altitude/sensor inputs are missing.", sum(missing_mpp_rows, na.rm = TRUE)))
  }
  impossible_length <- "TL" %in% names(combined_meters) & !is.na(combined_meters$TL) &
    (combined_meters$TL < 2 | combined_meters$TL > 35)
  width_cols <- intersect(c(tl_w_names, "W.at.eyes", "W.fluke"), names(combined_meters))
  impossible_width <- if (length(width_cols)) {
    rowSums(combined_meters[, width_cols, drop = FALSE] < 0.1 | combined_meters[, width_cols, drop = FALSE] > 10, na.rm = TRUE) > 0
  } else {
    rep(FALSE, nrow(combined_meters))
  }
  if (any(impossible_length | impossible_width, na.rm = TRUE)) {
    combined_meters <- append_warning_column(
      combined_meters,
      "measurement_qa_warnings",
      impossible_length | impossible_width,
      "meter measurements outside expected large-whale range"
    )
    log_line(sprintf("WARN: %d row(s) have meter measurements outside expected large-whale ranges.", sum(impossible_length | impossible_width, na.rm = TRUE)))
  }
  
  missing_meta <- setdiff(META_ORDER, names(combined_meters)); if (length(missing_meta)) combined_meters[missing_meta] <- NA
  missing_meas <- setdiff(MEAS_ORDER, names(combined_meters)); if (length(missing_meas)) combined_meters[missing_meas] <- NA_real_
  combined_meters <- combined_meters %>% select(any_of(META_ORDER), any_of(MEAS_ORDER), everything())
  
  combined_meters_file <- file.path(season_directory, sprintf("%s_%s_Combined_Photos_Measured_Meters.csv", season_name, year))
  readr::write_csv(strip_qa_warnings_column(combined_meters), combined_meters_file)
  log_line(paste("Wrote:", basename(combined_meters_file)))
  
  log_line(sprintf("LOG saved to: %s", log_file))
  invisible(NULL)
}

collate_measurements_for_flight_day <- function(
    flight_day_directory,
    measurement_status_message = NULL,
    debug_exports = TRUE
) {
  associate_measurements(
    season_directory = flight_day_directory,
    measurement_status_message = measurement_status_message,
    debug_exports = debug_exports,
    write_season_outputs = FALSE
  )
}

combine_field_season_measurements <- function(
    season_directory,
    measurement_status_message = NULL
) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(stringr)
  })

  `%||%` <- function(a, b) if (is.null(a)) b else a
  now_stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

  tl_w_names <- sprintf("TL_w%05.2f", seq(5, 95, by = 5))
  MEAS_ORDER <- c("TL", "L.to.blowhole", "L.to.start.of.fluke", tl_w_names, "W.at.eyes", "W.fluke")

  META_ORDER <- c(
    "FileName","ImageNum","datetime_utc","justtime","flightnum","platform",
    "pilot","permit","species","whaleinfo","Latitude","Longitude",
    "gps_alt_m","raw_laser_alt_cm","tilt_deg","costilt","laser_alt_m","barometric_alt_m",
    "FocalLength_mm","ImageWidth_px","ImageHeight_px","SensorWidth_mm","pixel_dimension_mm",
    "EGNO","camera_focus","body_straightness","body_roll","body_arch","body_pitch",
    "body_length_measurability","body_width_measurability","photogram_quality","photogram_comments"
  )

  season_dir_parts <- str_split(season_directory, "/|\\\\")[[1]]
  season_name <- tail(season_dir_parts, 1)
  year <- tail(season_dir_parts, 2)[1]

  logs_dir <- file.path(season_directory, "_logs")
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(logs_dir, sprintf("%s_%s_combine_field_season_measurements_%s.log", season_name, year, now_stamp()))

  log_line <- function(txt) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ln <- paste0("[", ts, "] ", txt)
    cat(ln, "\n"); flush.console()
    try(write(ln, file = log_file, append = TRUE), silent = TRUE)
    if (!is.null(measurement_status_message)) try(measurement_status_message(ln), silent = TRUE)
    invisible(NULL)
  }

  immediate_subdirs <- function(path) {
    kids <- list.files(path, full.names = TRUE, recursive = FALSE, include.dirs = TRUE, no.. = TRUE)
    if (!length(kids)) return(character())
    kids[file.info(kids)$isdir %in% TRUE]
  }
  is_excluded_dirname <- function(x) {
    x <- basename(x)
    startsWith(x, "_") | x %in% c("Archive","Meters","Pixels","Diagnostics","diagnostics","_diagnostics","_logs")
  }
  is_date_like <- function(x) grepl("^\\d{8}$", basename(x))
  backup_if_exists <- function(path) {
    if (!file.exists(path)) return(invisible(NULL))
    backup_path <- sub("\\.csv$", paste0(".backup-", now_stamp(), ".csv"), path, ignore.case = TRUE)
    file.copy(path, backup_path, overwrite = FALSE)
    log_line(sprintf("Existing output backed up to: %s", backup_path))
    invisible(backup_path)
  }
  has_measurement_values <- function(df, meas_cols = MEAS_ORDER) {
    present <- intersect(meas_cols, names(df))
    if (!length(present)) return(rep(FALSE, nrow(df)))
    rowSums(!is.na(df[, present, drop = FALSE])) > 0
  }
  append_warning_column <- function(df, column, mask, message) {
    if (!column %in% names(df)) df[[column]] <- NA_character_
    mask[is.na(mask)] <- FALSE
    df[[column]][mask] <- ifelse(
      is.na(df[[column]][mask]) | !nzchar(df[[column]][mask]),
      message,
      paste(df[[column]][mask], message, sep = "; ")
    )
    df
  }

  if (!dir.exists(season_directory)) {
    stop(sprintf("Season directory does not exist: %s", season_directory), call. = FALSE)
  }

  flight_date_directories <- immediate_subdirs(season_directory)
  flight_date_directories <- flight_date_directories[!is_excluded_dirname(flight_date_directories)]
  flight_date_directories <- flight_date_directories[is_date_like(flight_date_directories)]
  log_line(sprintf("Field season selected: %s/%s | Flight days found: %d", season_name, year, length(flight_date_directories)))

  all_tables <- list()
  for (flight_dir in flight_date_directories) {
    flight_date <- basename(flight_dir)
    platform_directories <- immediate_subdirs(flight_dir)
    platform_directories <- platform_directories[!is_excluded_dirname(platform_directories)]

    for (platform_dir in platform_directories) {
      platform_name <- basename(platform_dir)
      photos_measured_dir <- file.path(platform_dir, "Photos_Measured")
      if (!dir.exists(photos_measured_dir)) {
        log_line(sprintf("WARN: No Photos_Measured for %s on %s", platform_name, flight_date))
        next
      }

      candidates <- list.files(photos_measured_dir, pattern = "_photos_measured\\.csv$", full.names = TRUE)
      candidates <- candidates[grepl(paste0("^", flight_date), basename(candidates))]
      if (!length(candidates)) {
        log_line(sprintf("WARN: No photos_measured.csv found for %s on %s", platform_name, flight_date))
        next
      }

      if (length(candidates) > 1) {
        log_line(sprintf(
          "WARN: Multiple photos_measured.csv files found for %s on %s; reading all: %s",
          platform_name, flight_date, paste(basename(candidates), collapse = ", ")
        ))
      }

      for (pm_file in candidates) {
        df <- tryCatch(
          suppressMessages(readr::read_csv(pm_file, show_col_types = FALSE)),
          error = function(e) {
            log_line(sprintf("WARN: Failed to read %s (%s)", pm_file, e$message))
            NULL
          }
        )
        if (is.null(df) || !nrow(df)) next

        for (m in MEAS_ORDER) {
          if (!m %in% names(df)) df[[m]] <- NA_real_
          df[[m]] <- suppressWarnings(as.numeric(df[[m]]))
        }
        if ("ImageNum" %in% names(df)) df$ImageNum <- as.character(df$ImageNum)

        df$source_flight_day <- flight_date
        df$source_platform_folder <- platform_name
        df$source_photos_measured_csv <- basename(pm_file)
        all_tables[[length(all_tables) + 1]] <- df
        log_line(sprintf("Read %s rows from %s", nrow(df), pm_file))
      }
    }
  }

  if (!length(all_tables)) {
    log_line("No photos_measured.csv files found; field-season outputs skipped.")
    log_line(sprintf("LOG saved to: %s", log_file))
    return(invisible(NULL))
  }

  combined_pixels <- bind_rows(all_tables)
  missing_meta <- setdiff(META_ORDER, names(combined_pixels)); if (length(missing_meta)) combined_pixels[missing_meta] <- NA
  missing_meas <- setdiff(MEAS_ORDER, names(combined_pixels)); if (length(missing_meas)) combined_pixels[missing_meas] <- NA_real_
  combined_pixels <- combined_pixels %>% select(any_of(META_ORDER), any_of(MEAS_ORDER), everything())

  combined_pixels_file <- file.path(season_directory, sprintf("%s_%s_Combined_Photos_Measured.csv", season_name, year))
  backup_if_exists(combined_pixels_file)
  readr::write_csv(strip_qa_warnings_column(combined_pixels), combined_pixels_file)
  log_line(sprintf("Wrote %s rows to: %s", nrow(combined_pixels), combined_pixels_file))

  combined_meters <- combined_pixels
  to_num <- unique(c("laser_alt_m","FocalLength_mm","pixel_dimension_mm","SensorWidth_mm","ImageWidth_px", MEAS_ORDER))
  for (cc in to_num) if (cc %in% names(combined_meters)) combined_meters[[cc]] <- suppressWarnings(as.numeric(combined_meters[[cc]]))

  n <- nrow(combined_meters)
  mpp <- rep(NA_real_, n)
  laser_mask <- !is.na(combined_meters$laser_alt_m)

  primary_mask <- laser_mask & !is.na(combined_meters$pixel_dimension_mm) & !is.na(combined_meters$FocalLength_mm) & (combined_meters$FocalLength_mm != 0)
  mpp[primary_mask] <- combined_meters$laser_alt_m[primary_mask] * combined_meters$pixel_dimension_mm[primary_mask] / combined_meters$FocalLength_mm[primary_mask]

  fallback_mask <- laser_mask & is.na(mpp) &
    !is.na(combined_meters$SensorWidth_mm) & !is.na(combined_meters$FocalLength_mm) & !is.na(combined_meters$ImageWidth_px) &
    (combined_meters$FocalLength_mm != 0) & (combined_meters$ImageWidth_px != 0)
  mpp[fallback_mask] <- combined_meters$laser_alt_m[fallback_mask] * combined_meters$SensorWidth_mm[fallback_mask] /
    (combined_meters$FocalLength_mm[fallback_mask] * combined_meters$ImageWidth_px[fallback_mask])

  for (m in MEAS_ORDER) if (m %in% names(combined_meters))
    combined_meters[[m]] <- ifelse(laser_mask, combined_meters[[m]] * mpp, NA_real_)

  measured_pixel_rows <- has_measurement_values(combined_pixels)
  missing_mpp_rows <- measured_pixel_rows & is.na(mpp)
  if (any(missing_mpp_rows, na.rm = TRUE)) {
    combined_meters <- append_warning_column(
      combined_meters,
      "measurement_qa_warnings",
      missing_mpp_rows,
      "measurement values present but meter conversion inputs are missing"
    )
    log_line(sprintf("WARN: %d measured row(s) could not be converted to meters because altitude/sensor inputs are missing.", sum(missing_mpp_rows, na.rm = TRUE)))
  }
  impossible_length <- "TL" %in% names(combined_meters) & !is.na(combined_meters$TL) &
    (combined_meters$TL < 2 | combined_meters$TL > 35)
  width_cols <- intersect(c(tl_w_names, "W.at.eyes", "W.fluke"), names(combined_meters))
  impossible_width <- if (length(width_cols)) {
    rowSums(combined_meters[, width_cols, drop = FALSE] < 0.1 | combined_meters[, width_cols, drop = FALSE] > 10, na.rm = TRUE) > 0
  } else {
    rep(FALSE, nrow(combined_meters))
  }
  if (any(impossible_length | impossible_width, na.rm = TRUE)) {
    combined_meters <- append_warning_column(
      combined_meters,
      "measurement_qa_warnings",
      impossible_length | impossible_width,
      "meter measurements outside expected large-whale range"
    )
    log_line(sprintf("WARN: %d row(s) have meter measurements outside expected large-whale ranges.", sum(impossible_length | impossible_width, na.rm = TRUE)))
  }

  combined_meters <- combined_meters %>% select(any_of(META_ORDER), any_of(MEAS_ORDER), everything())
  combined_meters_file <- file.path(season_directory, sprintf("%s_%s_Combined_Photos_Measured_Meters.csv", season_name, year))
  backup_if_exists(combined_meters_file)
  readr::write_csv(strip_qa_warnings_column(combined_meters), combined_meters_file)
  log_line(sprintf("Wrote %s rows to: %s", nrow(combined_meters), combined_meters_file))
  log_line(sprintf("LOG saved to: %s", log_file))

  invisible(list(pixels = combined_pixels_file, meters = combined_meters_file))
}
