# Load the necessary functions from the 'functions' folder
source("./functions/schema_helpers.R", local = TRUE)$value
source("./functions/process_flight_data.R", local = TRUE)$value
source("./functions/generate_file_structure.R", local = TRUE)$value
source("./functions/timecheck.R", local = TRUE)$value
source("./functions/split_whaleinfo_rows.R", local = TRUE)$value
source("./functions/process_photos_measured.R", local = TRUE)$value
source("./functions/process_video_frames.R", local = TRUE)$value
source("./functions/associate_measurements.R", local = TRUE)$value

server <- function(input, output, session) {
  
  # Define allowed roots for file system
  #roots <- c(UAS_network_drive = "C:/Users/Isaac.Benaka/Desktop/2025 Field Days")
  #roots <- c(UAS_network_drive = "C:/Users/Isaac.Benaka/Desktop/Test Flight Day for Shiny App")
  #roots <- c(UAS_network_drive = '//nefscdata/PSD-UAS/Species_Projects/Whales')
  roots <- c(UAS_network_drive = 'C:/Users/Ike/Desktop/ShinyApp Dev')
  
  shinyDirChoose(input, 'fieldSeasonFolder', roots = roots, defaultRoot = "UAS_network_drive") # For Generate Flight Day Structure
  shinyDirChoose(input, 'directory', roots = roots, defaultRoot = "UAS_network_drive")         # For Image Metadata Processing
  shinyDirChoose(input, 'videoFrameFolder', roots = roots, defaultRoot = "UAS_network_drive")   # For Video Frame Metadata
  shinyDirChoose(input, 'metadataFolder', roots = roots, defaultRoot = "UAS_network_drive")     # For EGNO Processing
  shinyDirChoose(input, "flightDayFolder", roots = roots, defaultRoot = "UAS_network_drive")   # For Photos_Measured Processing
  shinyDirChoose(input, 'measurementDirectory', roots = roots, defaultRoot = "UAS_network_drive") # For flight-day measurement collation
  shinyDirChoose(input, 'seasonMeasurementDirectory', roots = roots, defaultRoot = "UAS_network_drive") # For field-season measurement combination

  checklist_lines <- function(items) {
    paste(
      vapply(items, function(item) {
        status <- if (isTRUE(item$ok)) "READY" else if (isTRUE(item$warning)) "WARNING" else "MISSING"
        paste0(status, ": ", item$message)
      }, character(1)),
      collapse = "\n"
    )
  }
  
  # TAB 1: Observe when the user selects a directory for file system generation
  observeEvent(input$fieldSeasonFolder, {
    if (!is.null(input$fieldSeasonFolder) && length(input$fieldSeasonFolder) > 1) {
      field_season_directory <- parseDirPath(roots, input$fieldSeasonFolder)
      if (!is.null(field_season_directory) && nzchar(field_season_directory)) {
        showModal(modalDialog(
          title = "Confirmation",
          paste("You have selected the field season folder:", field_season_directory),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  # Generate File Structure
  observeEvent(input$generate, {
    req(input$date, input$platforms, input$fieldSeasonFolder)
    
    field_season_folder <- parseDirPath(roots, input$fieldSeasonFolder)
    date_folder <- file.path(field_season_folder, input$date)
    created_folders <- generate_folder_structure(date_folder, input$platforms)
    
    # Update the status message to inform the user
    output$generationStatus <- renderText({
      paste(
        "File system template generated successfully at:",
        date_folder,
        "",
        "Folders created or confirmed:",
        created_folders,
        sep = "\n"
      )
    })
  })
  
  # TAB 2: Check Time on EVO Image functionality
  check_time_evo_images(input, output, session)
  
  # TAB 3: Observe when the user selects a directory for flight data processing
  observeEvent(input$directory, {
    if (!is.null(input$directory) && length(input$directory) > 1) {
      flight_date_directory <- parseDirPath(roots, input$directory)
      if (!is.null(flight_date_directory) && nzchar(flight_date_directory)) {
        showModal(modalDialog(
          title = "Confirmation",
          paste("You have selected the directory:", flight_date_directory),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  # Reactive value to store the status messages
  status_message <- reactiveVal("Processing not started.")
  
  # Flight data processing
  observeEvent(input$process, {
    req(input$directory)
    flight_date_directory <- parseDirPath(roots, input$directory)
    
    # Update the status message
    astro_dir <- file.path(flight_date_directory, "Astro")
    checklist <- list(
      list(ok = dir.exists(flight_date_directory), message = paste("flight-day folder:", flight_date_directory)),
      list(ok = nzchar(input$permit), warning = TRUE, message = "permit entered"),
      list(ok = nzchar(input$species), warning = TRUE, message = "species entered"),
      list(ok = nzchar(input$pilot), warning = TRUE, message = "pilot entered")
    )
    if (dir.exists(astro_dir)) {
      astro_image_dir <- file.path(astro_dir, if (identical(input$astro_image_source, "corrected")) "jpg_corr" else "jpg")
      checklist <- c(checklist, list(list(
        ok = dir.exists(astro_image_dir) && length(list.files(astro_image_dir, pattern = "\\.jpe?g$", recursive = TRUE, ignore.case = TRUE)) > 0,
        message = paste("Astro image source has image files:", astro_image_dir)
      )))
    }
    status_message(paste("Input checklist:", checklist_lines(checklist), "Processing started...", sep = "\n"))

    # Refresh processing functions so iterative fixes are picked up without a full app restart.
    source("./functions/process_flight_data.R", local = TRUE)$value
    
    # Call the process_flight_data function, passing the status_message reactiveVal
    process_flight_data(
      flight_date_directory,
      input$timeoff_pro,
      input$timeoff_dual,
      input$permit,
      input$species,
      input$pilot,
      status_message,
      input$baro_offset_pro,
      input$baro_offset_dual,
      input$baro_offset_aph,
      input$baro_offset_astro,
      input$astro_image_source
    )

    video_frame_messages <- prepare_video_frame_folders_for_flight_day(flight_date_directory)
    if (length(video_frame_messages)) {
      status_message(paste(status_message(), paste(video_frame_messages, collapse = "\n"), sep = "\n"))
    }
  })
  
  # Render the status message
  output$status <- renderText({
    status_message()
  })

  # TAB 4: Video Frame Metadata Processing
  video_frame_status <- reactiveVal("Processing not started.")

  observeEvent(input$processVideoFrames, {
    req(input$videoFrameFolder)

    video_frame_folder <- parseDirPath(roots, input$videoFrameFolder)

    if (!dir.exists(video_frame_folder)) {
      showModal(modalDialog(
        title = "Error",
        paste("The selected folder does not exist:", video_frame_folder),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    astro_dir <- if (basename(video_frame_folder) == "Astro") video_frame_folder else file.path(video_frame_folder, "Astro")
    checklist <- list(list(ok = dir.exists(video_frame_folder), message = paste("selected folder:", video_frame_folder)))
    if (dir.exists(astro_dir)) {
      astro_video_dir <- file.path(astro_dir, if (identical(input$astro_video_source, "corrected")) "video_corr" else "video")
      checklist <- c(checklist, list(list(
        ok = dir.exists(astro_video_dir) && length(list.files(astro_video_dir, pattern = "\\.(mp4|mov|m4v)$", recursive = TRUE, ignore.case = TRUE)) > 0,
        message = paste("Astro video source has video files:", astro_video_dir)
      )))
    }
    video_frame_status(paste("Input checklist:", checklist_lines(checklist), "Processing started...", sep = "\n"))
    video_frame_status(paste(video_frame_status(), process_video_frames_for_flight_day(video_frame_folder, input$astro_video_source), sep = "\n"))
  })

  output$videoFrameStatus <- renderText({
    video_frame_status()
  })

  # TAB 5: Whale ID / EGNO Processing
  processed_data <- reactiveVal()
  whaleinfo_status <- reactiveVal("Processing not started.")
  
  observeEvent(input$processWhaleInfo, {
    req(input$metadataFolder)

    metadata_folder <- parseDirPath(roots, input$metadataFolder)

    if (!dir.exists(metadata_folder)) {
      showModal(modalDialog(
        title = "Error",
        paste("The selected folder does not exist:", metadata_folder),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    result <- split_whaleinfo_metadata_folder(metadata_folder)
    processed_data(result$data)
    whaleinfo_status(paste(result$status, collapse = "\n"))
    
    # Notify the user that processing is complete
    showModal(modalDialog(
      title = "Processing Complete",
      "Whale IDs were split across discovered metadata files. Backups were created before overwriting.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  # Render the processed data table
  output$processedData <- DT::renderDataTable({
    req(processed_data())
      DT::datatable(
        processed_data(),
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = FALSE)
      )
  })

  output$whaleInfoStatus <- renderText({
    whaleinfo_status()
  })

  # TAB 6: Photos_Measured Processing
  observeEvent(input$processPhotosMeasured, {
    req(input$flightDayFolder)
    
    # Parse the selected flight day folder path
    flight_day_folder <- parseDirPath(roots, input$flightDayFolder)
    
    # Check if the folder exists
    if (!dir.exists(flight_day_folder)) {
      showModal(modalDialog(
        title = "Error",
        paste("The selected folder does not exist:", flight_day_folder),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Start processing
            output$photosMeasuredStatus <- renderText({
                platform_dirs <- list.dirs(flight_day_folder, recursive = FALSE, full.names = TRUE)
                metadata_files <- unlist(lapply(platform_dirs, function(d) {
                    list.files(d, pattern = "_(imgdata|video_frames)\\.csv$", full.names = TRUE, ignore.case = TRUE)
                }))
                checklist <- checklist_lines(list(
                    list(ok = dir.exists(flight_day_folder), message = paste("flight-day folder:", flight_day_folder)),
                    list(ok = length(metadata_files) > 0, message = paste("metadata CSV files found:", length(metadata_files)))
                ))
                paste("Input checklist:", checklist, "Processing started...", process_photos_measured(flight_day_folder), sep = "\n")
            })
  })
  
  # Reactive value to store the status message for measurements
  measurement_status_message <- reactiveVal("Processing not started.")
  
  # TAB 7: Collate Flight Day Measurements
  observeEvent(input$measurementDirectory, {
    if (!is.null(input$measurementDirectory) && length(input$measurementDirectory) > 1) {
      measurement_directory <- parseDirPath(roots, input$measurementDirectory)
      if (!is.null(measurement_directory) && nzchar(measurement_directory)) {
        showModal(modalDialog(
          title = "Confirmation",
          paste("You have selected the directory:", measurement_directory),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  observeEvent(input$associateMeasurements, {
    req(input$measurementDirectory)
    flight_day_directory <- parseDirPath(roots, input$measurementDirectory)
    
    # Update the status message
    platform_dirs <- list.dirs(flight_day_directory, recursive = FALSE, full.names = TRUE)
    photos_dirs <- file.path(platform_dirs, "Photos_Measured")
    checklist <- checklist_lines(list(
      list(ok = dir.exists(flight_day_directory), message = paste("flight-day folder:", flight_day_directory)),
      list(ok = any(dir.exists(photos_dirs)), message = "at least one platform Photos_Measured folder exists")
    ))
    measurement_status_message(paste("Input checklist:", checklist, "Processing started...", sep = "\n"))

    # Refresh measurement association code so iterative fixes are picked up without a full app restart.
    source("./functions/associate_measurements.R", local = TRUE)$value

    # Collate measurement CSVs only for the selected flight day. Season-level
    # combination is intentionally handled by a separate workflow.
    collate_measurements_for_flight_day(flight_day_directory, measurement_status_message)
  })
  
  # Render the status message
  output$measurementStatus <- renderText({
    measurement_status_message()
  })

  # TAB 8: Combine Field Season Measurements
  season_measurement_status <- reactiveVal("Processing not started.")

  observeEvent(input$seasonMeasurementDirectory, {
    if (!is.null(input$seasonMeasurementDirectory) && length(input$seasonMeasurementDirectory) > 1) {
      season_measurement_directory <- parseDirPath(roots, input$seasonMeasurementDirectory)
      if (!is.null(season_measurement_directory) && nzchar(season_measurement_directory)) {
        showModal(modalDialog(
          title = "Confirmation",
          paste("You have selected the field season directory:", season_measurement_directory),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })

  observeEvent(input$combineSeasonMeasurements, {
    req(input$seasonMeasurementDirectory)
    season_directory <- parseDirPath(roots, input$seasonMeasurementDirectory)

    if (!dir.exists(season_directory)) {
      showModal(modalDialog(
        title = "Error",
        paste("The selected folder does not exist:", season_directory),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    flight_days <- list.dirs(season_directory, recursive = FALSE, full.names = TRUE)
    flight_days <- flight_days[grepl("^\\d{8}$", basename(flight_days))]
    checklist <- checklist_lines(list(
      list(ok = dir.exists(season_directory), message = paste("field-season folder:", season_directory)),
      list(ok = length(flight_days) > 0, message = paste("date-like flight-day folders found:", length(flight_days)))
    ))
    season_measurement_status(paste("Input checklist:", checklist, "Processing started...", sep = "\n"))
    source("./functions/associate_measurements.R", local = TRUE)$value
    combine_field_season_measurements(season_directory, season_measurement_status)
  })

  output$seasonMeasurementStatus <- renderText({
    season_measurement_status()
  })
}
