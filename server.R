# Load the necessary functions from the 'functions' folder
source("./functions/process_flight_data.R", local = TRUE)$value
source("./functions/generate_file_structure.R", local = TRUE)$value
source("./functions/timecheck.R", local = TRUE)$value
source("./functions/split_whaleinfo_rows.R", local = TRUE)$value
source("./functions/process_photos_measured.R", local = TRUE)$value
source("./functions/associate_measurements.R", local = TRUE)$value

server <- function(input, output, session) {
  
  # Define allowed roots for file system
  #roots <- c(UAS_network_drive = "C:/Users/Isaac.Benaka/Desktop/2025 Field Days")
  #roots <- c(UAS_network_drive = "C:/Users/Isaac.Benaka/Desktop/Test Flight Day for Shiny App")
  #roots <- c(UAS_network_drive = '//nefscdata/PSD-UAS/Species_Projects/Whales')
  roots <- c(UAS_network_drive = 'C:/Users/Ike/Desktop/ShinyApp Dev')
  
  shinyDirChoose(input, 'fieldSeasonFolder', roots = roots, defaultRoot = "UAS_network_drive") # For Generate Flight Day Structure
  shinyDirChoose(input, 'directory', roots = roots, defaultRoot = "UAS_network_drive")         # For Image Metadata Processing
  shinyDirChoose(input, "flightDayFolder", roots = roots, defaultRoot = "UAS_network_drive")   # For Photos_Measured Processing
  shinyDirChoose(input, 'measurementDirectory', roots = roots, defaultRoot = "UAS_network_drive") # For Associate Measurements
  
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
    
    # Create the date folder
    if (!dir.exists(date_folder)) {
      dir.create(date_folder)
    }
    
    # Loop through selected platforms and create the required subfolders
    for (platform in input$platforms) {
      
      platform_folder <- switch(
        platform,
        "APH-22"       = "APH-22",
        "APH-28"       = "APH-28",
        "EVO II Pro"   = "EVO II Pro",
        "EVO II Dual"  = "EVO II Dual",
        "FreeFly Astro" = "Astro",
        platform
      )
      
      platform_path <- file.path(date_folder, platform_folder)
      
      if (!dir.exists(platform_path)) {
        dir.create(platform_path)
      }
      
      # Create subfolders based on the platform
      if (platform %in% c("EVO II Pro", "EVO II Dual")) {
        
        dir.create(file.path(platform_path, "jpg"))
        dir.create(file.path(platform_path, "video"))
        dir.create(file.path(platform_path, "EVO_logs"))
        dir.create(file.path(platform_path, "log"))
        
        if (platform == "EVO II Dual") {
          dir.create(file.path(platform_path, "thermal"))
        }
        
      } else if (platform %in% c("APH-22", "APH-28")) {
        
        dir.create(file.path(platform_path, "jpg"))
        
      } else if (platform == "FreeFly Astro") {
        
        dir.create(file.path(platform_path, "flights"))
        dir.create(file.path(platform_path, "log"))
        
      }
    }
    
    # Update the status message to inform the user
    output$generationStatus <- renderText(
      paste("File system template generated successfully at:", date_folder)
    )
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
    status_message("Processing started...")
    
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
      input$baro_offset_astro
    )
  })
  
  # Render the status message
  output$status <- renderText({
    status_message()
  })
  
  # TAB 4: EGNO Processing
  original_filename <- reactiveVal()
  processed_data <- reactiveVal()
  
  observeEvent(input$processWhaleInfo, {
    req(input$processedFile)
    
    # Read the uploaded CSV file
    data <- read.csv(input$processedFile$datapath, stringsAsFactors = FALSE)
    
    # Store the original filename
    original_filename(input$processedFile$name)
    
    # Process the data to split rows with multiple whales
    processed_df <- split_whaleinfo_rows(data)
    
    # Update the reactive value
    processed_data(processed_df)
    
    # Notify the user that processing is complete
    showModal(modalDialog(
      title = "Processing Complete",
      "The whale information has been processed successfully. You can view the results below and download the processed data.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  # Render the processed data table
  output$processedData <- DT::renderDataTable({
    req(processed_data())+
      DT::datatable(
        processed_data(),
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = FALSE)
      )
  })
  
  # Provide a download handler for the processed data
  output$downloadProcessedData <- downloadHandler(
    filename = function() {
      # Return the original filename without any changes
      original_filename()
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
  
  # TAB 5: Photos_Measured Processing
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
      process_photos_measured(flight_day_folder)
    })
  })
  
  # Reactive value to store the status message for measurements
  measurement_status_message <- reactiveVal("Processing not started.")
  
  # TAB 6: Associate Measurements
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
    season_directory <- parseDirPath(roots, input$measurementDirectory)
    
    # Update the status message
    measurement_status_message("Processing started...")
    
    # Call the function to associate measurements
    associate_measurements(season_directory, measurement_status_message)
  })
  
  # Render the status message
  output$measurementStatus <- renderText({
    measurement_status_message()
  })
}
