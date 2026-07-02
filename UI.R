instructionBox <- function(title, ...) {
  tags$div(
    class = "well well-sm workflow-help",
    tags$strong(title),
    ...
  )
}

# Define UI for the app
ui <- fluidPage(
  # Add custom CSS to style the header
  tags$head(
    tags$style(HTML("
      .title-logo-container {
        display: flex;
        align-items: center;
        justify-content: center; /* Centers the content horizontally */
        margin-bottom: 20px; /* Adds space below the header */
      }
      .title-logo-container img {
        margin-right: 20px; /* Adjusts space between logo and title */
      }
      .title-logo-container h1 {
        margin: 0; /* Removes default margins from the title */
      }
      .workflow-help {
        font-size: 0.92em;
        line-height: 1.35;
        background: #f7fbfd;
        border: 1px solid #d6e4ec;
        border-left: 4px solid #2c7fb8;
        box-shadow: none;
      }
      .workflow-help strong {
        display: block;
        margin-bottom: 6px;
        color: #20323f;
      }
      .workflow-help ol, .workflow-help ul {
        padding-left: 18px;
        margin-bottom: 4px;
      }
      .workflow-overview {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 6px;
        margin: -6px 0 16px 0;
      }
      .workflow-step {
        padding: 6px 9px;
        border: 1px solid #c9d6df;
        border-left: 4px solid #2c7fb8;
        border-radius: 4px;
        background: #fbfdfe;
        color: #20323f;
        font-size: 12px;
        line-height: 1.2;
      }
    "))
  ),
  
  # Header with image and title
  tags$div(
    class = "title-logo-container",
    tags$img(src = "NOAA_FISHERIES_LOGO.png", height = "60px"),
    tags$h1("NEFSC UAS Imagery Processing App")
  ),

  tags$div(
    class = "workflow-overview",
    tags$span(class = "workflow-step", "1. Folders"),
    tags$span(class = "workflow-step", "2. Time Check"),
    tags$span(class = "workflow-step", "3. Metadata"),
    tags$span(class = "workflow-step", "4. Video Frames"),
    tags$span(class = "workflow-step", "5. Whale IDs"),
    tags$span(class = "workflow-step", "6. Photos Measured"),
    tags$span(class = "workflow-step", "7. Flight-Day Measurements"),
    tags$span(class = "workflow-step", "8. Season Combine")
  ),
  
  tabsetPanel(
    tabPanel("Generate Flight Day File Structure",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Create or select the field-season folder where this flight day should live."),
                     tags$li("Confirm the flight date in YYYYMMDD format."),
                     tags$li("Select only the platforms flown on that date."),
                     tags$li("After the folders are generated, add the raw files and data collected during the flight day into the respective platform folders.")
                   ),
                   tags$p("APH platform folder: still images -> /jpg. Paste YYYYMMDD folder containing GPX and KML files into the main APH folder"),
                   tags$p("EVO platform folder: still images -> jpg folder, video files -> /video folder, EVO AirData log files -> EVO_logs, LiDARBoX/fragile altimeter log files -> /log folder."),
                   tags$p("Astro platform folder: uncorrected stills from SD card -> /jpg/card folders, uncorrected stills from Samsung thumbdrive -> /jpg/thumbdrive, lens distortion corrected stills -> /jpg_corr, video files -> /video/card## folders, lens distortion corrected videos -> /video_corr/card##, Drone Amplified photo_info logs -> /log, .ulg flight logs: /flight_logs."),
                   tags$p("VLC frame exports go in the platform video_frames folder after videos have been reviewed."),
                   tags$p("This step creates folders only; it does not move, rename, or modify raw files.")
                 ),
                 shinyDirButton("fieldSeasonFolder", "Select Field Season Folder", "Browse"),
                 helpText("Please navigate to the current field season and select it (e.g. Whales/2024/CCB)."),
                 textInput("date", "Enter Flight Date (YYYYMMDD)", value = ""),
                 checkboxGroupInput("platforms", "Select Drone Platforms Used", 
                                    choices = list("APH-22" = "APH-22", 
                                                   "APH-28" = "APH-28", 
                                                   "EVO II Pro" = "EVO II Pro", 
                                                   "EVO II Dual" = "EVO II Dual", 
                                                   "FreeFly Astro" = "FreeFly Astro")),
                 actionButton("generate", "Generate File System")
               ),
               mainPanel(
                 verbatimTextOutput("generationStatus")
               )
             )
    ),
    
    tabPanel("Check Time on EVO Image", 
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Use this when an EVO image of a GPS clock or other GPS time reference was collected."),
                     tags$li("The tab extracts the image EXIF timestamp so it can be compared with the GPS time shown in the image."),
                     tags$li("Use the difference as the EVO time offset in Image Metadata Processing.")
                   ),
                   tags$p("This offset is needed when camera EXIF time and GPS/log time are not synchronized.")
                 ),
                 fluidRow(
                   fileInput("myFile", "Choose an image", accept = c('image/*', '.jpeg', '.jpg', '.png'))
                 )
               ),
               mainPanel(
                 div(id="image-container", style = "display:flexbox"),
                 textOutput("txt_fileT")
               )
             )
    ),
    
    tabPanel("Image Metadata Processing",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Run Generate Flight Day File Structure and place the day files into the platform folders first."),
                     tags$li("Enter permit, species, pilot, time offsets, and barometric altitude offsets for the flight day/platform."),
                     tags$li("For corrected Astro imagery, select jpg_corr and review the pixel-dimension warning.")
                   ),
                   tags$p("This writes platform *_imgdata.csv files containing image filename, image number, datetime/time, flight number, platform, pilot, permit, species, whaleinfo, position, altitude, tilt, sensor fields, pixel dimension, and QA warnings where applicable."),
                   tags$p("For EVO video workflows, this also supports existing video metadata products used later for video-frame altitude assignment and creates or reports video_frames folders when video is detected.")
                 ),
                 shinyDirButton("directory", "Select Flight Date Folder", "Browse"),
                 helpText("Please navigate to the specific flight day folder (e.g., 20240202) and select it."),
                 textInput("permit", "Permit Number", value = "", placeholder = 27066),
                 textInput("species", label = "Species Code", value = "", placeholder = "Eg, Bp, etc."),
                 textInput("pilot", label = "Pilot Name", value = "", placeholder = "Two/three letter initial"),
                 numericInput("timeoff_pro", "Time Offset for EVO II Pro (seconds)", value = 0),
                 numericInput("timeoff_dual", "Time Offset for EVO II Dual (seconds)", value = 0),
                 numericInput("baro_offset_pro", "Barometric Altitude Offset for EVO II Pro (m)", value = 0),
                 numericInput("baro_offset_dual", "Barometric Altitude Offset for EVO II Dual (m)", value = 0),
                 numericInput("baro_offset_aph", "Barometric Altitude Offset for APH-22 (m)", value = 0),
                 numericInput("baro_offset_astro", "Barometric Altitude Offset for Astro (m)", value = 0),
                 radioButtons(
                   "astro_image_source",
                   "Astro still image source",
                   choices = c("Uncorrected: Astro/jpg" = "uncorrected", "Corrected: Astro/jpg_corr" = "corrected"),
                   selected = "uncorrected"
                 ),
                 actionButton("process", "Process Data")
               ),
               mainPanel(
                 verbatimTextOutput("status")
               )
             )
    ),

    tabPanel("Refresh Video Frame Metadata",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Run Image Metadata Processing first so platform context is available."),
                     tags$li("Use VLC to export selected video frames into the platform video_frames folder."),
                     tags$li("For Astro with multiple cards, export frames under video_frames/card01, video_frames/card02, etc. If the Drone Amplified flight is known, use a flight subfolder like video_frames/card01/f2."),
                     tags$li("For corrected Astro video, select video_corr and confirm corrected video filenames end in _corr.")
                   ),
                   tags$p("Select the flight-day folder or platform folder, then click Refresh Video Frame Metadata. This creates or refreshes *_video_inventory.csv and *_video_frames.csv."),
                   tags$p("After *_video_frames.csv exists, assign whaleinfo for both still images and video frames before splitting whale IDs.")
                 ),
                 shinyDirButton("videoFrameFolder", "Select Flight Day or Platform Folder", "Browse"),
                 radioButtons(
                   "astro_video_source",
                   "Astro video source for inventory",
                   choices = c("Uncorrected: Astro/video" = "uncorrected", "Corrected: Astro/video_corr" = "corrected"),
                   selected = "uncorrected"
                 ),
                 actionButton("processVideoFrames", "Refresh Video Frame Metadata")
               ),
               mainPanel(
                 verbatimTextOutput("videoFrameStatus")
               )
             )
    ),
    
    tabPanel("Assign Whale IDs / EGNO",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Run Image Metadata Processing first so *_imgdata.csv exists."),
                     tags$li("If video frames were extracted, run Refresh Video Frame Metadata first so *_video_frames.csv exists."),
                     tags$li("Fill whaleinfo in both metadata files before splitting. whaleinfo should contain the names or IDs of the whales featured in each image or frame."),
                     tags$li("Use comma or semicolon separated whale names/IDs when multiple whales occur in one image or frame."),
                     tags$li("Click Split Whale IDs Across Metadata Files after whaleinfo is filled in.")
                   ),
                   tags$p("This creates one row per whale/EGNO while preserving the still-image and video-frame metadata files separately.")
                 ),
                 shinyDirButton("metadataFolder", "Select Flight Day or Platform Folder", "Browse"),
                 actionButton("processWhaleInfo", "Split Whale IDs Across Metadata Files")
               ),
               mainPanel(
                 verbatimTextOutput("whaleInfoStatus"),
                 DT::dataTableOutput("processedData")
               )
             )
    ),

    tabPanel("Create Photogrammetry File Structure",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Split whale IDs so each selected still/frame has a single EGNO row."),
                     tags$li("Assign photogrammetry grades in the metadata CSVs or in Excel."),
                     tags$li("Only rows with positive photogram_quality will be copied into Photos_Measured.")
                   ),
                   tags$p("This creates the platform Photos_Measured folder, which is the working home for measurement-ready imagery."),
                   tags$p("Inside Photos_Measured, the app creates EGNO folders and copies graded still images or video frames into the matching EGNO folder for Morphometrix measurement.")
                 ),
                 shinyDirButton("flightDayFolder", "Select Flight Day Folder", "Browse"),
                 helpText("Navigate to the flight day folder and select it."),
                 actionButton("processPhotosMeasured", "Generate Photogrammetry File Structure")
               ),
               mainPanel(
                 verbatimTextOutput("photosMeasuredStatus")
               )
             )
    ),
    
    tabPanel("Collate Flight Day Measurements",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Measure copied images/frames in Morphometrix."),
                     tags$li("Save each Morphometrix CSV in the matching Photos_Measured/EGNO folder."),
                     tags$li("Keep the Image Path row in each Morphometrix CSV so the app can match by FileName and EGNO.")
                   ),
                   tags$p("This updates each platform *_photos_measured.csv for the selected flight day by joining Morphometrix pixel measurements back to the selected image/frame metadata."),
                   tags$p("The updated file preserves the metadata fields, adds or updates measurement columns such as TL and width measurements, keeps rows that have not yet been measured, and reports unmatched measurement files or missing conversion inputs.")
                 ),
                 shinyDirButton("measurementDirectory", "Select Flight Day Folder", "Browse"),
                 helpText("Select one flight-day folder. This collates Morphometrix CSVs from each platform's Photos_Measured/EGNO folders back into that platform's photos_measured.csv."),
                 actionButton("associateMeasurements", "Collate Flight Day Measurements")
               ),
               mainPanel(
                 verbatimTextOutput("measurementStatus")
               )
             )
    ),

    tabPanel("Combine Field Season Measurements",
             sidebarLayout(
               sidebarPanel(
                 instructionBox(
                   "Before using this tab",
                   tags$ol(
                     tags$li("Run Collate Flight Day Measurements for each processed flight day."),
                     tags$li("Review measurement_qa_warnings in platform photos_measured files."),
                     tags$li("Select the field-season folder that contains the YYYYMMDD flight-day folders.")
                   ),
                   tags$p("This writes season-level pixel and meter CSVs for final review and analysis.")
                 ),
                 shinyDirButton("seasonMeasurementDirectory", "Select Field Season Folder", "Browse"),
                 helpText("Select the field-season folder containing flight-day folders. This combines existing platform photos_measured.csv files across the season and writes season-level pixel and meter CSVs."),
                 actionButton("combineSeasonMeasurements", "Combine Field Season Measurements")
               ),
               mainPanel(
                 verbatimTextOutput("seasonMeasurementStatus")
               )
             )
    )
))
