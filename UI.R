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
    "))
  ),
  
  # Header with image and title
  tags$div(
    class = "title-logo-container",
    tags$img(src = "NOAA_FISHERIES_LOGO.png", height = "60px"),
    tags$h1("NEFSC UAS Imagery Processing App")
  ),
  
  tabsetPanel(
    tabPanel("Generate Flight Day File Structure",
             sidebarLayout(
               sidebarPanel(
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
                 actionButton("process", "Process Data")
               ),
               mainPanel(
                 verbatimTextOutput("status")
               )
             )
    ),
    
    tabPanel("Separate Images By EGNO",
             sidebarLayout(
               sidebarPanel(
                 fileInput("processedFile", "Upload Processed Image Metadata Spreadsheet", accept = c(".csv")),
                 helpText("Please upload the CSV file processed using the Image Metadata Processing tab with EGNO numbers added to the whaleinfo column (e.g. YYYYMMDD_APH_imgdata.csv/YYYYMMDD_EVO_imgdata.csv). Ensure that EGNOs are comma delimited."),
                 actionButton("processWhaleInfo", "Generate Image Metadata For Each Whale"),
                 downloadButton("downloadProcessedData", "Download Processed Data")
               ),
               mainPanel(
                 DT::dataTableOutput("processedData")
               )
             ),
             # Place the data table outside the sidebarLayout to make it full width
             fluidRow(
               column(12,
                      DT::dataTableOutput("processedData")
               )
             )
    ),
    
    tabPanel("Create Photogrammetry File Structure",
             sidebarLayout(
               sidebarPanel(
                 shinyDirButton("flightDayFolder", "Select Flight Day Folder", "Browse"),
                 helpText("Navigate to the flight day folder and select it."),
                 actionButton("processPhotosMeasured", "Generate Photogrammetry File Structure")
               ),
               mainPanel(
                 verbatimTextOutput("photosMeasuredStatus")
               )
             )
    ),
    
    tabPanel("Associate Measurements",
             sidebarLayout(
               sidebarPanel(
                 shinyDirButton("measurementDirectory", "Select Field Season Folder", "Browse"),
                 helpText("Please navigate to the field season folder (e.g., 2024/SEUS) and select it."),
                 actionButton("associateMeasurements", "Associate Measurements")
               ),
               mainPanel(
                 verbatimTextOutput("measurementStatus")
               )
             )
    )
))
