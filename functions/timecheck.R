check_time_evo_images <- function(input, output, session) {
  configure_exiftoolr(
    command = NULL,
    perl_path = "srv/shiny-server/ibenaka/UAS_IMG_Processing_ShinyApp/win_exe/exiftool(-k).exe",
    allow_win_exe = FALSE,
    quiet = FALSE
  )
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile)) return()
    
    # Convert image to base64 for display
    b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/png")
    insertUI(
      selector = "#image-container",
      where = "afterBegin",
      ui = img(src = b64, width = 650, height = 450)
    )
    
    # Extract EXIF data
    exif_data <- tryCatch({
      exif_read(inFile$datapath, tags = c("filename", "datetimeoriginal"))
    }, error = function(e) {
      NULL
    })
    
    if (is.null(exif_data)) {
      output$txt_fileT <- renderText("Failed to read EXIF data")
    } else {
      output$txt_fileT <- renderText(exif_data$DateTimeOriginal)
    }
  })
}
