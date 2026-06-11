# Source UI and server
source('global.R', local = TRUE)$value

# Run the app
shinyApp(ui = ui, server = server)
