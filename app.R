# library(shiny)
# library(shinydashboard)

# Source the UI and server files
source("global.R")
source("app_ui.R")
source("app_server.R")

# Define the Shiny app using the `shinyApp()` function
shinyApp(
  ui = app_ui,   # ui object is sourced from app_ui.R
  server = app_server # server function is sourced from app_server.R
)