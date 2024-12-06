library(shiny)
library(shinyjs)
library(leaflet)

# Source the UI and server files
source("SCEG_energy_boxes_shiny_app_ui.R")
source("SCEG_energy_boxes_shiny_app_server.R")

# Run the application
shinyApp(ui = ui, server = server)
