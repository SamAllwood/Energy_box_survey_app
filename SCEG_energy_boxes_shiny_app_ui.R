library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(exiftoolr)
library(htmltools)
library(leaflet.extras)
library(magick)
library(shinyjs)
library(shinythemes)
library(exifr)

# Shiny app to upload and log photos into database
# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Energy Box Survey"),
  theme = shinythemes::shinytheme('darkly'),
  tabsetPanel(
    tabPanel("Upload Photo",
      titlePanel("Add Photo to Database"),
      sidebarLayout(
        sidebarPanel(
          h3(id = "Step 1", "Step 1. Upload a photo"),
          fileInput("photo", "Upload Photo", accept = c('image/heic')),
          hidden(tags$h5("Preview:", id = "preview_title")),
          hidden(imageOutput("photo_preview")),
          hidden(h3(id = "Step 2", "Step 2. Enter photo details")),
          textInput("location", "Location", placeholder = "from map (you can zoom in!)"),
          selectInput("box_type", "Box Type", choices = c("NA", "1", "2", "3", "Colourful", "other")),
          textInput("code", "Label Number", placeholder = "if applicable"),
          actionButton("submit", "Add to Dataset"),
          hidden(actionButton("add_another", "Add Another"))
                  ),
        mainPanel(
          leafletOutput("preview_map"),
          tableOutput("data"),
            hidden(tags$div(
              id="example_images_hidden",
                h5("Example images for each box type:"),
                fluidRow(
                  column(3, tags$div(
                    tags$p("Box Type: 1", style = "text-align: center;"),
                    imageOutput("example_type1")
                  )),
                  column(3, tags$div(
                    tags$p("Box Type: 2", style = "text-align: center;"),
                    imageOutput("example_type2")
                    )),
                  column(3, tags$div(
                    tags$p("Box Type: 3", style = "text-align: center;"),
                    imageOutput("example_type3")
                    )),
                  column(3, tags$div(
                    tags$p("Box Type: Colourful", style = "text-align: center; white-space: nowrap;"),
                    imageOutput("example_colourful")
                    )
                  )
                )
              )
            )
          )
        )
      ),
  tabPanel("Overall Map",
    titlePanel("Interactive Map"),
    sidebarLayout(
      sidebarPanel(width = 3,
      tags$div(
        id="example_images",
          tags$p("Box Type: 1", style = "text-align: center;"),
          imageOutput("example_box1"),
          tags$p("Box Type: 2", style = "text-align: center;"),
          imageOutput("example_box2"),
          tags$p("Box Type: 3", style = "text-align: center;"),
          imageOutput("example_box3"),
          )),
      mainPanel(width = 9,
      leafletOutput("overall_map", width = "100%", height = "600px"))
    ))))
