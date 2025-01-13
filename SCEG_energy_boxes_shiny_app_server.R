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

# load overall map from separate file
source("overall_map.R")

# Shiny server to upload and log photos into database
# Define server logic
server <- function(input, output, session) {
  # Load and process existing data
  output_dir <- "Photos/JPEGs"
  photo_directory <- "Photos"
  preview_directory <- "www/previews"
  updated_dataset_path <- "updated_dataset.csv"
  
  if (!dir.exists(photo_directory)) {
    stop("Photo directory does not exist.")
  }
  
  if (!dir.exists(preview_directory)) {
    dir.create(preview_directory, recursive = TRUE)
  }
  
  if (file.exists(updated_dataset_path)) {
    all_data <- read.csv(updated_dataset_path, stringsAsFactors = FALSE)
    all_data$box_type <- as.factor(all_data$box_type)
    all_data$box_type[is.na(all_data$box_type)] <- "other"  # Replace 'NA' with 'other'
  } else {
    input_paths <- list.files(photo_directory, full.names = TRUE, recursive = TRUE)
    
    if (length(input_paths) == 0) {
      stop("No files found in the photo directory.")
    }
    
    exif_data <- exifr::read_exif(input_paths, tags = c("FileName", "GPSLatitude", "GPSLatitudeRef", "GPSLongitude", "GPSLongitudeRef"))
    
    if (!all(c("GPSLatitude", "GPSLongitude") %in% names(exif_data))) {
      stop("EXIF data does not contain GPS information.")
    }
    
    gps_data <- exif_data %>%
      filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) %>%
      select(SourceFile, FileName, GPSLatitude, GPSLongitude) %>%
      mutate(image_number = sub("\\.HEIC$", "", FileName)) %>%
      rename("latitude" = GPSLatitude, "longitude" = "GPSLongitude")
    
    survey_data <- read_excel("SCEG_Energy_Box_Register.xlsx") %>%
      select("Location", "Code Number", "Photo filename", "Box Type") %>%
      rename("Photo" = "Photo filename", 
             "label_number" = "Code Number", 
             "box_type" = "Box Type") %>%
      mutate("photo_filename" = paste0(Photo, ".jpg"))
    
    all_data <- left_join(survey_data, gps_data, by = c("Photo" = "image_number")) %>%
      mutate(Photo_src = file.path(output_dir, paste0(photo_filename)),
             box_type = as.factor(box_type)) %>%
      select(Location, label_number, box_type, Photo_src, latitude, longitude, photo_filename)
    
    all_data$box_type[is.na(all_data$box_type)] <- "other"  # Replace 'NA' with 'other'
  }
  
  # Render example images
  output$example_type1 <- renderImage({
    list(
      src = "www/example_type1.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$example_type2 <- renderImage({
    list(
      src = "www/example_type2.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$example_type3 <- renderImage({
    list(
      src = "www/example_type3.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$example_colourful <- renderImage({
    list(
      src = "www/example_colourful.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  # Render example images
  output$example_box1 <- renderImage({
    list(
      src = "www/example_type1.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$example_box2 <- renderImage({
    list(
      src = "www/example_type2.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$example_box3 <- renderImage({
    list(
      src = "www/example_type3.jpg",
      contentType = 'image/jpeg',
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  # Observe photo upload
  observeEvent(input$photo, {
    req(input$photo)
    print("Photo uploaded")
    print(input$photo$name)
    
    # Save the uploaded photo
    photo_path <- file.path(photo_directory, input$photo$name)
    file.copy(input$photo$datapath, photo_path)
    
    # Convert HEIC to JPG for preview
    if (grepl("\\.heic$", input$photo$name, ignore.case = TRUE)) {
      img <- image_read(photo_path)
      jpg_path <- file.path(preview_directory, sub("\\.heic$", ".jpg", input$photo$name, ignore.case = TRUE))
      image_write(img, jpg_path, format = "jpg")
      preview_path <- jpg_path
    } else {
      preview_path <- file.path(preview_directory, input$photo$name)
      file.copy(photo_path, preview_path)
    }
    
    print(paste("Preview path:", preview_path))
    
    # Process the new photo with read_exif
    new_exif_data <- exif_read(photo_path, tags = c("SourceFile", "FileName", "GPSLatitude", "GPSLatitudeRef", "GPSLongitude", "GPSLongitudeRef"))
    
    if (!all(c("GPSLatitude", "GPSLongitude") %in% names(new_exif_data))) {
      showNotification("No GPS data found in the uploaded photo.", type = "error")
      return()
    }
    
    new_gps_data <- new_exif_data %>%
      filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) %>%
      select(SourceFile, FileName, GPSLatitude, GPSLongitude) %>%
      rename("latitude" = GPSLatitude, "longitude" = "GPSLongitude")
    
    if (nrow(new_gps_data) == 0) {
      showNotification("No GPS data found in the uploaded photo.", type = "error")
      return()
    }
    
    # Render preview map with the new photo location (include existing photo locations to avoid duplication)
    output$preview_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("OpenStreetMap") %>%
        leaflet::setView(new_gps_data$longitude[1], new_gps_data$latitude[1], zoom = 15) %>%
        leaflet::addCircleMarkers(
          lng = all_data$longitude,
          lat = all_data$latitude,
          color = "blue",
          radius = 2,
          opacity = 1,
          stroke = TRUE,
          weight = 5
        ) %>%
        leaflet::addCircleMarkers(
          lng = new_gps_data$longitude[1],
          lat = new_gps_data$latitude[1],
          color = "red",
          radius = 1,
          opacity = 1,
          stroke = TRUE,
          weight = 4
        )
    })
    
    # Render photo preview
    output$photo_preview <- renderImage({
      list(
        src = preview_path,
        contentType = 'image/jpeg',
        width = "75%"
      )
    }, deleteFile = FALSE)
    
    # Show photo preview and map preview
    shinyjs::show("photo_preview")
    shinyjs::show("preview_title")
    shinyjs::show("preview_map")
    shinyjs::hide("Step 1")
    shinyjs::show("Step 2")
    shinyjs::show("example_images_hidden")
    shinyjs::show("survey_run_number")
  })

  # Observe submit button
  observeEvent(input$submit, {
    req(input$photo)
    req(input$survey_run_number)
    print("Submitted to database")
    
    # Save the uploaded photo
    photo_path <- file.path(photo_directory, input$photo$name)
    file.copy(input$photo$datapath, photo_path)
    
    # Convert HEIC to JPG for storage
    if (grepl("\\.heic$", input$photo$name, ignore.case = TRUE)) {
      img <- image_read(photo_path)
      base_name <- tools::file_path_sans_ext(basename(photo_path))
      jpg_path <- file.path(output_dir, paste0(base_name, "_", survey_run_number, ".jpg"))
      jpg_path <- file.path(output_dir, sub("\\.heic$", ".jpg", input$photo$name, ignore.case = TRUE))
      image_write(img, jpg_path, format = "jpg")
      final_path <- jpg_path
    } else {
      final_path <- file.path(output_dir, input$photo$name)
      file.copy(photo_path, final_path)
    }
    
    # Process the new photo with read_exif
    new_exif_data <- exif_read(photo_path, tags = c("FileName", "GPSLatitude", "GPSLatitudeRef", "GPSLongitude", "GPSLongitudeRef"))
    
    if (!all(c("GPSLatitude", "GPSLongitude") %in% names(new_exif_data))) {
      showNotification("No GPS data found in the uploaded photo.", type = "error")
      return()
    }
    
    new_gps_data <- new_exif_data %>%
      filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) %>%
      select(SourceFile, FileName, GPSLatitude, GPSLongitude) %>%
      mutate(image_number = sub("\\.jpg$", "", FileName)) %>%
      rename("latitude" = GPSLatitude, "longitude" = "GPSLongitude")
    
    if (nrow(new_gps_data) == 0) {
      showNotification("No GPS data found in the uploaded photo.", type = "error")
      return()
    }
    
    # Add new entry to the data
    new_entry <- data.frame(
      Location = input$location,
      label_number = input$code,
      Photo = sub("\\.jpg$", "", input$photo$name),
      box_type = input$box_type,
      Photo_src = file.path(output_dir, basename(final_path)),
      latitude = new_gps_data$latitude[1],
      longitude = new_gps_data$longitude[1],
      stringsAsFactors = FALSE
    )
    
    all_data <<- bind_rows(all_data, new_entry)
    
    # refresh output map
    output$overall_map <- renderLeaflet({
      create_overall_map(all_data)
    })
    
     # refresh data table
    output$data <- renderTable({
      all_data %>%
        select(-c(Photo,latitude, longitude, photo_filename)) %>%
        mutate(box_type = as.factor(box_type))
    })
    
    # Save the updated dataset to an external file
    write.csv(all_data, updated_dataset_path, row.names = FALSE)
    
    # Hide instructions
    shinyjs::hide("Step 2")
    
    # Hide the preview map and photo preview
    shinyjs::hide("preview_map")
    shinyjs::hide("photo_preview")
    shinyjs::hide("preview_title")
    
    # Hide the submit button and show the add another button
    shinyjs::hide("submit")
    shinyjs::show("add_another")
    
    # hide the inputs
    shinyjs::hide("photo")
    shinyjs::hide("location")
    shinyjs::hide("box_type")
    shinyjs::hide("code")
  })
  
  # Render data table
  # Render the full data table with hyperlinks
  output$full_data_table <- renderDT({
    datatable(all_data(), escape = FALSE, options = list(pageLength = 10)) %>%
      formatStyle('Google_Photos_Link', target = 'row', 
                  backgroundColor = styleEqual(c("https://photos.google.com/search/"), c('lightblue')))
  })
  output$data <- renderTable({
    all_data %>%
      select(-c(Photo,latitude, longitude, photo_filename)) %>%
      mutate(box_type = as.factor(box_type))
  })
  
  # Observe add another button
  observeEvent(input$add_another, {
    # Reset input fields
    updateTextInput(session, "location", value = "")
    updateSelectInput(session, "box_type", selected = "NA")
    updateTextInput(session, "code", value = "")
    shinyjs::reset("photo")
    
    # Hide the preview map and photo preview
    shinyjs::hide("preview_map")
    shinyjs::hide("photo_preview")
    shinyjs::hide("preview_title")
    shinyjs::hide("example_images_hidden")
    
    # Show the inputs
    shinyjs::show("photo")
    shinyjs::show("location")
    shinyjs::show("box_type")
    shinyjs::show("code")
    
    # Show the submit button and hide the add another button
    shinyjs::show("submit")
    shinyjs::hide("add_another")
  })
  
  # Overall map
  output$overall_map <- renderLeaflet({
    create_overall_map(all_data)
  })
}

