library(leaflet)

# Load all_data from csv file
updated_dataset_path <- "updated_dataset.csv"
all_data <- read.csv(updated_dataset_path, stringsAsFactors = FALSE)
all_data$box_type <- as.factor(all_data$box_type)
all_data$box_type[is.na(all_data$box_type)] <- "other"  # Replace 'NA' with 'other'

# Define the geographical centre of the map for view-reset purposes
centre <- c(55.903, -4.2906)

# Define a color palette for the Box Type
box_type_colors <- colorFactor(
  palette = c("darkorange", "red", "purple", "black"),
  domain = all_data$`box_type`)

#Leaflet map code - popup images do not show the correct photos, can't work out why so have turned them off for now
(leaflet(all_data) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView(centre[2], centre[1], zoom = 14) %>%
  addCircleMarkers(data=(all_data %>% filter(box_type== 1)),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(`box_type`),
                   group = "Type 1",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Source: ", Photo_src, "<br>",
                    # popupImage(img=all_data$Photo_src, height = 300)
                    ),
                   radius = 2,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 5) %>%
  addCircleMarkers(data=(all_data %>% filter(box_type== 2)),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(box_type),
                   group = "Type 2",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Source: ", Photo_src, "<br>",
                   #  popupImage(img=Photo_src, height = 300)
                   ),
                   radius = 2,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 5)%>%
  addCircleMarkers(data=(all_data %>% filter(box_type== 3)),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(box_type),
                   group = "Type 3",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Source: ", Photo_src, "<br>",
                  #   popupImage(img=Photo_src, height = 300)
                  ),
                   radius = 2,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 5)%>%
  addCircleMarkers(data=(all_data %>% filter(!(box_type %in% c(1, 2, 3)))),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(box_type),
                   group = "Other",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Source: ", Photo_src, "<br>",
                  #   popupImage(img=Photo_src, height = 300)
                     ),
                   radius = 2,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 5)%>%
  addLegend(
    "bottomright",
    pal = box_type_colors,
    values = ~box_type,
    title = "Box Type",
    opacity = 1) %>%
   addLayersControl(
    overlayGroups = c("Type 1", "Type 2", "Type 3", "Other"),
    options = layersControlOptions(collapsed = FALSE)
  ) )
