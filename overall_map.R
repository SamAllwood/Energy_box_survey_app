library(leaflet)

create_overall_map <- function(all_data) {

# Define the geographical centre of the map for view-reset purposes
centre <- c(55.903, -4.2906)
# Define a color palette for the Box Type
box_type_colors <- colorFactor(
  palette = c("blue", "red", "purple", "black"),
  domain = all_data$`box_type`)
#Leaflet map code
leaflet(all_data) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView(centre[2], centre[1], zoom = 15) %>%
  addCircleMarkers(data=(all_data %>% filter(box_type== 1)),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(`box_type`),
                   group = "Type 1",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Photo filename: ", photo_filename, "<br>",
                     popupImage(img=all_data$Photo_src, height = 300)),
                   radius = 2,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 5)%>%
  addCircleMarkers(data=(all_data %>% filter(box_type== 2)),
                   lng = ~longitude, lat = ~latitude,
                   color = ~box_type_colors(box_type),
                   group = "Type 2",
                   popup = ~paste(
                     "Location: ", Location, "<br>",
                     "Code Number: ", label_number, "<br>",
                     "Box Type: ", box_type, "<br>",
                     "Photo filename: ", photo_filename, "<br>",
                     popupImage(img=all_data$Photo_src, height = 300)),
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
                     "Photo filename: ", photo_filename, "<br>",
                     popupImage(img=all_data$Photo_src, 
                                src=local,
                                embed=FALSE,
                                height = 300)),
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
                     "Photo filename: ", photo_filename, "<br>",
                     popupImage(img=all_data$Photo_src, height = 300)),
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
  addEasyButton(easyButton(
    icon = htmltools::span(style = "font-size: 12px; width: auto;", "Reset view"),
    onClick = JS(sprintf("
    function(btn, map) {
      map.closePopup();
      map.setView([%f, %f], 15);
    }
  ", centre[1], centre[2])))) %>%
  addLayersControl(
    overlayGroups = c("Type 1", "Type 2", "Type 3", "Other"),
    options = layersControlOptions(collapsed = FALSE)
  ) 
}