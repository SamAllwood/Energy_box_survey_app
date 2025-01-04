## Leaflet Map to consolidate survey routes 
library(leaflet)
library(geojsonsf)
library(sf)

# Load the survey route data
survey_routes <- st_read("GPX_files/Survey_routes.kml")

# Extract coordinates from the sf object
points <- survey_routes[st_geometry_type(survey_routes) == "POINT", ]

# Define the geographical centre of the map for view-reset purposes
centre <- c(55.903, -4.2906)

# Create a leaflet map
# Create a Leaflet map and add the geometries
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  setView(centre[2], centre[1], zoom = 14.4) %>%
  # Add points
  addCircleMarkers(
    data = points,
    lng = ~st_coordinates(points)[, 1],
    lat = ~st_coordinates(points)[, 2],
    radius = 0.5,
    weight = 0.5,
    color = "blue",
    fill = TRUE,
    fillColor = "blue",
    fillOpacity = 0.7
  ) 
