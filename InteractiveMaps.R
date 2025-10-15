# Load required libraries
library(mapview)
library(leafpop)
library(sf)
library(viridis)

# Filter to national protected areas
nat_areas <- prot.areas[prot.areas$type == "National", ]

# Create a custom popup content with multiple fields
# Assuming your nat_areas has columns like: SITENAME, area_km2, obs.per.km2, designation_year, etc.

# Create HTML popup content
nat_areas$popup_content <- paste0(
  "<div style='font-family: Arial, sans-serif; max-width: 300px;'>",
  "<h3 style='color: #2c5c3f; margin-bottom: 10px;'>", nat_areas$SITENAME, "</h3>",
  "<hr style='border: 1px solid #e0e0e0; margin: 10px 0;'>",
  "<p><strong>Type:</strong> ", nat_areas$type, "</p>",
  "<p><strong>Observations per km²:</strong> ", round(nat_areas$obs.per.km2, 2), "</p>",
  if("area_km2" %in% names(nat_areas)) 
    paste0("<p><strong>Area:</strong> ", round(nat_areas$area_km2, 2), " km²</p>"),
  if("designation_year" %in% names(nat_areas))
    paste0("<p><strong>Designation Year:</strong> ", nat_areas$designation_year, "</p>"),
  if("management" %in% names(nat_areas))
    paste0("<p><strong>Management:</strong> ", nat_areas$management, "</p>"),
  "<p style='font-size: 0.9em; color: #666; margin-top: 15px;'>Click for more details</p>",
  "</div>"
)

# Create the map with enhanced popups
m <-   mapview(
  lux_borders_2169,
  color = "black",
  alpha.regions = 0,
  lwd = 2,
  layer.name = "Luxembourg Borders"
) +
  mapview(
  nat_areas,
  zcol = "obs.per.km2",          # column controlling fill color
  col.regions = viridis(256, option = "plasma"),
  at = pretty(range(nat_areas$obs.per.km2, na.rm = TRUE)),
  layer.name = "Observations per km²",
  alpha.regions = 0.7,
  legend = TRUE,
  popup = leafpop::popupTable(    # Enhanced popup with table format
    nat_areas,
    zcol = c("SITENAME", "obs.per.km2", "type"),  # Select columns to display
    feature.id = FALSE,
    row.numbers = FALSE
  ),
  label = nat_areas$SITENAME,     # Shows name on hover
  highlight = leaflet::highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 0.9,
    bringToFront = TRUE
  )
)

# Display the map
m

################################################################################

# Color palette
pal <- colorNumeric(viridis(256, option = "plasma"), nat_areas$obs.per.km2, na.color = "transparent")

# Create leaflet map
m <- leaflet() %>%
  # Base layers
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
  
  # Protected areas polygons
  addPolygons(
    data = nat_areas,
    fillColor = ~pal(obs.per.km2),
    fillOpacity = 0.8,
    color = "black",
    weight = 1,
    popup = ~SITENAME,
    group = "Protected Areas"
  ) %>%
  
  # Borders
  addPolygons(
    data = lux_borders_2169,
    fillOpacity = 0,
    color = "black",
    weight = 1,
    group = "Lux Borders"
  ) %>%
  
  # Layer control
  addLayersControl(
    baseGroups = c("OSM", "ESRI Satellite"),
    overlayGroups = c("Protected Areas", "Observations", "Lux Borders"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save as self-contained HTML
library(htmlwidgets)
saveWidget(m, "lux_map.html", selfcontained = TRUE)
