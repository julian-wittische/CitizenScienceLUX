######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : Store all homemade functions used in the project
# Functions are not the main product of this project

############ In Luxembourg or not ----
luxornot <- function(coord){
  # Split and convert to numeric (note: sf uses lon, lat order)
  latlon <- as.numeric(strsplit(coord, ",")[[1]])
  point_sf <- st_sf(
    geometry = st_sfc(st_point(c(latlon[2], latlon[1])), crs = 4326)
  )
  
  return(c(st_within(point_sf, lux_borders, sparse = FALSE)))
}