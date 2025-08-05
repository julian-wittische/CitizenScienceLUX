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

############ Circular concentration ----
seasonality <- function(user_df) {
  if (nrow(user_df) == 0 || all(is.na(user_df$observed_on_details.date))) {
    return(data.frame(
      mean_month_deg = NA_real_,
      mean_month_numeric = NA_real_,
      rho = NA_real_
    ))
  }
  
  # Clean and ensure proper Date format, remove NAs
  user_df <- user_df %>%
    mutate(observed_on_details.date = as.Date(observed_on_details.date)) %>%
    filter(!is.na(observed_on_details.date))
  
  if (nrow(user_df) == 0) {
    return(data.frame(
      mean_month_deg = NA_real_,
      mean_month_numeric = NA_real_,
      rho = NA_real_
    ))
  }
  
  month_counts <- user_df %>%
    mutate(month = lubridate::month(observed_on_details.date)) %>%
    count(month)
  
  months_rep <- rep(month_counts$month, month_counts$n)
  
  # Use radians for correct circular math
  circ_months_rad <- circular(
    months_rep * 2 * pi / 12,
    units = "radians",
    template = "clock12",
    type = "angles"
  )
  
  mean_rad <- as.numeric(mean(circ_months_rad))
  mean_numeric <- (mean_rad %% (2 * pi)) * 12 / (2 * pi) + 1
  rho_val <- as.numeric(rho.circular(circ_months_rad))
  
  return(data.frame(
    mean_month_deg = mean_rad * 180 / pi,
    mean_month_numeric = mean_numeric,
    rho = rho_val
  ))
}