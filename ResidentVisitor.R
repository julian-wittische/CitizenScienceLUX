######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : finding out whether a user is a resident or a visitor

load("ALL_OBS.RData")

# TESTING
userJW <- 4818229

user_data <- ALL_OBS %>% filter(user.id == userJW)

user_data$luxornot <- sapply(user_data$location, luxornot)
user_data$luxornot

user_char <- data.frame()
# CHECK LATER: we do not yet limit it by first upload date but maybe we should
active_days <- length(table(user_data$observed_on_details.date))
# In Luxembourg
active_days_lu <- length(table(user_data[user_data$luxornot==TRUE,]$observed_on_details.date))
# we use created at to avoid issues with scanning old photos
active_period <- as.numeric(max(as.Date(user_data$created_at_details.date)) - min(as.Date(user_data$created_at_details.date)))
# 
perc_o_lu <- round(sum(user_data$luxornot)/nrow(user_data), 5) 
perc_d_lu <- round((active_days_lu / active_days), 5)

h <- perc_o_lu*0.4 + perc_d_lu*0.6
RorV <- ifelse(h > 0.5, "Resident", "Visitor")

activity_ratio
first_upload <- as.Date("2011-05-26")
rel_act_duration <- active_days/(as.Date("2025-06-30")-as.Date("2011-05-26"))
obvs_log <- log(nrow(user_data))
log_minmaxscaled_total <- (obvs_log - min(obvs_log)) / (max(obvs_log) - min(obvs_log))

class(ALL_OBS$user.id)

luxornot <- function(coord){
  
  # Split and convert to numeric (note: sf uses lon, lat order)
  latlon <- as.numeric(strsplit(coord, ",")[[1]])
  point_sf <- st_sf(
    geometry = st_sfc(st_point(c(latlon[2], latlon[1])), crs = 4326)
  )
  
  return(c(st_within(point_sf, lux_borders, sparse = FALSE)))
}


############ h scale
# (h = 0.4(oh) + 0.6(dh); Dimson & Gillespie 2023)

###### oh: # Lux / # global
user$oh <- country=="Luxembourg" / nrow(user)

###### dh:  proportion of active days in Luxembourg / active days global
user$active <- unique(user$date)
user$activeLux <- unique(userINLUX$date)
user$dh <- length(user$active)/length(user$activeLux)
  
###### Calculating h
user$h <- 0.4*oh + 0.6*dh

