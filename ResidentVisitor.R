######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : finding out whether a user is a resident or a visitor

############ Function to find if observation is in Luxembourg or not ----
luxornot <- function(coord){
  
  # Split and convert to numeric (note: sf uses lon, lat order)
  latlon <- as.numeric(strsplit(coord, ",")[[1]])
  point_sf <- st_sf(
    geometry = st_sfc(st_point(c(latlon[2], latlon[1])), crs = 4326)
  )
  
  return(c(st_within(point_sf, lux_borders, sparse = FALSE)))
}

# TESTING
userJW <- ALL_OBS$user.id[1]

user_data <- ALL_OBS %>% filter(user.id == userJW)

user_data$luxornot <- sapply(user_data$location, luxornot)
user_data$luxornot

############ User characteristics: activity, taxonomic bias, residency ----
num <- numeric(length=length(unique(ALL_OBS$user.id)))
chr <- character(length=length(unique(ALL_OBS$user.id)))

### Initialize 
user_char <- data.frame(active_days = num, #number of active days (observed on)
                        active_days_lu = num, #number of active days in Luxembourg (observed on)
                        active_period = num, #full period of activity (uploaded at); we use created at to avoid issues with scanning old photos
                        perc_o_lu = num, #% of observations in Luxembourg (compared to global)
                        perc_d_lu = num, #% of active days in Luxembourg (compared to global)
                        h = num, #residency index based on Dimson & Gillespie (2023); between 0 and 1, >0.5 means resident
                        RorV = chr, #residency interpretation: resident or visitor
                        maxobs = num, # 
                        




# CHECK LATER: we do not yet limit it by first upload date but maybe we should

i <- 1

###### Activity
user_char[i]$active_days <- length(table(user_data$observed_on_details.date))
# In Luxembourg
user_char$active_days_lu <- length(table(user_data[user_data$luxornot==TRUE,]$observed_on_details.date))
# 
user_char$active_period <- as.numeric(max(as.Date(user_data$created_at_details.date)) - min(as.Date(user_data$created_at_details.date)))
# 
user_char$perc_o_lu <- round(sum(user_data$luxornot)/nrow(user_data), 5) 
user_char$perc_d_lu <- round((active_days_lu / active_days), 5)

user_char$h <- perc_o_lu*0.4 + perc_d_lu*0.6
user_char$RorV <- ifelse(h > 0.5, "Resident", "Visitor")

user_char$max

###### 

activity_ratio
first_upload <- as.Date("2011-05-26")
rel_act_duration <- active_days/(as.Date("2025-06-30")-as.Date("2011-05-26"))
obvs_log <- log(nrow(user_data))
log_minmaxscaled_total <- (obvs_log - min(obvs_log)) / (max(obvs_log) - min(obvs_log))

class(ALL_OBS$user.id)




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

