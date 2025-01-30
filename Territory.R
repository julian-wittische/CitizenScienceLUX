######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out the main territory of a user

############ Function  ----

observers_id <- unique(inat_all$user_id)
idfiers_id <- unique(id$user_id)

######

test_user <- "10058"
test_user <- "julian_wittische"
test_user <- "wolffchristiane"
obs_user <- get_inat_obs_user(test_user, maxresults = 100000)
coords <- st_as_sf()




  
centroid.df <- st_centroid()

getBox(coords2)