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

lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")

coords <- verif250notobsc[,c("longitude","latitude")]
coords <- st_as_sf(x = coords, coords = c("longitude", "latitude"), crs = "EPSG:4326")
coords <- st_transform(coords, crs="EPSG:2169")
coords <- st_intersection(coords, lux_borders)
crop_logical <- st_contains(lux_borders, coords, sparse=FALSE)
verif250notobsc <- verif250notobsc[which(crop_logical==TRUE),]




  
centroid.df <- st_centroid()

getBox(coords2)