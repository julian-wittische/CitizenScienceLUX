######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out the main territory of a user

############ Filtering out visitors and occasional users  ----
obs_res50 <- all %>%
  filter(user.id %in% res) %>%
  group_by(user.id) %>%
  filter(n() >= 50) %>%
  filter(luxornot==TRUE) %>%
  ungroup()
  
latlon_r <- do.call(rbind, strsplit(obs_res50$location, ","))
latlon_r <- apply(latlon_r, 2, as.numeric)
colnames(latlon_r) <- c("latitude", "longitude")

obs_res50 <- cbind(obs_res50, latlon_r)


res50 <- unique(obs_res50$user.id)
res50 <- res50[res50 %!in% c(1662249, 1689667, 1662111)] # extreme bimodal users with few points

############ Calculate core and home ranges using kernel density estimation ----
### Loop through each resident with >50 obs 
# Create a list to store results
results <- list()

# Loop
for(i in seq_along(res50)) {
  u <- res50[i]
  cat("Processing user", i, "->", u, "...\n")
  user_obs <- obs_res50  %>% filter(user.id == u)
  
  # Convert to SpatialPoints
  coordinates(user_obs) <- ~longitude+latitude
  proj4string(user_obs) <- CRS("+proj=longlat +datum=WGS84")
  
  # Project to a meters CRS
  user_obs_utm <- spTransform(user_obs, CRS("+init=epsg:2169"))
  
  
  # KDE
  kde <- kernelUD(user_obs_utm, h="href", grid = 400, extent = 2)
  
  # Home range 95% and core area 50%
  hr95 <- getverticeshr(kde, percent=95)
  core50 <- getverticeshr(kde, percent=50)
  
  # Compute areas in km²
  area_hr95 <- sum(sapply(hr95@polygons, function(x) x@area)) / 1e6
  area_core50 <- sum(sapply(core50@polygons, function(x) x@area)) / 1e6
  
  # Store results
  results[[i]] <- list(
    user.id = u,
    user.login = obs_res50[obs_res50$user.id==u,]$user.login[1],
    hr95 = hr95,
    core50 = core50,
    area_hr95_km2 = area_hr95,
    area_core50_km2 = area_core50
  )
}


###### Transform into sf object
### Extract hr95 polygons
hr95_sf <- lapply(results, function(x) {
  st_as_sf(x$hr95) %>%
    mutate(user.id = x$user.id,
           user.login = x$user.login,
           area_hr95_km2 = x$area_hr95_km2)
}) %>%
  do.call(rbind, .)

### Extract core50 polygons
core50_sf <- lapply(results, function(x) {
  st_as_sf(x$core50) %>%
    mutate(user.id = x$user.id,
           user.login = x$user.login,
           area_core50_km2 = x$area_core50_km2)
}) %>%
  do.call(rbind, .)

###### Plotting
ggplot(hr95_sf) +
  geom_sf(aes(fill = area), alpha = 0.3, color = NA) +
  theme_minimal()

ggplot(core50_sf) +
  geom_sf(fill = "darkgreen", color = NA, alpha = 0.025) +
  theme_minimal()

############ Find centroids and their municipalities ----
###### Project 
centr_c50 <- st_centroid(core50_sf)
ggplot(centr_c50) +
  geom_sf() +
  theme_minimal()

#com <- st_transform(com, crs=st_crs("+init=epsg:2169"))
#centr_c50 <- st_transform(centr_c50, crs=st_crs("+init=epsg:2169"))

# Esch décevant, steinfort strong, cold spots

centr_c50_com <- st_join(centr_c50, com, join = st_within)

sort(table(centr_c50_com$COMMUNE), decreasing=TRUE)

###### Is there a commune with no centroid (=user home)?
unique(com$COMMUNE)[which(unique(com$COMMUNE) %!in% unique(centr_c50_com$COMMUNE))]

