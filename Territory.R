######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out the main territory of a user

############ Function  ----

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

### TESTING
res50 <- unique(obs_res50$user.id)
res50 <- res50[res50 %!in% c(1662249, 1689667)] # extreme bimodal users with few points
res50 <-res50[440:length(res50)]
res50t <- res50[1:50]

# Create a list to store results
results <- list()

# Loop through each user
for(i in seq_along(res50)) {
  u <- res50[i]
  cat("Processing user", i, "->", u, "...\n")
  user_obs <- obs_res50  %>% filter(user.id == u)
  
  # Convert to SpatialPoints
  coordinates(user_obs) <- ~longitude+latitude
  proj4string(user_obs) <- CRS("+proj=longlat +datum=WGS84")
  
  # Project to UTM (meters)
  user_obs_utm <- spTransform(user_obs, CRS("+proj=utm +zone=32 +datum=WGS84"))
  
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

terr_df <- do.call(rbind, lapply(names(results), function(u) {
  data.frame(
    user.id = u,
    area_hr95_km2 = results[[u]]$area_hr95_km2,
    area_core50_km2 = results[[u]]$area_core50_km2,
    stringsAsFactors = FALSE
  )
}))

centroid.df <- st_centroid()

getBox(coords2)