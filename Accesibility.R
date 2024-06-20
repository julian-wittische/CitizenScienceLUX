################################################################################
################### GOAL: citizen science data analysis#########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ TO DO LIST
# Use server because we need at least 46 Gb of RAM

# Crop observations to only include ones from Luxembourg
lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
coords <- res[,c("longitude","latitude")]
coords <- st_as_sf(x = coords, coords = c("longitude", "latitude"), crs = "EPSG:4326")
coords <- st_transform(coords, crs="EPSG:2169")

crop_logical <- st_contains(lux_borders, coords, sparse=FALSE)
res <- res[which(crop_logical==FALSE),]

coords <- st_intersection(coords, lux_borders)

roads <- st_read("S:/BDPatNat/_Julian/ENV_DATA_EUROPE/roadsLUX2169_1.geojson")
roads <- st_intersection(lux_borders, roads, tolerance=0)
roadsimp <- st_simplify(roads, dTolerance=50, preserveTopology = FALSE)
gc()

atcf_dist_road <- st_distance(coords, roadsimp)
atcf_dist_road_min <- apply(atcf_dist_road, 1, which.min)
is.matrix(atcf_dist_road)
beepr::beep(7)
gc()

min_distances <- numeric(nrow(atcf_dist_road))
for (i in 1:nrow(atcf_dist_road)){
  min_distances[i] <- atcf_dist_road[i, atcf_dist_road_min[i]]
  print(paste(i,"/", nrow(atcf_dist_road)))
}

# We have a simple computer and we need to delete stuff to not overwhelm RAP
print(object.size(atcf_dist_road), units = "Gb")
rm(atcf_dist_road)

# Let us compare with random locations
rand <- st_sample(lux_borders, 300000)

atcf_dist_road_rand <- st_distance(rand, roadsimp)
atcf_dist_road_rand_min <- apply(atcf_dist_road_rand, 1, which.min)
is.matrix(atcf_dist_road_rand)
beepr::beep(7)
gc()

min_distances_rand <- numeric(nrow(atcf_dist_road_rand))
for (i in 1:nrow(atcf_dist_road_rand)){
  min_distances_rand[i] <- atcf_dist_road_rand[i, atcf_dist_road_rand_min[i]]
  print(paste(i,"/", nrow(atcf_dist_road_rand)))
}

# We have a simple computer and we need to delete stuff to not overwhelm RAP
print(object.size(atcf_dist_road_rand), units = "Gb")
rm(atcf_dist_road_rand)

# Mean comparison test
car::qqPlot(min_distances) # distribution not obvious
car::qqPlot(min_distances_rand)

descdist(min_distances, discrete = FALSE)
descdist(min_distances_rand, discrete = FALSE)

mean(min_distances)
mean(min_distances_rand)

wilcox.test(min_distances, min_distances_rand)