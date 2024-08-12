################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ TO DO LIST



# roads of Luxembourg and surroundings (bounding box)
roads <- st_read("./ENV_DATA_EUROPE/roadsLUX2169_1.geojson")
roads <- st_intersection(lux_borders, roads, tolerance=0)
roadsimp <- st_simplify(roads, dTolerance=50, preserveTopology = FALSE)
atcf_nearest_road <- st_nearest_feature(coords, roadsimp, check_crs = TRUE, longlat = FALSE)
atcf_dist_road <- st_distance(coords, roadsimp[atcf_nearest_road,], by_element = TRUE)
is.matrix(atcf_dist_road)
gc()

# Let us compare with random locations
rand <- st_sample(lux_borders, nrow(res))
atcf_nearest_road_rand <- st_nearest_feature(rand, roadsimp, check_crs = TRUE, longlat = FALSE)
atcf_dist_road_rand <- st_distance(rand, roadsimp[atcf_nearest_road_rand,], by_element = TRUE)
is.matrix(atcf_dist_road_rand)
gc()

roadObsDist <- as.numeric(atcf_dist_road) 
roadObsDistRand <- as.numeric(atcf_dist_road_rand) 

# Mean comparison test
qqPlot(roadObsDist) # not normal
qqPlot(roadObsDistRand)

descdist(roadObsDist, discrete = FALSE)
descdist(roadObsDistRand, discrete = FALSE)

mean(roadObsDist)
sd(roadObsDist)
mean(roadObsDistRand)
sd(roadObsDistRand)

wilcox.test(roadObsDist, roadObsDistRand, alternative="less")
ks.test(roadObsDist, roadObsDistRand, alternative="less")

plotdf <- cbind(as.numeric(c(roadObsDist, roadObsDistRand)), c(rep("EMP", nrow(res)), rep("RAND", nrow(res))))
colnames(plotdf) <- c("Distance", "EmpirRandom")
plotdf <- as.data.frame(plotdf)
plotdf$Distance <- as.numeric(plotdf$Distance)

ggplot(plotdf, aes(x = Distance, fill = EmpirRandom)) +
  geom_density(alpha = .5) + 
  geom_vline(xintercept=25, linetype = "longdash") 

