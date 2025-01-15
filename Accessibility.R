################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: accessibility analyses 
################################################################################

###### Load roads of Luxembourg and surroundings (bounding box)
# Load raw data
roads <- st_read(paste0(ENVIPATH, "roadsLUX2169_1.geojson"))
# Remove parts outside Luxembourg
roads <- st_intersection(lux_borders, roads, tolerance=0)
# Simplify road network to make analyses faster
roadsimp <- st_simplify(roads, dTolerance=50, preserveTopology = FALSE)

###### Distance from observations to roads and large trails
# Find each pair of observation and their nearest  linear features
atcf_nearest_road <- st_nearest_feature(coords, roadsimp, check_crs = TRUE, longlat = FALSE)
# Calculate distance
atcf_dist_road <- st_distance(coords, roadsimp[atcf_nearest_road,], by_element = TRUE)
is.matrix(atcf_dist_road)
gc()

###### Distance from random locations to roads and large trails
# Create as many random points as they are observations
rand <- st_sample(lux_borders, nrow(verif250notobsc))
# Find each pair of observation and their nearest  linear features
atcf_nearest_road_rand <- st_nearest_feature(rand, roadsimp, check_crs = TRUE, longlat = FALSE)
# Calculate distance
atcf_dist_road_rand <- st_distance(rand, roadsimp[atcf_nearest_road_rand,], by_element = TRUE)
is.matrix(atcf_dist_road_rand)
gc()
# Change object class for analyses
roadObsDist <- as.numeric(atcf_dist_road) 
roadObsDistRand <- as.numeric(atcf_dist_road_rand) 

###### Analyses
# Quantile-quantile plots
qqPlot(roadObsDist) # not normal
qqPlot(roadObsDistRand) # not normal

# Cullen and Frey graphs
descdist(roadObsDist, discrete = FALSE) # beta
descdist(roadObsDistRand, discrete = FALSE) # no clear theoretical distribution

# Mean and standard deviations values
mean(roadObsDist)
sd(roadObsDist)
mean(roadObsDistRand)
sd(roadObsDistRand)

# Mean comparison tests
wilcox.test(roadObsDist, roadObsDistRand, alternative="less")
ks.test(roadObsDist, roadObsDistRand, alternative="greater")
# TECHNICAL NOTE:
# wilcox.test(x, y, alternative = "greater") means𝑥>𝑦
# ks.test(x, y, alternative = "greater") means𝐹𝑥( 𝑡)<𝐹𝑦(𝑡)

# Plotting empprical VS. random
plotdf <- cbind(as.numeric(c(roadObsDist, roadObsDistRand)),
                c(rep("EMP", nrow(verif250notobsc)),
                  rep("RAND", nrow(verif250notobsc))))

colnames(plotdf) <- c("Distance", "EmpirRandom")
plotdf <- as.data.frame(plotdf)
plotdf$Distance <- as.numeric(plotdf$Distance)

ggplot(plotdf, aes(x = Distance, fill = EmpirRandom)) +
  geom_density(alpha = .5) + 
  geom_vline(xintercept=25, linetype = "longdash") 

