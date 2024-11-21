################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Analyze observations in the context of land cover

# Load reclassified land cover data
land <- st_read("./CITIZEN SCIENCE/iNaturalistLU/paper iNat/CLC18_vec_reclassified_5cat.geojson")

# Get land cover relative surfaces (%of the land covered by each)
land_percent <- data.frame(land.cover=land$Reclassified_LC,
                           area_m2=as.numeric(land$area_m2))
land_percent$relgis <- land_percent$area_m2/sum(land_percent$area_m2)*100

# Get the land cover category extracted for each observation location
obs_percent <- st_join(coords2, land)
obs_percent <- data.frame(table(obs_percent$Reclassified_LC))
obs_percent$relobs <- obs_percent$Freq/sum(obs_percent$Freq)*100
colnames(obs_percent) <- c("land.cover", "freq", "rel")

# Combine
df_land_cover <- data.frame(land.cover=land_percent$land.cover,
                       GIS.percent=land_percent$relgis,
                       obs.percent=obs_percent$rel)
df_land_cover

# Make a graph to visualize the difference
ggplot(data=df_land_cover, aes(x = land.cover)) +
  geom_bar(aes(y = GIS.percent, fill = "GIS percent"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = obs.percent, fill = "Observed percent"), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y = "Percentage") +
  scale_fill_manual(name = "Legend", values = c("GIS percent" = "skyblue", "Observed percent" = "orange"))
