################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Analyze observations in the context of land cover
################################################################################

###### Load reclassified land cover data
land <- st_read(paste0(ENVIPATH, "CLC18_vec_reclassified_5cat.geojson"))
# see methods or CLC18_5cat.R to see reclassification information

###### Compare land cover surface with ratio from observations' land cover cat
# Get land cover relative surfaces (%of the land covered by each)
land_percent <- data.frame(land.cover=land$Reclassified_LC,
                           area_m2=as.numeric(land$area_m2))
# Proportion
land_percent$relgis <- land_percent$area_m2/sum(land_percent$area_m2)*100

# Get the land cover category extracted for each observation location
obs_percent <- st_join(coords2, land)
obs_percent <- data.frame(table(obs_percent$Reclassified_LC))
# Proportion
obs_percent$relobs <- obs_percent$Freq/sum(obs_percent$Freq)*100
colnames(obs_percent) <- c("land.cover", "freq", "rel")

# Combine into single dataset
df_land_cover <- data.frame(land.cover=land_percent$land.cover,
                       GIS.percent=land_percent$relgis,
                       obs.percent=obs_percent$rel)

###### Plotting

# Wrong plot
ggplot(data=df_land_cover, aes(x = land.cover)) +
  geom_bar(aes(y = GIS.percent, fill = "GIS percent"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = obs.percent, fill = "Observed percent"), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y = "Percentage") +
  scale_fill_manual(name = "Legend", values = c("GIS percent" = "skyblue", "Observed percent" = "orange"))

# Divergence plot
df_land_cover$difference <- df_land_cover$obs.percent - df_land_cover$GIS.percent

ggplot(df_land_cover, aes(x = reorder(land.cover, difference), y = difference, fill = difference > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Land Cover",
    y = "Difference (obs.percent - GIS.percent)",
    title = "Divergence Between Observed and GIS Land Cover Percentages"
  ) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Underrepresented", "Overrepresented")) +
  theme_minimal()