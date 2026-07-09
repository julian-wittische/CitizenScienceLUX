######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Hinatea Ariey
# Start: Spring 2026
# Data: MNHNL
# Script objective : get borders for nice map

###### Libraries ----
library(sf)
library(tidyverse)

###### Spatial object ----
bbox <- st_bbox(c(xmin = 5.7, xmax = 6.55, ymax = 50.2, ymin = 49.4), crs = st_crs(4326))
GRborders <- st_read("/home/jwittische/Code/CitizenScienceLUX/GRborders.gpkg")
# HINA CHANGE LE PATH STP

GR2169_c <- GRborders %>%
  st_crop(bbox) %>%
  st_transform("EPSG:2169")

country_labels <- data.frame(
  name = c("FRANCE", "BELGIUM", "GERMANY"),
  x = c(60000, 58000, 100000),   # adjust these
  y = c(55000, 135000, 105000)    # adjust these
)

###### Plotting ----
ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "viridis", name = expression("")) +
  labs(title = "") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title=element_blank())
