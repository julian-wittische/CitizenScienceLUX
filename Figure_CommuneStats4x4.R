######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2026
# Data: MNHNL
# Script objective : plot map with commune metrics

############ Local configuration ----
source("config.R")

############ Loading libraries ----
source("0_Libraries.R")

############ Loading necessary script ----
# source("1_Data.R")
# source("Communes.R")

bbox <- st_bbox(c(xmin = 5.7, xmax = 6.55, ymax = 50.2, ymin = 49.4), crs = st_crs(4326))
GRborders <- st_read("GRborders.gpkg")

GR2169_c <- GRborders %>%
  st_crop(bbox) %>%
  st_transform("EPSG:2169")

country_labels <- data.frame(
  name = c("FRANCE", "BELGIUM", "GERMANY"),
  x = c(60000, 58000, 100000),   # adjust these
  y = c(55000, 135000, 105000)    # adjust these
)

###### Observations per km2
ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_sf(data = com, aes(fill = observationsPK), color = "black") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "viridis", name = expression("Obs.km"^-2*"")) +
  labs(title = "Observations per squared kilometer") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank())

###### Observations per km2
ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_sf(data = com, aes(fill = observationsPK), color = "black") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "viridis", name = expression("Obs.km"^-2*"")) +
  labs(title = "Observations per squared kilometer") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank())


###### Observers per capita
ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_sf(data = com, aes(fill = observersPC), color = "black") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "plasma", name = expression("Observers.inhabitant"^-1*"")) +
  labs(title = "Observers per capita") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank())

###### Species richness estimate




ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_sf(data = com, aes(fill = inv.rate*prot.rate/max(inv.rate*prot.rate)), color = "black") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "rocket", name = expression("Risk index"), direction=-1) +
  labs(title = "Potential for invasive effects on protected species") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank())


###### Invasive rate * Protected rate

ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_sf(data = com, aes(fill = inv.rate*prot.rate/max(inv.rate*prot.rate)), color = "black") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "rocket", name = expression("Risk index"), direction=-1) +
  labs(title = "Potential for invasive effects on protected species") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank())





