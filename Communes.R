################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Load communes data and calculate metrics for each 

############ Load communes
com <- st_read("~/Data/limadmin.geojson", layer="communes") %>% st_transform(crs="EPSG:2169")
com$area <- as.numeric(st_area(com))/1000000
pop <- read.csv("~/Data/PopCommunes.csv", encoding = "UTF-8")
names(pop)[2] <-  "COMMUNE"
pop$pop <- rowSums(pop[,3:6])
pop <- pop[,c(2,7)]
com <- left_join(com, pop)

# # Check
# setdiff(sort(com$COMMUNE), sort(pop$COMMUNE_NOM))
# setdiff(sort(pop$COMMUNE_NOM),sort(com$COMMUNE))
# ggplot()+ geom_sf(data=com)
# # PASSED

############ Calculate metrics

### Commune level; absolute values

# Spatial join 
joined <- st_join(coords2, com, left=FALSE)

# Observations                                                                                                        
com$observations <- lengths(st_intersects(com, coords2))

# Observers
observers <- joined %>%  group_by(COMMUNE) %>% summarise(observers = n_distinct(user_login))
com <- st_join(com, observers, left=FALSE)

# Species
species <- joined %>% filter(str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(species = n_distinct(scientific_name))
com <- st_join(com, species, left=FALSE)

# Individual engagement (observation/observer)
com$obsPobservers <- com$observations/com$observers

# Taxonomic coverage efficiency (species/observation)
com$speciesPobs <- com$species/com$observations

# Community specialization (basic ratio)
com$speciesPobservers <- com$species/com$observers

### Commune per capita

# Observations per capita                                                                                                         
com$observationsPC <- com$observations/com$pop

# Observers per capita
com$observersPC <- com$observers/com$pop
  
# Species per capita
com$speciesPC <- com$species/com$pop

### Commune per km2

# Observations per commune per km2                                                                                                         
com$observationsPK <- com$observations/com$area

# Observers per commune per km2
com$observersPK <- com$observers/com$area

# Species per km2
com$speciesPK <- com$species/com$area

### Percentage of protected area in commune
com2 <- com
com2$area_municipality <- as.numeric(st_area(com2))/1000000
intersection <- st_intersection(com2, st_union(prot.areas))
intersection$area_intersection <- st_area(intersection)

intersection_summary <- intersection %>%
  group_by(COMMUNE) %>% summarise(area_prot_in_municipality = sum(area_intersection))

inter <- st_drop_geometry(intersection_summary)
inter$area_prot_in_municipality <- as.numeric(inter$area_prot_in_municipality)/1000000
inter <- rbind(inter, c("Diekirch", 0))
inter$area_prot_in_municipality <- as.numeric(inter$area_prot_in_municipality)

com2 <- left_join(com2, inter)

com2 <- com2 %>%
  mutate(percentage_protected = (area_prot_in_municipality / area_municipality) * 100)

ggplot() +
  geom_sf(data = com2, aes(fill = percentage_protected), color = "black") +  # Fill by percentage
  scale_fill_viridis_c(option = "plasma", name = "% Protected") +  # Use a color scale for the percentage
  theme_minimal() +
  labs(title = "Percentage of Municipality Area Covered by Protected Areas",
       subtitle = "Each municipality is shaded by the percentage of its area in a protected area",
       fill = "Protected %") +
  theme(legend.position = "right")

com$percent.protected <- com2$percentage_protected

# Export for Paul
st_write(com, "commune_dataset.gpkg", append=FALSE)

############ Plotting
ggplot(data=com) + geom_sf(aes(fill=pop)) +
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=percent.protected)) +
  scale_fill_viridis_c(option = "viridis")

ggplot(data=com) + geom_sf(aes(fill=observations))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=observers))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=species))+
  scale_fill_viridis_c(option = "viridis")

ggplot(data=com) + geom_sf(aes(fill=obsPobservers))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=speciesPobs))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=speciesPobservers))+
  scale_fill_viridis_c(option = "viridis")

ggplot(data=com) + geom_sf(aes(fill=observationsPC))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=observersPC))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=speciesPC))+
  scale_fill_viridis_c(option = "viridis")

ggplot(data=com) + geom_sf(aes(fill=observationsPK))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=observersPK))+
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=speciesPK))+
  scale_fill_viridis_c(option = "viridis")

############ Analysis
com_df <- st_drop_geometry(com)
mod <- glm(speciesPobs ~ as.numeric(area) + as.numeric(percent.protected) + observers, data=com_df)
vif(mod)
summary(mod)

plot_model(mod, type="pred")
