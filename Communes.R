######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load communes data and calculate metrics for each 

############ Load commune data ----
com <- st_read(paste0(ENVIPATH, "limadmin.geojson"), layer="communes") %>% st_transform(crs="EPSG:2169")
com$area <- as.numeric(st_area(com))/1000000
pop <- read.csv(paste0(ENVIPATH,"/PopCommunes.csv"), encoding = "UTF-8")
names(pop)[2] <-  "COMMUNE"
pop$pop <- rowSums(pop[,3:6])
pop <- pop[,c(2,7)]
com <- left_join(com, pop)

# # Check
# setdiff(sort(com$COMMUNE), sort(pop$COMMUNE_NOM))
# setdiff(sort(pop$COMMUNE_NOM),sort(com$COMMUNE))
# ggplot()+ geom_sf(data=com)
# # PASSED

############ Calculate commune metrics ----

###### Commune: absolute values

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

###### Commune per capita

# Observations per capita                                                                                                         
com$observationsPC <- com$observations/com$pop

# Observers per capita
com$observersPC <- com$observers/com$pop
  
# Species per capita
com$speciesPC <- com$species/com$pop

###### Commune per km2

# Observations per commune per km2                                                                                                         
com$observationsPK <- com$observations/com$area

# Observers per commune per km2
com$observersPK <- com$observers/com$area

# Species per km2
com$speciesPK <- com$species/com$area

###### Number of unique species per commune (municipality endemism)

# Calculate the number of distinct communes for each species
species_commune_counts <- joined %>%
  st_drop_geometry() %>%  # Drop geometry for easier processing
  group_by(scientific_name) %>%
  summarise(num_communes = n_distinct(COMMUNE)) %>%
  filter(num_communes == 1)  # Retain species found in only one commune

# Filter original data for species with commune-level endemism
commune_endemism <- joined %>%
  st_drop_geometry() %>%
  inner_join(species_commune_counts, by = "scientific_name") %>%
  group_by(COMMUNE) %>%
  summarise(endemic_species_count = n_distinct(scientific_name))

com <- com %>%
  left_join(commune_endemism, by = "COMMUNE")

com$endemism_rate <- com$endemic_species_count/com$species

com[order(com$endemism_rate, decreasing=TRUE),]
com[order(com$endemism_rate, decreasing=FALSE),]

############ Plotting indices per commune ----

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


############ Commune indices minima ----
com[which.min(com$observations),]$COMMUNE.x # Biwer
com[which.min(com$observers),]$COMMUNE.x # Biwer
com[which.min(com$species),]$COMMUNE.x # Biwer

com[which.min(com$obsPobservers),]$COMMUNE.x # Biwer
com[which.min(com$speciesPobs),]$COMMUNE.x # Luxembourg
com[which.min(com$speciesPobservers),]$COMMUNE.x # Luxembourg

com[which.min(com$observationsPC),]$COMMUNE.x # Sanem
com[which.min(com$observersPC),]$COMMUNE.x # Esch-sur-Alzette
com[which.min(com$speciesPC),]$COMMUNE.x # Luxembourg

com[which.min(com$observationsPK),]$COMMUNE.x # Biwer
com[which.min(com$observersPK),]$COMMUNE.x # Biwer
com[which.min(com$speciesPK),]$COMMUNE.x # Biwer

com[which.min(com$endemic_species_count),]$COMMUNE #"Heffingen"
com[which.min(com$endemism_rate),]$COMMUNE #"Heffingen"


############ Commune indices maxima ----
com[which.max(com$observations),]$COMMUNE.x # Luxembourg
com[which.max(com$observers),]$COMMUNE.x # Luxembourg
com[which.max(com$species),]$COMMUNE.x # Luxembourg

com[which.max(com$obsPobservers),]$COMMUNE.x # Kiischpelt
com[which.max(com$speciesPobs),]$COMMUNE.x # Biwer
com[which.max(com$speciesPobservers),]$COMMUNE.x # Bech

com[which.max(com$observationsPC),]$COMMUNE.x # Kiischpelt
com[which.max(com$observersPC),]$COMMUNE.x # Kiischpelt
com[which.max(com$speciesPC),]$COMMUNE.x # Kiischpelt

com[which.max(com$observationsPK),]$COMMUNE.x # Pétange
com[which.max(com$observersPK),]$COMMUNE.x # Pétange
com[which.max(com$speciesPK),]$COMMUNE.x # Pétange

com[which.max(com$endemic_species_count),]$COMMUNE # Luxembourg
com[which.max(com$endemism_rate),]$COMMUNE # Luxembourg

############ Make table S2 ----
 commune_tab <- data.frame(commune=com$COMMUNE,
                           obsPK=round(com$observationsPK, 2),
                           surface=round(com$area, 2),
                           humans=com$pop,
                           species=com$species)

commune_tab <- commune_tab[order(commune_tab$obsPK, decreasing=TRUE),]

View(commune_tab)

############ Export df for other software ----
# st_write(com, "commune_dataset.gpkg", append=FALSE)
