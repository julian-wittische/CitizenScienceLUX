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
com <- st_make_valid(com)
com <- left_join(com, pop)

#Checkpoint
ggplot(data=com) + geom_sf(aes(fill=pop))


############ Calculate commune metrics ----

###### Commune: absolute values

# Spatial join
st_crs(com) <- st_crs(coords2)

#Checkpoint
# test
cm <- coords2[coords2$user_login=="cobymeester",]
table(cm$place_county_name)

ggplot(data = cm) +
  geom_sf(data = cm, color = "red", size = 0.1) + # points overlay
  theme_minimal()

joined_testcm <- st_join(cm, com, join = st_intersects, left = FALSE)
table(joined_testcm$CANTON)

sum(st_within(cm, com) %>% lengths() > 0)

joined <- st_join(coords2, com, join = st_within, left = FALSE)

length(unique(joined[joined$COMMUNE=="Kiischpelt",]$user_id))

sort(table(joined[joined$COMMUNE=="Kiischpelt",]$user_login), decreasing = T)
# FALSE!!!



# Observations                                                                                                        
com$observations <- lengths(st_intersects(com, coords2))

observers <- joined %>%  group_by(COMMUNE) %>% summarise(observers = n_distinct(user_id))
com <- st_join(com, observers, join = st_equals, left=FALSE)

# Species
species <- joined %>% filter(str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(species = n_distinct(scientific_name))
com <- st_join(com, species, left=FALSE)

# Introduced
introd_com <- joined %>% filter(introd2==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(introd = n_distinct(scientific_name))
com <- st_join(com, introd_com, left=FALSE)
com$introd.rate <- com$introd/com$species

# Invasive
inv_com <- joined %>% filter(inv==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(inv = n_distinct(scientific_name))
com <- st_join(com, inv_com, left=FALSE)
com$inv.rate <- com$inv/com$species

# Protected
prot_com <- joined %>% filter(prot==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(prot = n_distinct(scientific_name))
com <- st_join(com, prot_com, left=FALSE)
com$prot.rate <- com$prot/com$species

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

commune_endemism$COMMUNE.x <- commune_endemism$COMMUNE

com <- com %>%
  left_join(commune_endemism, by = "COMMUNE.x")

com$endemism_rate <- com$endemic_species_count/com$species

############ Plotting ----
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


############ Make table S2 ----
 commune_tab <- data.frame(commune=com$COMMUNE,
                           surface=round(com$area, 2),
                           humans=com$pop,
                           observation=com$observations,
                           observers=com$observers,
                           species=com$species,
                           obs.per.km2=round(com$observations, 2),
                           observers.per.km2=round(com$observationsPK, 2),
                           species.per.km2=round(com$speciesPK, 2),
                           obs.per.cap=round(com$observationsPC, 2),
                           observer.per.cap=round(com$observersPC, 2),
                           species.per.cap=round(com$speciesPC, 2),
                           unique.sp=com$endemic_species_count,
                           introd.sp=com$introd,
                           introd.rate=round(com$introd.rate, 2),
                           inv.sp=com$inv,
                           inv.rate=round(com$inv.rate, 2),
                           prot.sp=com$prot,
                           prot.rate=round(com$prot.rate, 2))
                           

commune_tab <- commune_tab[order(commune_tab$observers.per.km2, decreasing=TRUE),]

View(commune_tab)

############ Export df for other software ----
# st_write(com, "commune_dataset.gpkg", append=FALSE)
# Export for Paul
st_write(com, "commune_dataset.gpkg", append=FALSE)
View(com
)
