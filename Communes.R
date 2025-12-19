######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load communes data and calculate metrics for each 

############ Loading libraries ----
source("config.R")

############ Loading libraries ----
library(sf)
library(tidyverse)

############ Load commune data ----
# Administrative limits
com <- st_read(paste0(ENVIPATH, "limadmin.geojson"), layer="communes") %>% st_transform(crs="EPSG:2169")
com$area <- as.numeric(st_area(com))/1000000
# Census
pop <- read.csv(paste0(ENVIPATH,"/PopCommunes.csv"), encoding = "UTF-8")
names(pop)[2] <-  "COMMUNE"
pop$pop <- rowSums(pop[,3:6])
pop <- pop[,c(2,7)]
# Join commune and demography
com <- left_join(com, pop)
# Spatial join with observations
st_crs(com) <- st_crs(coords2)
joined <- st_join(coords2, com, left = FALSE)

############ Calculate commune metrics ----

#######################################
metrcomm <- joined %>%
  st_drop_geometry() %>%
  group_by(COMMUNE) %>%
  summarise(
    observations = n(),
    observers = n_distinct(user_login),
    species =  n_distinct(scientific_name[str_detect(scientific_name, "\\w+ \\w+")]),
    introd = n_distinct(scientific_name[introd2 == TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    inv = n_distinct(scientific_name[inv==TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    prot = n_distinct(scientific_name[prot==TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    .groups = "drop"
  )
########################################


###### Observations                                                                                                        
com$observations <- lengths(st_intersects(com, coords2))

###### Observers
observers <- joined %>%  group_by(COMMUNE) %>% summarise(observers = n_distinct(user_id))
com <- st_join(com, observers, left=FALSE)

###### Species
species <- joined %>% filter(str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(species = n_distinct(scientific_name))
com <- st_join(com, species, left=FALSE)

###### Introduced
introd_com <- joined %>% filter(introd2==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(introd = n_distinct(scientific_name))
com <- st_join(com, introd_com, left=FALSE)
com$introd.rate <- com$introd/com$species

###### Invasive
inv_com <- joined %>% filter(inv==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(inv = n_distinct(scientific_name))
com <- st_join(com, inv_com, left=FALSE)
com$inv.rate <- com$inv/com$species

###### Protected
prot_com <- joined %>% filter(prot==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(COMMUNE) %>% summarise(prot = n_distinct(scientific_name))
com <- st_join(com, prot_com, left=FALSE)
com$prot.rate <- com$prot/com$species

###### Individual engagement (observation/observer)
com$obsPobservers <- com$observations/com$observers

###### Taxonomic coverage efficiency (species/observation)
com$speciesPobs <- com$species/com$observations

###### Community specialization (basic ratio)
com$speciesPobservers <- com$species/com$observers

###### Per capita
### Observations per capita                                                                                                         
com$observationsPC <- com$observations/com$pop
### Observers per capita
com$observersPC <- com$observers/com$pop
### Species per capita
com$speciesPC <- com$species/com$pop

###### Per km^2
### Observations per commune per km2                                                                                                         
com$observationsPK <- com$observations/com$area
### Observers per commune per km2
com$observersPK <- com$observers/com$area
### Species per km2
com$speciesPK <- com$species/com$area

###### Number of unique species per commune (municipality endemism)
### Calculate the number of distinct communes for each species
species_commune_counts <- joined %>%
  st_drop_geometry() %>%  # Drop geometry for easier processing
  group_by(scientific_name) %>%
  summarise(num_communes = n_distinct(COMMUNE)) %>%
  filter(num_communes == 1)  # Retain species found in only one commune
### Filter original data for species with commune-level endemism
commune_endemism <- joined %>%
  st_drop_geometry() %>%
  inner_join(species_commune_counts, by = "scientific_name") %>%
  group_by(COMMUNE) %>%
  summarise(endemic_species_count = n_distinct(scientific_name))
commune_endemism$COMMUNE.x <- commune_endemism$COMMUNE
com <- left_join(com, commune_endemism, by = "COMMUNE.x")
com$endemism_rate <- com$endemic_species_count/com$species

############ Plotting ----
###### Demography
ggplot(data=com) + geom_sf(aes(fill=observations))+
  scale_fill_viridis_c(option = "mako")


############ Exports ----
###### Make table for annex
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

###### Excel for Tania and Claude exploration
write_excel_csv(st_drop_geometry(com), "communes_results.csv")

###### Export df for other software
st_write(com, "commune_dataset.gpkg", append=FALSE)

ggplot(com, aes(x = observations, y = prot.rate*inv.rate)) + 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(com %>% filter(observations <= 10000),
       aes(x = observations, y = prot.rate * inv.rate)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(com %>% filter(observations <= 10000),
       aes(x = observations, y = prot.rate * inv.rate)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ log(x))

ggplot(com %>% filter(observations <= 10000),
       aes(x = observations, y = prot.rate * inv.rate)) +
  geom_point() +
stat_smooth(method = "lm", formula = y ~ log(x), color = "red") +
  stat_smooth(method = "loess", span = 0.8, color = "blue")

### Idée Tania

# Create a color palette
col_palette <- colorRampPalette(c("blue", "white", "red"))  # low->high

# Map z to colors
colors <- col_palette(100)[as.numeric(cut(log(commune_tab$observation), breaks = 100))]


plot(commune_tab$species/commune_tab$observation, 
     commune_tab$observation/commune_tab$observers,
     col = colors, pch = 19, cex = 1.5)

cor.test(commune_tab$species/commune_tab$observation, 
     commune_tab$observation/commune_tab$observers)
