######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load protected areas data and calculate metrics for each 

############ Load data ----
#load("iNat.RData") # Created using 1_Data.R

############ Local configuration ----
source("config.R")

############ Loading libraries ----
source("0_Libraries.R")

############ Loading protected area data ----
####### Encoding option for sf
enc.opt <- "ENCODING=latin1"
enc.opt <- "ENCODING=UTF8"

enc.opt <- "ENCODING=Windows1242"
enc.opt.z <- "ENCODING=latin1"

###### Load Natura 2000 data
ludh <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"ludh-20231006.zip"), options = enc.opt)
ludo <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"ludo-20231006.zip"), options = enc.opt)

ludh <- ludh[,2:3]
ludo <- ludo[,2:3]

ludh$type <- "Habitats Dir."
ludo$type <- "Birds Dir."

### Double check potential full overlap - same name "Dudelange Haard"

# Slight difference
st_area(ludo[ludo$SITENAME=="Dudelange Haard",])
st_area(ludh[ludh$SITENAME=="Dudelange Haard",])

# Sliver in the southwesternmost part of the protected area
ggplot() + 
  geom_sf(data=ludh[ludh$SITENAME=="Dudelange Haard",], fill=alpha("green", 0.99)) +
  geom_sf(data=ludo[ludo$SITENAME=="Dudelange Haard",], fill=alpha("black", 0.99))

# Solution: rename with HD and BD
ludo[ludo$SITENAME=="Dudelange Haard",]$SITENAME <- "Dudelange Haard (BD)"
ludh[ludh$SITENAME=="Dudelange Haard",]$SITENAME <- "Dudelange Haard (HD)"

# ERROR: typo and same same for different directives
ludo[ludo$SITENAME=="Esch-sur-Alzette Sud-est - Anciennes minières / Ellergronn",]$SITENAME <- "Esch-sur-Alzette Sud-est - Anciennes minières / Ellergronn (BD)"
ludh[ludh$SITENAME=="Esch-sur-Alzette Sud-est - Anciennes minières / Ellegronn",]$SITENAME <- "Esch-sur-Alzette Sud-est - Anciennes minières / Ellergronn (HD)"

###### Load ZPIN data

zpin <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"zpin-declarees.zip"),options = enc.opt.z)
zpin <- st_zm(zpin)
# ERROR: at least 1 geometry not valid
zpin <- st_make_valid(zpin)
# ERROR: A\r\n and 1 instead of A
# Correction
zpin[107,"SOUSZONE"] <- "A"
zpin[120, "SOUSZONE"] <- "A"

# ERROR: Weird Taupeschwues
zpin[27, "NOM"] <- "Haff Réimech - Taupeschwues"

# ISSUE: both haff Réimech zones have same PRIE_ID
# Add something
zpin[27, "PRIE_ID"] <- 80018666
zpin[29, "PRIE_ID"] <- 80018666

# ERROR: Weird character 
zpin[55, "NOM"] <- "Carrière de Bettendorf - Schoofsbësch"

# ERROR: Typo 
zpin[111, "NOM"] <- "Härebësch"

# Problem Laangmuer
ggplot() + geom_sf(data=zpin[zpin$NOM=="Laangmuer",])

zpin_temp <- zpin %>%   group_by(PRIE_ID) %>%
  summarise(SITECODE= first(PRIE_ID), SITENAME=first(NOM), geometry = st_union(geometry))
sort(zpin_temp$SITENAME)

# Checkpoint
ggplot() + geom_sf(data=zpin_temp[zpin_temp$SITENAME=="Laangmuer",])

zpin <- zpin_temp
zpin <- zpin[,c("SITECODE","SITENAME")]
zpin$SITECODE <- as.character(zpin$SITECODE)

zpin$type <- "National"
################################################################################
################################################################################
################################################################################

###### Combine
prot.areas <- rbind(ludh, ludo, zpin)

# Spatial join
st_crs(prot.areas) <- st_crs(coords2)
# Spatial join 
joined_prot <- st_join(coords2, prot.areas, left=FALSE)

# Checkpoint
sum(table(prot.areas$SITENAME)>1)
which(table(prot.areas$SITENAME)>1)

centroid.df <- st_centroid(prot.areas)

############ Calculate various metrics ----
### Observations
# Number of observations in protected areas
in.prot <- st_intersects(prot.areas, coords)

# Number of observations per protected area
prot.areas$per.prot <- lengths(in.prot)

### Species
speciesP <- prot.areas %>%
  left_join(
    joined_prot %>%
      st_drop_geometry() %>%               # drop geometry if joined_prot is sf
      group_by(SITENAME) %>%
      summarise(species = n_distinct(scientific_name), .groups = "drop"),
    by = "SITENAME"
  ) %>%
  mutate(species = replace_na(species, 0))
prot.areas_sp <- st_join(prot.areas, speciesP, join = st_equals, left=FALSE)

### Observers
observersP <- prot.areas %>%
  left_join(
    joined_prot %>%
      st_drop_geometry() %>%               # drop geometry if joined_prot is sf
      group_by(SITENAME) %>%
      summarise(observers = n_distinct(user_login), .groups = "drop"),
    by = "SITENAME"
  ) %>%
  mutate(observers = replace_na(observers, 0))
prot.areas <- st_join(prot.areas, observersP, join = st_equals, left=FALSE)

###### Per km^2
### Observations/km^2 per protected area
prot.areas$area.per.prot <- as.numeric(st_area(prot.areas))/1000000 # convertin m2 in km2
prot.areas$obs.per.km2 <- prot.areas$per.prot / prot.areas$area.per.prot

### Prep






###### Mean and SD
mean(prot.areas$obs.per.km2)
sd(prot.areas$obs.per.km2)

###### Min/Max
View(prot.areas)

############ Area size effect on obs/km2 ----
mod <- glmer(per.prot ~ area.per.prot + (1|SITENAME), data=prot.areas, family = poisson)
summary(mod)
sim <- simulateResiduals(mod, refit=T, n=99)
plotSimulatedResiduals(sim)
r2(mod)
model_performance(mod)
r.squaredGLMM(mod)

###### Number of species per protected area

# Species
species_prot <- joined_prot %>%
  filter(str_detect(scientific_name, "\\w+ \\w+")) %>%
  group_by(SITENAME) %>%
  summarise(species = n_distinct(scientific_name))

# Checkpoint
lol <- rbind(ludh, ludo, zpin)
lol[which(lol$SITENAME %!in% species_prot$SITENAME),]
# "Schimpach - Carrières de Schimpach" has zero observations and hence zero species

### Add it to the dataset
zeroprot <- prot.areas[prot.areas$SITENAME %in% c("Schimpach - Carrières de Schimpach","Sonlez-Pamer"),]
zeroprot$species <- 0
zeroprot <- zeroprot[, c("SITENAME", "species", "geometry")]
species_prot <- rbind(species_prot, zeroprot)

### Join species
prot_map <- prot.areas %>%
  left_join(st_drop_geometry(species_prot), by = "SITENAME") %>%
  st_as_sf(prot.areas)

###### Introduced, invasive, protected

# Introduced
introd_prot <- joined_prot %>% filter(introd2==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(SITENAME) %>% summarise(introd = n_distinct(scientific_name))
prot_map <- st_join(prot_map, introd, left=FALSE)
prot_map$introd.rate <- prot_map$introd/prot_map$species

# Invasive
inv_prot <- joined_prot %>% filter(inv==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(SITENAME) %>% summarise(inv = n_distinct(scientific_name))
prot_map <- st_join(prot_map, inv, left=FALSE)
prot_map$inv.rate <- prot_map$inv/prot_map$species

# Protected
prot_prot <- joined_prot %>% filter(prot==TRUE, str_detect(scientific_name, "\\w+ \\w+")) %>% group_by(SITENAME) %>% summarise(prot = n_distinct(scientific_name))
prot_map <- st_join(prot_map, prot, left=FALSE)
prot_map$prot.rate <- prot_map$prot/prot_map$species

############ Make table S1 ----
prot_tab <- data.frame(prot_name=prot_map$SITENAME,
                          obsPK=round(prot_map$obs.per.km2, 2),
                          surface=round(prot_map$area.per.prot, 2),
                       type=prot_map$type,
                       species=prot_map$species,
                       observations=$observations,
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


prot_tab <- prot_tab[order(prot_tab$obsPK, decreasing=TRUE),]

View(prot_tab)
write_excel_csv(prot_tab, "protected_areas.csv")

############ Plotting exploration

######
###### Bot/Top 5%
index.bot1 <- which(prot.areas$obs.per.km2 < quantile(prot.areas$obs.per.km2, 0.1))
index.top1 <- which(prot.areas$obs.per.km2 > quantile(prot.areas$obs.per.km2, 0.9))

bot1.areas <- prot.areas[index.bot1,]
bot1.areas 
top1.areas <- prot.areas[index.top1,]
top1.areas

top1.areas[5,]$SITENAME <- "Haff Réimech - Taupeschwues"

centroidTB.df <- st_centroid(rbind(bot1.areas,top1.areas))

seed=666
ggplot() +
  geom_sf(data=lux_borders_2169, fill=NA, alpha=0.5) +
  geom_sf(data=bot1.areas, fill="darkred") + 
  geom_sf(data=top1.areas, fill="blue") + 
  geom_sf_text(data=centroidTB.df, aes(label=SITENAME),
               position=position_jitter(5000)) +
  theme_minimal()

###### Plot maps with fill values depending on metrics
### All PAs with obs per km2
ggplot() +
  geom_sf(data = lux_borders_2169, fill = NA, alpha = 0.5) +
  geom_sf(data = prot.areas, aes(fill = obs.per.km2)) +
  scale_fill_viridis_c(option = "plasma", trans = "log") +  # adjust palette & scaling
  theme_minimal()

### ZPINs with obs per km2
ggplot() +
  geom_sf(data = lux_borders_2169, fill = NA, alpha = 0.5) +
  geom_sf(data = prot.areas[prot.areas$type=="National",], aes(fill = obs.per.km2)) +
  geom_sf_text(data=st_centroid(prot.areas[prot.areas$type=="National",]),
               aes(label=SITENAME), position=position_jitter(5000), size=3) +
  scale_fill_viridis_c(option = "plasma", trans = "log") +  # adjust palette & scaling
  theme_minimal()

ggplot(prot_tab, aes(x = type, y = species, fill = type)) +
  stat_summary(fun = mean, geom = "bar", width = 0.7) +
  labs(
    title = "",
    x = "Type",
    y = "Average number of species"
  ) +
  theme_minimal()


ggplot(prot_tab, aes(x = type, y = obsPK, fill = type)) +
  stat_summary(fun = mean, geom = "bar", width = 0.7) +
  labs(
    title = "",
    x = "Type",
    y = "Average observations per km2"
  ) +
  theme_minimal()

######

