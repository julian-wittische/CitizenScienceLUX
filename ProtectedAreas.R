######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load protected areas data and calculate metrics for each 

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

###### Load ZPIN data

zpin <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"zpin-declarees.zip"),options = enc.opt.z)
zpin <- st_zm(zpin)
# ERROR: at least 1 geometry not valid
zpin <- st_make_valid(zpin)
# ERROR: A\r\n and 1 instead of A
# Correction
zpin[107,"SOUSZONE"] <- "A"
zpin[120, "SOUSZONE"] <- "A"

# Problem Laangmuer
ggplot() + geom_sf(data=zpin[zpin$NOM=="Laangmuer",])

# zpin$NATCODE <- gsub(" ", "", zpin$NATCODE, fixed = TRUE)
# which(zpin$NATCODE=="")
zpin <- zpin[order(zpin$PRIE_ID),]
zpin_temp <- zpin %>%   group_by(PRIE_ID) %>%   summarise(SITECODE= first(PRIE_ID), SITENAME=first(NOM), geometry = st_union(geometry))

# Checkpoint
ggplot() + geom_sf(data=zpin_temp[zpin_temp$SITENAME=="Laangmuer",])

zpin <- zpin_temp
zpin <- zpin[,c("SITECODE","SITENAME")]
zpin$SITECODE <- as.character(zpin$SITECODE)

zpin$type <- "National"

###### Combine
prot.areas <- rbind(ludh, ludo, zpin)

############ Calculate various metrics ----

###### Number of observations in protected areas
in.prot <- st_intersects(prot.areas, coords)

###### Number of observations per protected area
prot.areas$per.prot <- lengths(in.prot)

###### Observations/km^2 per protected area
prot.areas$area.per.prot <- as.numeric(st_area(prot.areas))/1000000 # convertin m2 in km2
prot.areas$obs.per.km2 <- prot.areas$per.prot / prot.areas$area.per.prot

index.bot1 <- which(prot.areas$obs.per.km2 < quantile(prot.areas$obs.per.km2, 0.1))
index.top1 <- which(prot.areas$obs.per.km2 > quantile(prot.areas$obs.per.km2, 0.9))

bot1.areas <- prot.areas[index.bot1,]
bot1.areas 
top1.areas <- prot.areas[index.top1,]
top1.areas

top1.areas[5,]$SITENAME <- "Haff Réimech - Taupeschwues"

centroid.df <- st_centroid(rbind(bot1.areas,top1.areas))

seed=666
ggplot() +
  geom_sf(data=lux_borders, fill=NA, alpha=0.5) +
  geom_sf(data=bot1.areas, fill="darkred") + 
  geom_sf(data=top1.areas, fill="blue") + 
  geom_sf_text(data=centroid.df, aes(label=SITENAME),position=position_jitter(5000)) +
  theme_minimal()

###### Number of species per protected area
# Spatial join 
joined_prot <- st_join(coords2, prot.areas, left=FALSE)

# Species
species_prot <- joined_prot %>%
  filter(str_detect(scientific_name, "\\w+ \\w+")) %>%
  group_by(SITENAME) %>%
  summarise(species = n_distinct(scientific_name))

unique(prot.areas$SITENAME)[which(!unique(prot.areas$SITENAME)%in%species_prot$SITENAME)]
# "Schimpach - Carrières de Schimpach" has zero observations and hence zero species

### Add it to the dataset
zeroprot <- prot.areas[prot.areas$SITENAME=="Schimpach - Carrières de Schimpach",]
zeroprot$species <- 0
zeroprot <- zeroprot[, c("SITENAME", "species", "geometry")]
species_prot <- rbind(species_prot, zeroprot)

prot.areas <- prot.areas %>%
  left_join(species_prot %>% st_drop_geometry(), by = "SITENAME")


prot.areas <- st_join(prot.areas, species_prot, left=FALSE)

############ Make table S1 ----
prot_tab <- data.frame(prot_name=prot.areas$SITENAME,
                          obsPK=round(prot.areas$obs.per.km2, 2),
                          surface=round(prot.areas$area.per.prot, 2),
                          type=prot.areas$type,
                          species=prot.areas$species)

prot_tab <- prot_tab[order(prot_tab$obsPK, decreasing=TRUE),]

View(prot_tab)

############ Area size analysis ----
mod <- glmer(per.prot ~ area.per.prot + (1|SITENAME), data=prot.areas, family = poisson)
sim <- simulateResiduals(mod, refit=T, n=99)
plotSimulatedResiduals(sim)


# Example data
data <- data.frame(
  type = c("A", "A", "B", "B", "C", "C"),
  species = c(10, 15, 20, 25, 30, 35)
)


# Calculate averages and plot
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

