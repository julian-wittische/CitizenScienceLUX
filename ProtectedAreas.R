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
#source("0_Libraries.R")
library(sf)
library(ggplot2)

############ Loading Natura2000 data ----
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

############ Loading ZPIN data ----
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

# Homogenization
zpin <- zpin_temp
zpin <- zpin[,c("SITECODE","SITENAME")]
zpin$SITECODE <- as.character(zpin$SITECODE)
zpin$type <- "National"

############ Combine and join observations----
prot.areas <- rbind(ludh, ludo, zpin)
st_crs(prot.areas) <- st_crs(coords2)

# Spatial join with observations
joined_prot <- st_join(coords2, prot.areas, left=FALSE)

# Checkpoint
sum(table(prot.areas$SITENAME)>1)
which(table(prot.areas$SITENAME)>1)

############ Calculate protected area metrics ----
###### Main metrics bloc
metrics <- joined_prot %>%
  st_drop_geometry() %>%
  group_by(SITENAME) %>%
  summarise(
    observations = n(),
    observers = n_distinct(user_login),
    species =  n_distinct(scientific_name[str_detect(scientific_name, "\\w+ \\w+")]),
    introd = n_distinct(scientific_name[introd2 == TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    inv = n_distinct(scientific_name[inv==TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    prot = n_distinct(scientific_name[prot==TRUE & str_detect(scientific_name, "\\w+ \\w+")]),
    .groups = "drop"
  )

###### Spatial join and adding empty protected areas
prot.areas <- prot.areas %>%
  left_join(metrics, by = "SITENAME") %>%
  mutate(across(c(observers, observations, species, introd, inv, prot), ~replace_na(.x, 0)))

###### Introduced, invasive, protected rates
prot.areas$introd.rate <- prot.areas$introd/prot.areas$species
prot.areas$inv.rate <- prot.areas$inv/prot.areas$species
prot.areas$prot.rate <- prot.areas$prot/prot.areas$species

###### Per km^2
prot.areas$area <- as.numeric(st_area(prot.areas))/1000000 # convertin m2 in km2
prot.areas$obs.per.km2 <- prot.areas$observations / prot.areas$area
prot.areas$observers.per.km2 <- prot.areas$observers / prot.areas$area
prot.areas$spe.per.km2 <- prot.areas$species / prot.areas$area

############ Exploration ----
### Mean and SD
mean(prot.areas$obs.per.km2)
sd(prot.areas$obs.per.km2)

### Min/Max
View(prot.areas)

############ Area size effect on obs/km2 ----
mean(prot.areas$observations)
var(prot.areas$observations)

library(DHARMa)
library(glmmTMB)
library(marginaleffects)

mod <- glmer(observations ~ area + type + (1|SITENAME),
               data=prot.areas, family = poisson)


summary(mod)
sim <- simulateResiduals(mod, refit=T, n=99)
plotSimulatedResiduals(sim)
r2(mod)
model_performance(mod)
r.squaredGLMM(mod)
testDispersion(sim)
avg_slopes(mod, component="conditional")


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

############ Exports ----
############ Make table S1 ----

prot.areas <- prot.areas[order(prot.areas$, decreasing=TRUE),]

write_excel_csv(prot_tab, "protected_areas.csv")



