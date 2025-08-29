################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: find aboutiNat completedness

######### iNaturalist
source("Libraries&Data.R")
source("C:/Users/YNM724/Desktop/Projects/CitizenScienceLUX/Taxa4Fig.R")

inat_fig3 <- inatf[inatf$quality_grade=="research",]

uniq_inat <- inat_fig3 %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(uniq_inat$unique_count)
print(uniq_inat, n=50)

### Keep only strict mini_mmum columns
mini_m <- mdata_before[,c("Lat", "Long", "preferred","Taxon_Kingdom", "Taxon_Phylum",
                 "Taxon_Class", "Taxon_Order")]

### Keep only observations with species ID and geolocalisation (avoids old collection stuff)
mini_m <- mini_m[complete.cases(mini_m$preferred),]
mini_m <- mini_m[complete.cases(mini_m$Lat),]
mini_m <- mini_m[complete.cases(mini_m$Long),]

### Unique species
uniq_mdata <- mini_m %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(uniq_mdata$unique_count)
print(uniq_mdata, n=70)

################################################################################
######### INSECT SUBPLOT
################################################################################

### mdata
insect_mdata <- mini_m[mini_m$Taxon_Class=="Insecta",]

insect_mdata <- insect_mdata %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(insect_mdata$unique_count)
print(insect_mdata, n=70)

### inat
insect_inat <- inat[inat$taxon_class_name=="Insecta",]

insect_inat <- insect_inat %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(insect_inat$unique_count)
print(insect_inat, n=70)


################################################################################
######### BIRDS SUBPLOT
################################################################################

### mdata
birds_mdata <- mini_m[mini_m$Taxon_Class=="Aves",]

birds_mdata <- birds_mdata %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(birds_mdata$unique_count)
print(birds_mdata, n=70)

### inat
birds_inat <- inat_fig3[inat_fig3$taxon_class_name=="Aves",]

birds_inat <- birds_inat %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(birds_inat$unique_count)
print(birds_inat, n=70)

################################################################################
######### FLOWERS SUBPLOT
################################################################################

### mdata
flowers_mdata <- mini_m[mini_m$Taxon_Class=="Magnoliopsida",]

flowers_mdata <- flowers_mdata %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(flowers_mdata$unique_count)
print(flowers_mdata, n=70)

### inat
flowers_inat <- inat_fig3[inat_fig3$taxon_class_name=="Magnoliopsida",]

flowers_inat <- flowers_inat %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(flowers_inat$unique_count)
print(flowers_inat, n=70)


################################################################################
######### FUNGI SUBPLOT
################################################################################

### mdata
fung_mdata <- mini_m[mini_m$Taxon_Class=="Ascomycota",]

fung_mdata <- fung_mdata %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(fung_mdata$unique_count)
print(fung_mdata, n=70)

### inat
fung_inat <- inat_fig3[inat_fig3$taxon_class_name=="Ascomycota",]

fung_inat <- fung_inat %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(fung_inat$unique_count)
print(fung_inat, n=70)