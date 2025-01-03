################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Make a figure about iNat VS national database

### Source iconic taxa
source("C:/Users/YNM724/Desktop/Projects/CitizenScienceLUX/Taxa4Fig.R")

inat_fig3 <- inatf[inatf$quality_grade=="research",]

uniq_inat <- inat_fig3 %>%
  group_by(taxon_class_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(uniq_inat$unique_count)
print(uniq_inat, n=50)
