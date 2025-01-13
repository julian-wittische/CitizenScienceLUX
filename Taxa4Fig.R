################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Ad hoc selection of iconic taxa for plotting 
################################################################################

############ Loading
source("Libraries&Data.R")

############ Cleaning

inatf <- noncas 

# Remove observations without a kingdom, phylum, or class

inatf <- inatf[complete.cases(inatf$taxon_kingdom_name),]
inatf <- inatf[complete.cases(inatf$taxon_phylum_name),]
inatf <- inatf[complete.cases(inatf$taxon_class_name),]

# Remove Chromista, Bacteria, Protozoa
inatf <- inatf[!inatf$taxon_kingdom_name %in% c("Bacteria", "Protozoa", "Chromista"),]

# Remove Seek observations
inatf <- inatf[!inatf$oauth_application_name %in% c("Seek"),]

############ Define new groups

# Fill a new column with empty strings
inatf$taxon_figure_name <- character(nrow(inatf))

# Insects
inatf[inatf$taxon_class_name == "Insecta", "taxon_figure_name"] <- "Insects"

# Arachnids
inatf[inatf$taxon_class_name == "Arachnida", "taxon_figure_name"] <- "Arachnids"

# Non-bird vertebrates
inatf[inatf$taxon_subphylum_name == "Vertebrata" & !is.na(inatf$taxon_subphylum_name), "taxon_figure_name"] <- "Other vertebrates"

# Birds
inatf[inatf$taxon_class_name == "Aves", "taxon_figure_name"] <- "Birds"

# Other invertebrates
inatf[inatf$taxon_kingdom_name == "Animalia" & inatf$taxon_figure_name=="", "taxon_figure_name"] <- "Other invertebrates"

# Non-vascular plants
inatf[inatf$taxon_kingdom_name=="Plantae" & inatf$taxon_phylum_name != "Tracheophyta", "taxon_figure_name"] <- "Non-vascular plants"

# Flowering plants
inatf[inatf$taxon_subphylum_name == "Angiospermae" & !is.na(inatf$taxon_subphylum_name), "taxon_figure_name"] <- "Flowering plants"

# Non-flowering vascular plants
inatf[inatf$taxon_kingdom_name=="Plantae" & !inatf$taxon_figure_name %in% c("Non-vascular plants", "Flowering plants"), "taxon_figure_name"] <- "Non-flowering vascular plants"
          
# Fungi
inatf[inatf$taxon_kingdom_name == "Fungi", "taxon_figure_name"] <- "Fungi"

# # Any left
# table(inatf$taxon_figure_name, useNA = "ifany")
# 
# # Which are there
# table(inatf[inatf$taxon_figure_name=="", "taxon_kingdom_name"])

# write.csv(inatf[,c("oauth_application_name", "quality_grade", "taxon_figure_name")], "Data4Fig.csv")
