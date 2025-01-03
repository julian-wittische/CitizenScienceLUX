################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script goal: group taxa to make a nice graph
################################################################################

######### Cleaning

# Remove casual observations
inatf <- inat[inat$quality_grade!="casual",]

# Remove observations without a kingdom, phylum, or class

inatf <- inatf[complete.cases(inatf$taxon_kingdom_name),]
inatf <- inatf[complete.cases(inatf$taxon_phylum_name),]
inatf <- inatf[complete.cases(inatf$taxon_class_name),]

# Remove Chromista, Bacteria, Protozoa
inatf <- inatf[!inatf$taxon_kingdom_name %in% c("Bacteria", "Protozoa", "Chromista"),]

# Remove Seek observations
inatf <- inatf[!inatf$oauth_application_name %in% c("Seek"),]

######### Define new groups

# Fill a new column with empty strings
inatf$figure_taxon_name <- character(nrow(inatf))

# Insects
inatf[inatf$taxon_class_name == "Insecta", "figure_taxon_name"] <- "Insects"

# Arachnids
inatf[inatf$taxon_class_name == "Arachnida", "figure_taxon_name"] <- "Arachnids"

# Non-bird vertebrates
inatf[inatf$taxon_subphylum_name == "Vertebrata" & !is.na(inatf$taxon_subphylum_name), "figure_taxon_name"] <- " Other vertebrates"

# Birds
inatf[inatf$taxon_class_name == "Aves", "figure_taxon_name"] <- "Birds"

# Other invertebrates
inatf[inatf$taxon_kingdom_name == "Animalia" & inatf$figure_taxon_name=="", "figure_taxon_name"] <- "Other invertebrates"

# Non-vascular plants
inatf[inatf$taxon_kingdom_name=="Plantae" & inatf$taxon_phylum_name != "Tracheophyta", "figure_taxon_name"] <- "Non-vascular plants"

# Flowering plants
inatf[inatf$taxon_subphylum_name == "Angiospermae" & !is.na(inatf$taxon_subphylum_name), "figure_taxon_name"] <- "Flowering plants"

# Non-flowering vascular plants
inatf[inatf$taxon_kingdom_name=="Plantae" & !inatf$figure_taxon_name %in% c("Non-vascular plants", "Flowering plants"), "figure_taxon_name"] <- "Non-flowering vascular plants"
          
# Fungi
inatf[inatf$taxon_kingdom_name == "Fungi", "figure_taxon_name"] <- "Fungi"

# # Any left
# table(inatf$figure_taxon_name, useNA = "ifany")
# 
# # Which are there
# table(inatf[inatf$figure_taxon_name=="", "taxon_kingdom_name"])

#write.csv(inatf[,c("oauth_application_name", "quality_grade", "figure_taxon_name")], "Data4Fig.csv")
