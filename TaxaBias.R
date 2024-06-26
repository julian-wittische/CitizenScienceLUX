################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ TO DO LIST
# Check with Paul if we want all, verifiable, or research

# Find the less observed classes of animals
anim <- res
anim <- anim[anim$taxon_kingdom_name=="Animalia",]
tab <- table(anim$taxon_class_name)
tab <- tab[order(tab,decreasing = FALSE)]
nrow(anim[anim$taxon_species_name=="Harmonia axyridis",]) #read paper
# Remove two to reach similar number of observations as the H. axyridis 
taxnames <- names(tab[c(1:11)])[c(-1,-7)]
taxnames
# Species names
unique(anim[anim$taxon_class_name%in%taxnames, "taxon_species_name"])
# Number of species in those 9 classes
length(unique(anim[anim$taxon_class_name%in%taxnames, "taxon_species_name"]))
table(anim[anim$taxon_class_name%in%taxnames, "taxon_species_name"])
# Total number of individuals
sum(table(anim[anim$taxon_class_name%in%taxnames, "taxon_species_name"]))

# Find the less observed classes of non-animals
nonanim <- res
nonanim <- nonanim[nonanim$taxon_kingdom_name!="Animalia",]

nrow(nonanim[nonanim$taxon_species_name=="Galanthus nivalis",])
tabn <- table(nonanim$taxon_class_name)
tabn <- tabn[order(tabn,decreasing = FALSE)]
sum(tabn[1:29])
taxnamesn <- names(tabn[c(1:29)])[c(-24)]
taxnamesn
unique(nonanim[nonanim$taxon_class_name%in%taxnamesn, "taxon_species_name"])
