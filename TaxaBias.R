################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ TO DO LIST

################################################################################

######### Casual species exploration

### Only casual observation
cas <- inat[inat$quality_grade=="casual",]

# Common taxa
sort(table(cas$taxon_species_name), decreasing=TRUE)[1:15]

### Only needs ID
need <- inat[inat$quality_grade=="needs_id",]

# Common taxa
sort(table(need$taxon_species_name), decreasing=TRUE)[1:15]
sum((need$taxon_class_name=="Aves" & !is.na(need$sound_url)), na.rm=TRUE)

# Pelophylax
sum((need$taxon_genus_name=="Pelophylax" & !is.na(need$image_url)), na.rm=TRUE)
sum((inat$taxon_genus_name=="Pelophylax" & !is.na(inat$image_url)) & inat$quality_grade=="research", na.rm=TRUE)

# Find the less observed classes of animals
anim <- verif250notobsc
anim <- anim[anim$taxon_kingdom_name=="Animalia",]
tab <- table(anim$taxon_class_name)
tab <- tab[order(tab,decreasing = FALSE)]
sum(anim$taxon_species_name=="Harmonia axyridis", na.rm=TRUE) #read paper
# Check number of classes to reach similar number of observations as the H. axyridis 
taxnames <- names(tab[c(1:14)])
taxnames # Weird Petrozonti name
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
