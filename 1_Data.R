######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load and clean up data

############ Local configuration ----
source("config.R")

############ Loading libraries ----
source("0_Libraries.R")

############ Load and preprocess observations ----

### Original data
inat <- read_excel(paste0(DATAPATH,"MASTER_inat-lux-combined.xlsx"), sheet = 5)
inat <- as.data.frame(inat)
inat_all <- inat

### Remove observations with incomplete coordinates
inat  <- inat[complete.cases(inat$longitude),]
inat  <- inat[complete.cases(inat$latitude),]

### Prepare subset for spatial analyses
 
# Verifiable, not obscured, and with geoaccuracy below 250m
verif250notobsc <- inat[inat$quality_grade!="casual" &
                          inat$coordinates_obscured==FALSE & (
                          inat$public_positional_accuracy<=250|is.na(inat$public_positional_accuracy)),]

# Crop observations to only include ones from Luxembourg
lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
coords <- verif250notobsc[,c("longitude","latitude")]
coords <- st_as_sf(x = coords, coords = c("longitude", "latitude"), crs = "EPSG:4326")
coords <- st_transform(coords, crs="EPSG:2169")
coords <- st_intersection(coords, lux_borders)
crop_logical <- st_contains(lux_borders, coords, sparse=FALSE)
verif250notobsc <- verif250notobsc[which(crop_logical==TRUE),]

# Add info back to sf object as fields
coords2 <- cbind(coords, verif250notobsc)

############ Ad hoc selection of iconic taxa ----

inatf <- inat[inat$quality_grade!="casual",]

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


######
save(inat_all, inat, inatverif250notobsc, lux_borders, coords, coords2, inatf, file="iNat.RData")
