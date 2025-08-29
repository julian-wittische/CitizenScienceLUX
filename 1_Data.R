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

### New data (29-08-2025)
path <- normalizePath(paste0(DATAPATH,"data obs LUX/download290825"))
temp <- list.files(path=path, pattern="\\.csv$", full.names = TRUE)
inat <- lapply(temp, read.csv)
inat <- do.call(rbind, inat)
dim(inat) # difference with online number - just removed ones without coordinates?

### Remove observations with incomplete coordinates
inat  <- inat[complete.cases(inat$longitude),]
inat  <- inat[complete.cases(inat$latitude),]
dim(inat) #same number giving answer to difference - see above

###### Prepare subset for spatial analyses
 
### Verifiable, not obscured, and with geoaccuracy below 250m
verif250notobsc <- inat[inat$quality_grade!="casual" &
                          inat$coordinates_obscured=="false" & (
                          inat$public_positional_accuracy<=250|is.na(inat$public_positional_accuracy)),]

### Crop observations to only include ones from Luxembourg
# Administrative boundary
lux_borders <- readRDS("lux_borders.RDS")
lux_borders_2169 <- st_transform(lux_borders, crs="EPSG:2169")
# Just coords
coords <- verif250notobsc[,c("longitude","latitude")]
coords <- st_as_sf(x = coords, coords = c("longitude", "latitude"), crs = "EPSG:4326")
coords <- st_transform(coords, crs="EPSG:2169")
coords <- st_intersection(coords, lux_borders_2169)
crop_logical <- st_contains(lux_borders_2169, coords, sparse=FALSE)
# Go back to original file
verif250notobsc <- verif250notobsc[which(crop_logical==TRUE),]
# Add info back to sf object as fields
coords2 <- cbind(coords, verif250notobsc)

###### Saving data
save(inat, verif250notobsc, lux_borders, lux_borders_2169, coords, coords2, file="iNat.RData")
