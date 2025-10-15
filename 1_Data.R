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
path <- normalizePath(paste0(DATAPATH,"data obs LUX/download260925"))
temp <- list.files(path=path, pattern="\\.csv$", full.names = TRUE)
inat <- lapply(temp, read.csv)
inat <- do.call(rbind, inat)
dim(inat) # difference with online number - just removed ones without coordinates?

### Remove observations with incomplete coordinates
inat  <- inat[complete.cases(inat$longitude),]
inat  <- inat[complete.cases(inat$latitude),]
dim(inat) #same number giving answer to difference - see above

############ Add protected, introduced, invasive statuses ----

###### Invasive (according to latest Neobiota national list - not EU)
inv <- read_xlsx(paste0(ENVIPATH,"neobiota_list.xlsx"))
inat$inv <- inat$scientific_name %in% inv$Species_name

###### Introduced
introd <- read.csv(paste0(ENVIPATH,"introd.csv"))
inat$introd <- inat$scientific_name %in% unique(introd$scientific_name)

### Harmonization: if invasive, must be introduced
inat$introd2 <- ifelse(inat$introd==T, inat$introd==T, inat$inv)
sum(inat$introd)
sum(inat$introd2)

# Check which ones are invasive but not introduced
unique(inat[inat$inv==T & inat$introd==F,"scientific_name"])

###### Protected
prosp_fauna <- read_xlsx(paste0(ENVIPATH,"espèces_protégées_luxembourg.xlsx"), sheet=1)
prosp_flora <- read_xlsx(paste0(ENVIPATH,"espèces_protégées_luxembourg.xlsx"), sheet=2)
prosp <- rbind(prosp_fauna, prosp_flora)
table(prosp$rank)

### Aggregate/Species complex
# Neotinea ustulata (only species of the genus here so use genus)
inat$prot <- ifelse(inat$taxon_genus_name=="Neotinea", TRUE, FALSE)

# Complex Dryopteris affinis
dryo_comp <- c("Dryopteris affinis", "Dryopteris borreri", "Dryopteris cambrensis",
  "Dryopteris carpatica", "Dryopteris lacunosa", "Dryopteris pseudodisjuncta")

# Pelophylax already covered

# Rhithrogena hybrida and semicolorata groups: maybe just use genus because all species are rare
rhi <- c("Rhithrogena")

# "Simple" species complex names
ssc <- unique(prosp[prosp$rank=="species complex", "Species"])[4:13,1]
ssc <- ssc$Species
ssc <- gsub('[\"“”„]', '', ssc)

expand_species <- function(s) {
  parts <- strsplit(s, " ")[[1]]
  if (length(parts) < 2) return(s)  # skip if malformed or single-word
  genus <- parts[1]
  species <- unlist(strsplit(parts[2], "/"))
  paste(genus, species)
}
ssc_expanded <- unlist(lapply(ssc, expand_species))

### Family level
fam <- unique(prosp[prosp$rank=="family", "Species"])[[1]]

### Form
form <- unique(prosp[prosp$rank=="form", "Species"])[[1]]

### Genus
genus <- unique(prosp[prosp$rank=="genus", "Species"])[[1]]

### Hybrid
hybrid <- unique(prosp[prosp$rank=="hybrid", "Species"])[[1]]

### Species
species <- unique(prosp[prosp$rank=="species", "Species"])[[1]]

### Subpecies
subspecies <- unique(prosp[prosp$rank=="subspecies", "Species"])[[1]]

### Variety
variety <- unique(prosp[prosp$rank=="variety", "Species"])[[1]]

### Global condition
condition <- inat$taxon_genus_name=="Neotinea" |
  inat$scientific_name %in% dryo_comp |
  inat$taxon_genus_name %in% rhi |
  inat$scientific_name %in% ssc_expanded |
  inat$taxon_family_name %in% fam |
  inat$taxon_form_name %in% form |
  inat$taxon_genus_name %in% genus |
  inat$taxon_hybrid_name %in% hybrid |
  inat$taxon_species_name %in% species |
  inat$taxon_subspecies_name %in% subspecies |
  inat$taxon_variety_name %in% variety

inat$prot <- ifelse(condition, TRUE, FALSE)

###### Prepare subset for spatial analyses
 
### Verifiable, not obscured, and with geoaccuracy below 250m

# Which percentile is 250m?
quant <- ecdf(inat$public_positional_accuracy)
quant(250)
# RESULT: 81%

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
coords2 <- st_as_sf(
  x = verif250notobsc[which(crop_logical==TRUE), ],  # Use the filtered dataframe
  coords = c("longitude", "latitude"), 
  crs = "EPSG:4326"
) %>%
  st_transform(crs = "EPSG:2169")

###### Saving data
#save(inat, verif250notobsc, lux_borders, lux_borders_2169, coords, coords2, file="iNat.RData")
st_write(coords, "verif_noObscured_goodprec.gpkg", append=FALSE)
