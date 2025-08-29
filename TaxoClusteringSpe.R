######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : Analyses of observer taxonomic preferences

###############################################################################.
############  Loading and preparing relevant data ----
###### Loading
### Taxa ID DWCA
taxa_ID <- read.csv(paste0(DATAPATH, "taxa.csv"))

### All obs from observers with at least 1 obs in Luxembourg
load(paste0(DATAPATH,"all_obs/all.RData"))

######  Preparing
### Remove observations without location
all <- all[!is.na(all$location),]

### Make it an sf object
latlon_mat <- do.call(rbind, strsplit(all$location, ","))
latlon_mat <- apply(latlon_mat, 2, as.numeric)
points_sf <- st_as_sf(
  data.frame(lon = latlon_mat[,2], lat = latlon_mat[,1]),
  coords = c("lon", "lat"),
  crs = 4326
)

### Add logical info whether observation is in Luxembourg
luxornot <- st_within(points_sf, lux_borders, sparse = FALSE)[,1]
all$luxornot <- luxornot

taxa_ID <- taxa_ID[taxa_ID$id %in% unique(all$taxon.id),] 
all2 <- all

# Convert to data.table
setDT(all2)
all2[, .N]
setDT(taxa_ID)

# Make sure `id` in taxa_ID and `community_taxon_id` in all_df are integers
all2 <- all2[, taxon.id := as.integer(taxon.id)]
all2[, .N]
taxa_ID <- taxa_ID[, id := as.integer(id)]

# Join in taxonomy info
all2 <- all2[taxa_ID, on = .(taxon.id = id)]
all2[, .N]

###############################################################################.
############  Calculate taxonomic metrics ----
###### Class level
### Count class per observer
user_taxa <- all2[, .N, by = .(user.id, class)]
user_taxa <- as.data.frame(user_taxa)

### Remove stuff without class
user_taxa <- user_taxa[user_taxa$class!="",]

# Only pick classes from Di Cecco 2021
dc_classes <- c("Liliopsida", "Magnoliopsida", 
                "Agaricomycetes", "Arachnida",
                "Insecta","Actinopterygii",
                "Amphibia", "Reptilia", "Aves","Mammalia")

### Find proportions for each of the top 10 classes per user
inat_user_classes <- user_taxa %>%
  group_by(user.id) %>%
  mutate(obs_total = sum(N)) %>%
  filter(obs_total > 50, class %in% dc_classes) %>%
  mutate(obs_filtered = sum(N)) %>%  # recompute after filtering
  ungroup() %>%
  mutate(prop_obs = N / obs_filtered) %>%
  dplyr::select(user.id, class, obs_total, obs_filtered, prop_obs)

### Reshape data to wide format
inat_classes_wide <- pivot_wider(inat_user_classes, values_from = prop_obs, names_from = class)
inat_classes_wide[is.na(inat_classes_wide)] <- 0
# Checkpoint
rowSums(inat_classes_wide[,4:13], na.rm=TRUE)

############  Clustering based on top 10 class proportions ----
res <- user_char[user_char$RorV=="Resident",]$user.id
vis <- user_char[user_char$RorV=="Visitor",]$user.id

inat_rat <- inat_classes_wide[,4:13]
inat_rat_res <- inat_classes_wide[inat_classes_wide$user.id %in% res, 4:13]
inat_rat_vis <- inat_classes_wide[inat_classes_wide$user.id %in% vis, 4:13]

taxo_spe(inat_rat, 10)
taxo_spe(inat_rat, 10, top_n=5)
taxo_spe(inat_rat_res, 10, "Residents")
taxo_spe(inat_rat_vis, 10, "Visitors")

grid.arrange(taxo_spe(inat_rat_res, 10, "Residents", top_n=5),
             taxo_spe(inat_rat_vis, 10, "Visitors", top_n=5),
             ncol = 1)
