
###### Taxa ID DWCA
taxa_ID <- read.csv(paste0(DATAPATH, "taxa.csv"))
load(paste0(DATAPATH,"all_obs/all.RData"))
lux_borders <- readRDS("lux_borders.RDS")
all <- all[!is.na(all$location),]
latlon_mat <- do.call(rbind, strsplit(all$location, ","))
latlon_mat <- apply(latlon_mat, 2, as.numeric)
points_sf <- st_as_sf(
  data.frame(lon = latlon_mat[,2], lat = latlon_mat[,1]),
  coords = c("lon", "lat"),
  crs = 4326
)
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

# Count class per observer
user_taxa <- all2[, .N, by = .(user.id, class)]
user_taxa <- as.data.frame(user_taxa)

# Remove stuff without class
user_taxa <- user_taxa[user_taxa$class!="",]

# Only pick classes from Di Cecco 2021
dc_classes <- c("Liliopsida", "Magnoliopsida", 
                "Agaricomycetes", "Arachnida",
                "Insecta","Actinopterygii",
                "Amphibia", "Reptilia", "Aves","Mammalia")

# Find proportions for each of the top 10 classes per user
inat_user_classes <- user_taxa %>%
  group_by(user.id) %>%
  mutate(obs_total = sum(N)) %>%
  filter(obs_total > 50, class %in% dc_classes) %>%
  mutate(obs_filtered = sum(N)) %>%  # recompute after filtering
  ungroup() %>%
  mutate(prop_obs = N / obs_filtered) %>%
  dplyr::select(user.id, class, obs_total, obs_filtered, prop_obs)

# Reshape data to wide format
inat_classes_wide <- pivot_wider(inat_user_classes, values_from = prop_obs, names_from = class)
inat_classes_wide[is.na(inat_classes_wide)] <- 0
# Checkpoint
rowSums(inat_classes_wide[,4:13], na.rm=TRUE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Distance matrix
classes_dist <- dist(inat_classes_wide)

# Clustering for classes
clust_classes <- hclust(classes_dist)

# Proportion of observations by class
user_obs <- inat_user_classes %>%
  filter(class %in% dc_classes) %>%
  group_by(user.id) %>%
  summarize(obs = sum(obs_filtered))

class_total_obs <- sum(user_obs$obs)

g <- 10

sub_grp <- cutree(clust_classes, k = g)

classes_df <- inat_classes_wide %>%
  mutate(cluster = sub_grp)

groups_classes <- classes_df %>%
  dplyr::select(user.id, cluster) 

pct_users <- groups_classes %>%
  left_join(inat_user_classes) %>%
  group_by(cluster) %>%
  summarize(n_user = n_distinct(user.id),
            prop_all_obs = 100*sum(obs_filtered)/class_total_obs) %>%
  mutate(total_user = sum(n_user),
         prop_user = n_user/total_user,
         pct_user = prop_user*100,
         rank = dense_rank(pct_user),
         rev_rank = dense_rank(desc(pct_user)))

taxon_grps <- inat_user_classes %>%
  left_join(groups_classes) %>%
  group_by(cluster, class) %>%
  summarize(mean_prop = mean(prop_obs)) %>%
  group_by(cluster) %>%
  mutate(total = sum(mean_prop)) %>%
  mutate(mean_prop_scaled = mean_prop/total)

top_10_class <- taxon_grps %>%
  group_by(class) %>%
  summarize(total_mean = mean(mean_prop_scaled)) %>%
  arrange(desc(total_mean)) %>%
  mutate(class_plot = ifelse(row_number() > 10, "Other", class))

paired_cols <- RColorBrewer::brewer.pal(10, "Paired")

class_cols <- data.frame(class = c("Liliopsida", "Magnoliopsida", 
                                   "Agaricomycetes", "Arachnida",
                                   "Insecta","Actinopterygii",
                                   "Amphibia", "Reptilia", "Aves","Mammalia"),
                         col = c("#B2DF8A", "#33A02C", "#FB9A99",
                                 "#E31A1C", "#FDBF6F", "#FF7F00",
                                 "#CAB2D6", "#6A3D9A", "#A6CEE3","#1F78B4"), stringsAsFactors = F)

cols <- class_cols$col
names(cols) <- class_cols$class

class_grp_plot <- taxon_grps %>%
  left_join(top_10_class) %>%
  group_by(class_plot, cluster) %>%
  summarize(mean = sum(mean_prop_scaled)) %>%
  left_join(pct_users) %>%
  mutate(group_label = factor(rev_rank, levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"))) 

cluster_plot <- ggplot(class_grp_plot, aes(x = group_label, y = mean, 
                                           fill = fct_relevel(class_plot, class_cols$class))) + 
  geom_col(position = "stack") + scale_fill_manual(values = cols) + 
  labs(x = "Group", y = "Mean proportion of observations", fill = "Class") + coord_flip() +
  theme(legend.position = "left")

cluster_plot

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

taxa_gmm_df <- as.data.frame(inat_classes_wide[,4:13])
taxa_gmm_df_hel <- as.data.frame(vegan::decostand(taxa_gmm_df, method = "hellinger"))

gmm_fit <- Mclust(taxa_gmm_df, G=1:20)
summary(gmm_fit)
plot(gmm_fit)

one <- taxa_gmm_df[gmm_fit$classification==1,]
rowSums(one)
five <- taxa_gmm_df[gmm_fit$classification==5,]
nine <- taxa_gmm_df[gmm_fit$classification==9,]

rbind(numeric_column_means(one),
      numeric_column_means(five),
      numeric_column_means(nine))
