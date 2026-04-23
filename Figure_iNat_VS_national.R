################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: find aboutiNat completedness

######### iNaturalist ----

###### Load utility functions
source("Code/utils.R")

###### Load iconic taxa 

###### iNat species richness
inatf4 <- inat[inat$quality_grade=="research"|inat$quality_grade=="needs_id",]

uniq_inat <- inatf4 %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(uniq_inat$unique_count)
print(uniq_inat, n=50)

###### mData species richness
files <- list.files(paste0(DATAPATH,"MDATA2025"), full.names = TRUE, pattern="*.csv")
mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))

### Keep only strict mini_mmum columns
mini_mb <- mdata[as.Date(mdata$date_end)>=as.Date("2018-01-01"),c("Lat", "Long", "preferred","Taxon_Kingdom", "Taxon_Phylum",
                                                                  "Taxon_Class", "Taxon_Order")]

### Keep only observations with species ID and geolocalisation (avoids old collection stuff)
mini_mb <- mini_mb[complete.cases(mini_m$preferred),]
mini_mb <- mini_mb[complete.cases(mini_m$Lat),]
mini_mb <- mini_mb[complete.cases(mini_m$Long),]

### Keep only strict mini_mmum columns
mini_m <- mdata[,c("Lat", "Long", "preferred","Taxon_Kingdom", "Taxon_Phylum",
                   "Taxon_Class", "Taxon_Order")]

### Keep only observations with species ID and geolocalisation (avoids old collection stuff)
mini_m <- mini_m[complete.cases(mini_m$preferred),]
mini_m <- mini_m[complete.cases(mini_m$Lat),]
mini_m <- mini_m[complete.cases(mini_m$Long),]

### Unique species
inat_fig4 <- inatf4[inatf4$quality_grade=="research",]

uniq_inat <- inat_fig4 %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))%>%
  tidyr::complete(taxon_order_name, fill = list(unique_count = 0))

uniq_mdatab <- mini_mb %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))%>%
  tidyr::complete(Taxon_Order, fill = list(unique_count = 0))

uniq_mdata <- mini_m %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))%>%
  tidyr::complete(Taxon_Order, fill = list(unique_count = 0))

# ############ SUPER IMPORTANT LINE
# arrange(uniq_mdata[uniq_mdata$Taxon_Order %!in% uniq_inat$taxon_order_name,], desc(unique_count))

orders <- c("Coleoptera", "Lepidoptera", "Diptera", "Hymenoptera",
            "Passeriformes", "Anseriformes","Charadriiformes","Accipitriformes",
            "Rosales", "Asterales", "Fabales", "Caryophyllales",
            "Polyporales", "Boletales", "Cantharellales", "Sordariales")

groups <- c(rep("Insecta", 4), rep("Aves", 4), rep("Magnoliopsida", 4), rep("Agaricomycetes", 4))

f4_plot_df <- data.frame(orders, groups)
f4_plot_df$inat <- ifelse(is.na(m <- match(orders, uniq_inat$taxon_order_name)), 0,
                          uniq_inat$unique_count[m])


f4_plot_df$mdata_post18 <- ifelse(is.na(m <- match(orders, uniq_mdatab$Taxon_Order)), 0,
                                  uniq_mdatab$unique_count[m])

f4_plot_df$mdata_all <- ifelse(is.na(m <- match(orders, uniq_mdata$Taxon_Order)), 0,
                               uniq_mdata$unique_count[m])

f4_plot_df

################################################################################

# Reshape to long format for all 3 sources
f4_plot_long <- f4_plot_df %>%
  pivot_longer(
    cols = c(inat, mdata_post18, mdata_all),
    names_to = "source",
    values_to = "n_species"
  ) %>%
  mutate(
    source = factor(source, levels = c("mdata_all", "mdata_post18", "inat"))  # Order for layering
  )

order_levels <- f4_plot_long %>%
  filter(source == "mdata_all") %>%
  arrange(n_species) %>%
  pull(orders)

f4_plot_long <- f4_plot_long %>%
  mutate(orders = factor(orders, levels = order_levels))

# Define colors and alpha
custom_colors_inatnat <- c(
  "Magnoliopsida" = "#33A02C",
  "Agaricomycetes"= "#FB9A99",
  "Insecta"       = "#FDBF6F",
  "Aves"          = "#A6CEE3"
)

source_alpha <- c(
  "inat"        = 1.0,
  "mdata_post18" = 0.6,
  "mdata_all"   = 0.3
)


ggplot(f4_plot_long, aes(
  x    = n_species,
  y    = orders,
  fill = groups,
  alpha = source
)) +
  geom_bar(stat = "identity", color = "black", size = 0.3,
           position = "identity") +
  facet_wrap(~groups, scales = "free", ncol = 1) +
  scale_fill_manual(values = custom_colors_inatnat) +
  scale_alpha_manual(
    values = source_alpha,
    labels = c("mdata_all"    = "National database (all time)",
               "mdata_post18" = "National database (post-2018)",
               "inat"         = "iNaturalist"),
    breaks = c("inat", "mdata_post18", "mdata_all")
  ) +
  guides(fill = "none") +
  labs(
    x     = "Number of species",
    y     = "Orders",
    alpha = "Source"
  ) +
  theme_minimal() +
  theme(
    strip.text      = element_text(size = 12, face = "bold"),
    axis.text.x     = element_text(size = 12),
    axis.text.y     = element_text(size = 12),
    legend.position = "bottom"
  )
################################################################################
################################################################################
################################################################################
################################################################################

inatVSmdata <- ggplot(f4_plot_long, aes(
  x    = n_species,
  y    = orders,
  fill = groups,
  alpha = source,
  color = "black"
)) +
  geom_bar(stat = "identity", color = NA, size = 0.3,
           position = "identity", width = 0.8) +
  geom_bar(
    data = f4_plot_long %>% filter(source == "mdata_all"),
    aes(x = n_species, y = orders),
    stat = "identity", fill = NA, color = "black", size = 0.3,
    position = "identity", inherit.aes = FALSE, width = 0.8
  ) +
  geom_bar(
    data = f4_plot_long %>% filter(source == "mdata_post18"),
    aes(x = n_species, y = orders),
    stat = "identity", fill = NA, color = "black", size = 0.3,
    position = "identity", inherit.aes = FALSE, width = 0.8
  ) +
  geom_bar(
    data = f4_plot_long %>% filter(source == "inat"),
    aes(x = n_species, y = orders),
    stat = "identity", fill = NA, color = "black", size = 0.3,
    position = "identity", inherit.aes = FALSE, width = 0.8
  ) +
  facet_wrap(~groups, scales = "free", ncol = 1) +
  scale_fill_manual(values = custom_colors_inatnat) +
  scale_alpha_manual(
    values = source_alpha,
    labels = c("mdata_all"    = "National database (all time)",
               "mdata_post18" = "National database (post-2018)",
               "inat"         = "iNaturalist"),
    breaks = c("inat", "mdata_post18", "mdata_all")
  ) +
  guides(fill = "none") +
  labs(
    x     = "Number of species",
    y     = "",
    alpha = "Source"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    strip.text      = element_text(size = 16, face = "bold", hjust=0),
    axis.text.x     = element_text(size = 14, color = "black"),
    axis.text.y     = element_text(size = 14, color = "black", margin = margin(r = -32)),
    axis.title      = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.text     = element_text(size = 12, color = "black"),
    legend.title    = element_text(size = 14, color = "black"),
    axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid      = element_blank()
  )

#####################
ggsave("inatVSmdata.pdf", device="pdf", plot = inatVSmdata,
       width = 15, height = 10, dpi = 600, bg = "white")
