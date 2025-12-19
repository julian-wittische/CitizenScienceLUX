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
mini_m <- mdata[,c("Lat", "Long", "preferred","Taxon_Kingdom", "Taxon_Phylum",
                   "Taxon_Class", "Taxon_Order")]

### Keep only observations with species ID and geolocalisation (avoids old collection stuff)
mini_m <- mini_m[complete.cases(mini_m$preferred),]
mini_m <- mini_m[complete.cases(mini_m$Lat),]
mini_m <- mini_m[complete.cases(mini_m$Long),]

### Unique species
uniq_mdata <- mini_m %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(uniq_mdata$unique_count)
print(uniq_mdata, n=70)

inat_fig4 <- inatf4[inatf4$quality_grade=="research",]

uniq_inat <- inat_fig4 %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(uniq_inat$unique_count)
print(uniq_inat, n=80)

### Keep only strict mini_mmum columns
mini_m <- mdata_before[,c("Lat", "Long", "preferred","Taxon_Kingdom", "Taxon_Phylum",
                 "Taxon_Class", "Taxon_Order")]

### Keep only observations with species ID and geolocalisation (avoids old collection stuff)
mini_m <- mini_m[complete.cases(mini_m$preferred),]
mini_m <- mini_m[complete.cases(mini_m$Lat),]
mini_m <- mini_m[complete.cases(mini_m$Long),]

### Unique species
uniq_mdata <- mini_m %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(uniq_mdata$unique_count)
print(uniq_mdata, n=90)

nrow(mdata[mdata$Taxon_Order=="Coleoptera",])
nrow(inat_fig4[inat_fig4$taxon_order_name=="Coleoptera",])

# mdata species counts
uniq_mdata[uniq_mdata$Taxon_Order=="Coleoptera",]
uniq_mdata[uniq_mdata$Taxon_Order=="Lepidoptera",]
uniq_mdata[uniq_mdata$Taxon_Order=="Odonata",]
uniq_mdata[uniq_mdata$Taxon_Order=="Diptera",]

# mdata recent

# inat
uniq_inat[uniq_inat$taxon_order_name=="Coleoptera",]
uniq_inat[uniq_inat$taxon_order_name=="Lepidoptera",]
uniq_inat[uniq_inat$taxon_order_name=="Odonata",]
uniq_inat[uniq_inat$taxon_order_name=="Diptera",]

arrange(uniq_mdata[uniq_mdata$Taxon_Order %!in% uniq_inat$taxon_order_name,], desc(unique_count))

orders <- c("Coleoptera", "Lepidoptera", "Odonata", "Diptera",
            "Passeriformes", "Anseriformes","Charadriiformes","Accipitriformes",
            "Asterales", "Rosales", "Lamiales", "Caryophyllales",
            "Protura", "Siphonaptera", "Sordariales", "Naviculales")

groups <- c(rep("Insects", 4), rep("Birds", 4), rep("Flowering plants", 4), rep("Absent", 4))

f4_plot_df <- data.frame(orders, groups)

f4_plot_df$mdata_all <- uniq_mdata[uniq_mdata$Taxon_Order %in% orders,]$unique_count

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

############ Make a figure about iNat VS national database ----

inat_vs_mdata <- read.csv("./CITIZEN SCIENCE/iNaturalistLU/paper iNat/iNatVSmData.csv")

# Set up the horizontal bar plot
ggplot(inat_vs_mdata, aes(x = mdata, y = reorder(order, mdata), fill = group)) +
  geom_bar(stat = "identity", alpha = 0.3, color = "black", size = 0.5) + # Empty bars for mdata
  geom_bar(aes(x = inat), stat = "identity", alpha = 0.8) + # Filled bars for inat
  facet_wrap(~group, scales = "free", ncol = 1) + # Independent scales for both axes
  labs(
    title = "",
    x = "Number of species",
    y = "Orders"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )