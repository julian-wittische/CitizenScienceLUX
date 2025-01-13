################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: analyses of the taxonomic focus section 
################################################################################

################################################################################
############ Completeness of species inventory

###### iNat species richness
inat_fig4 <- inatf[inatf$quality_grade=="research",]

uniq_inat <- inat_fig4 %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(uniq_inat$unique_count)
print(uniq_inat, n=50)

###### mData species richness
files <- list.files(paste0(ENVIPATH,"MDATA2024"), full.names = TRUE, pattern="*.csv")
mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))

### Keep only observations made by May 10 2024
mdata_before <- mdata
# Problem with dates - QUICK DIRTY FIX
mdata_before$date_end[1:1000000] <- "01/01/1900"
table(is.na(mdata$date_end))
#
mdata_before$date_end <- parse_date_time(mdata_before$date_end, 
                                         orders = c("d/m/Y", "Y-m-d"))

table(is.na(mdata_before$date_end))

mdata_before <- mdata_before[mdata_before$date_end <= as.Date("10/05/2024","%d/%m/%Y"),]
dim(mdata_before)
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
print(uniq_mdata, n=70)

###### INSECT SUBPLOT

### mdata
insect_mdata <- mini_m[mini_m$Taxon_Class=="Insecta",]

insect_mdata <- insect_mdata %>%
  group_by(Taxon_Order) %>%
  summarise(unique_count = n_distinct(preferred))

hist(insect_mdata$unique_count)
print(insect_mdata, n=70)

### inat
insect_inat <- inat_fig4[inat_fig4$taxon_class_name=="Insecta",]

insect_inat <- insect_inat %>%
  group_by(taxon_order_name) %>%
  summarise(unique_count = n_distinct(taxon_species_name))

hist(insect_inat$unique_count)
print(insect_inat, n=70)

################################################################################
############ Number of observations per species in each iconic taxa

###### Iconic taxa
source("Taxa4Fig.R")

###### Number of observations per species
num_sp <- as.data.frame(table(inatf$taxon_species_name))

###### Associate iconic taxa names to species
uniq_figure_tax <- inatf %>%
  arrange(taxon_species_name) %>%  # Order by species_taxon_name alphabetically
  group_by(taxon_species_name) %>%
  summarise(taxon_figure_name = paste(unique(taxon_figure_name), collapse = ", ")) %>%
  ungroup()

# Problem
which(!(uniq_figure_tax$taxon_species_name%in%num_sp$Var1))

# Remove NA
uniq_figure_tax <- uniq_figure_tax[-nrow(uniq_figure_tax),]

###### Combine
df_fig5 <- cbind(num_sp, uniq_figure_tax$taxon_figure_name)

# Mean
barplot <- aggregate(df_fig5$Freq, by=list(df_fig5$`uniq_figure_tax$taxon_figure_name`), FUN=mean)
# Standard deviation
barplot <- cbind(barplot, aggregate(df_fig5$Freq, by=list(df_fig5$`uniq_figure_tax$taxon_figure_name`), FUN=sd))
# Cleaning
barplot <- barplot[,-3]
colnames(barplot) <- c("taxa", "mean", "sd")

###### Plotting
ggplot(barplot, aes(x = mean, y = reorder(taxa, mean))) +  # reorder taxa by mean
  geom_bar(stat = "identity", fill = "skyblue") +       # horizontal bars
  geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0.2) +  # SD bars
  labs(
    title = "",
    x = "Number of observations per species",
    y = ""
  ) +
  theme_minimal()

# Adjust xmin to start at the bar edge (mean) to avoid overlap
barplot$xmin <- barplot$mean  # Lower limit starts at the bar
barplot$xmax <- barplot$mean + barplot$sd  # Upper limit includes SD

custom_colors <- c("Arachnids" = "red",
                   "Birds" = "blue",
                   "Flowering plants" = "green",
                   "Fungi" = "orange",
                   "Insects" = "pink",
                   "Non-flowering vascular plants" = "purple",
                   "Non-vascular plants" = "yellow",
                   "Other invertebrates" = "black",
                   "Other vertebrates" = "grey")

# Create the horizontal barplot with adjusted error bars
ggplot(barplot, aes(x = mean, y = reorder(taxa, mean), fill=taxa)) +  # reorder taxa by mean
  geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0) +  # Adjusted SD bars
  scale_fill_manual(values = custom_colors) +# horizontal bars
  labs(x = "Number of observations",
    y = ""
  ) +
  theme_minimal() + 
  theme(axis.text=element_text(size=16, colour = "black"),
        axis.title=element_text(size=18))



################################################################################
############ Other explorations

### Casual species exploration

# Only casual observation
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


