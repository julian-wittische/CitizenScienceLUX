######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the taxonomic focus section

############ Load data ----
#load("iNat.RData") # Created using 1_Data.R

############ Loading libraries ----
source("0_Libraries.R")

############ Completeness of species inventory ----

source("Figure_iNat_VS_national.R")

############ Number of observations per species in each select taxa (based on DiCecco 2022) ----

###### Restrict to select taxa
dc_classes <- c("Liliopsida", "Magnoliopsida", 
                "Agaricomycetes", "Arachnida",
                "Insecta","Actinopterygii",
                "Amphibia", "Reptilia", "Aves","Mammalia")

inatf5 <- inat[inat$taxon_class_name %in% dc_classes & inat$taxon_species_name!="",]

###### Number of observations per species
num_sp_f5 <- as.data.frame(table(inatf5$taxon_species_name))
head(num_sp_f5 )

###### Associate select taxa names to species
uniq_figure_tax_f5 <- inatf5 %>%
  filter(taxon_species_name != "" & !is.na(taxon_species_name)) %>%  # REMOVE EMPTY SPECIES NAMES
  arrange(taxon_species_name) %>%
  group_by(taxon_species_name) %>%
  summarise(taxon_class_name = paste(unique(taxon_class_name), collapse = ", ")) %>%
  ungroup()

# Checkpoint
which(!(uniq_figure_tax_f5$taxon_species_name%in%num_sp_f5$Var1))

###### Combine
df_fig5 <- cbind(num_sp_f5, uniq_figure_tax_f5$taxon_class_name)

# Mean
barplot <- aggregate(df_fig5$Freq, by=list(df_fig5$`uniq_figure_tax_f5$taxon_class_name`), FUN=mean)
# Standard deviation
barplot <- cbind(barplot, aggregate(df_fig5$Freq, by=list(df_fig5$`uniq_figure_tax_f5$taxon_class_name`), FUN=sd))
# Cleaning
barplot <- barplot[,-3]
colnames(barplot) <- c("taxa", "mean", "sd")

# Adjust xmin to start at the bar edge (mean) to avoid overlap
barplot$xmin <- barplot$mean  # Lower limit starts at the bar
barplot$xmax <- barplot$mean + barplot$sd  # Upper limit includes SD

custom_colors <- c(
  "Liliopsida" = "#B2DF8A",
  "Magnoliopsida" = "#33A02C",
  "Agaricomycetes" = "#FB9A99",
  "Arachnida" = "#E31A1C",
  "Insecta" = "#FDBF6F",
  "Actinopterygii" = "#FF7F00",
  "Amphibia" = "#CAB2D6",
  "Reptilia" = "#6A3D9A",
  "Aves" = "#A6CEE3",
  "Mammalia" = "#1F78B4"
)

# Create the horizontal barplot with adjusted error bars
ggplot(barplot, aes(x = mean, y = reorder(taxa, mean), fill=taxa)) +  # reorder taxa by mean
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0) +  # Adjusted SD bars
  scale_fill_manual(values = custom_colors) +# horizontal bars
  labs(x = "Number of observations",
       y = ""
  ) +
  theme_minimal() + 
  theme(axis.text=element_text(size=26, colour = "black"),
        axis.title=element_text(size=28)) 

############ Other explorations ----

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