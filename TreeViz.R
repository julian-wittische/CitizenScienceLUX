# library(ape)
# library(ggtree)
# library(ggplot2)
# 
# # Define a Newick tree string for Animalia to Class level
# tree_string <- "(((Eumetazoa, Parazoa), (Protostomia, Deuterostomia)), Animalia);"
# 
# # Read the tree
# phylo_tree <- read.tree(text = tree_string)
# 
# # Plot the tree using ggtree
# p <- ggtree(phylo_tree) +
#   geom_tiplab() +
#   theme_tree() +
#   ggtitle("Animalia Phylogenetic Tree (Class Level)")
# 
# print(p)

library(rotl)

taxa <- c("Cestoda", "Demospongiae", "Gordioida", "Hydrozoa", "Trematoda",
          "Symphyla", "Branchiopoda", "Phylactolaemata", "Hyperoartia", 
          "Bivalvia", "Chilopoda", "Clitellata", "Collembola", "Protura",
          "Actinopteri", "Diplura (order in Mandibulata)", "Insecta")

# taxa <- unique(inat$taxon_class_name)[c(-16)]
# taxa[taxa == "Ceratiomyxomycetes"] <- "Protosteliomycetes"
# 
# taxa[taxa == "Orbiliomycetes"] <- ""

matched_taxa <- tnrs_match_names(taxa)
print(matched_taxa)

ott_ids <- matched_taxa$ott_id
phylo_tree <- tol_induced_subtree(ott_ids = ott_ids, label_format = "name")
phylo_tree$tip.label[phylo_tree$tip.label=="Diplura_(order_in_Mandibulata)"] <- "Diplura"
plot(phylo_tree)
