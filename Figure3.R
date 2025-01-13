################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Make a figure about iNat VS national database

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