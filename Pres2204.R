library(dplyr)

species_counts <- inatf %>%
  group_by(taxon_figure_name) %>%
  summarise(unique_species = n_distinct(taxon_species_name, na.rm = TRUE)) %>%
  arrange(desc(unique_species))  # Optional: sort by descending count


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
  labs(x = "Number of species",
       y = ""
  ) +
  theme_minimal() + 
  theme(axis.text=element_text(size=16, colour = "black"),
        axis.title=element_text(size=18))



##########################

library(dplyr)
library(ggplot2)

# Calculate unique species counts per group (replace 'inatf' with your actual data)
species_counts <- inatf %>%
  group_by(taxon_figure_name) %>%
  summarise(
    mean = n_distinct(taxon_species_name, na.rm = TRUE),  # Unique species count
    sd = sd(n_distinct(taxon_species_name), na.rm = TRUE)  # Optional: SD if needed
  ) %>%
  mutate(
    xmin = mean - sd,  # Lower bound for error bars (set to 0 if negative)
    xmax = mean + sd   # Upper bound for error bars
  ) %>%
  arrange(desc(mean))  # Sort by descending mean

# Ensure xmin doesn't go below 0
species_counts$xmin <- pmax(species_counts$xmin, 0)

# Custom colors (same as yours)
custom_colors <- c(
  "Arachnids" = "red",
  "Birds" = "blue",
  "Flowering plants" = "green",
  "Fungi" = "orange",
  "Insects" = "pink",
  "Non-flowering vascular plants" = "purple",
  "Non-vascular plants" = "yellow",
  "Other invertebrates" = "black",
  "Other vertebrates" = "grey"
)

# Horizontal barplot with your preferred styling
ggplot(species_counts, aes(
  x = mean, 
  y = reorder(taxon_figure_name, mean),  # Order taxa by mean
  fill = taxon_figure_name
)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbarh(
    aes(xmin = xmin, xmax = xmax), 
    height = 0.2,  # Adjust height of error bars (0.2 for slight visibility)
    color = "black",
    linewidth = 0.8  # Thicker error bars
  ) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Number of unique species",  # Updated label
    y = ""  # Empty y-axis label (as per your preference)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 18),
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank()     # Remove minor grid lines
  )