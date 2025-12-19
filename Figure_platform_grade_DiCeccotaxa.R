######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Fall 2025
# Data: MNHNL
# Script objective : Preparing data for figure for platform, grade, select taxa

############ Ad hoc selection of iconic taxa ----

inatf <- inat[inat$quality_grade!="casual",]

# Remove observations without a kingdom, phylum, or class

inatf <- inatf[complete.cases(inatf$taxon_kingdom_name),]
inatf <- inatf[complete.cases(inatf$taxon_phylum_name),]
inatf <- inatf[complete.cases(inatf$taxon_class_name),]

# Prepare upload method
map <- c(
  "2" = "Android",
  "3" = "iOS",
  "333" = "Seek",
  "843" = "Next"
)

inatf$oauth_application_name <- map[as.character(inatf$oauth_application_id)]
inatf$oauth_application_name[is.na(inatf$oauth_application_id)] <- "Web"


# Remove Seek and Next observations
inatf <- inatf[!inatf$oauth_application_name %in% c("Seek", "Next"),]

### Only pick classes from Di Cecco 2021
dc_classes <- c("Liliopsida", "Magnoliopsida", 
                "Agaricomycetes", "Arachnida",
                "Insecta","Actinopterygii",
                "Amphibia", "Reptilia", "Aves","Mammalia")

### Only pick classes from Di Cecco 2021 and remove those with tiny values
dc_classes <- c("Liliopsida", "Magnoliopsida", 
                "Agaricomycetes", "Arachnida",
                "Insecta", "Reptilia", "Aves","Mammalia")

inatf <- inatf[inatf$taxon_class_name %in% dc_classes,]

###### Calculations
source("utils.R")
qual_prop(inatf, "Insecta")
qual_prop(inatf, "Magnoliopsida")
qual_prop(inatf, "Liliopsida")
qual_prop(inatf, "Aves")
qual_prop(inatf, "Agaricomycetes")
qual_prop(inatf, "Arachnida")

df <- inatf

################################################################################
################################################################################
################################################################################

# Replace labels in the original dataframe BEFORE processing
df <- df %>%
  mutate(quality_grade = case_when(
    quality_grade == "research" ~ "Research",
    quality_grade == "needs_id" ~ "Needs ID", 
    TRUE ~ quality_grade
  ))

# Prepare the data with groups
prepare_sankey_data_with_groups <- function(df, class_cols) {
  # Create platform -> quality links
  links1 <- df %>%
    count(oauth_application_name, quality_grade) %>%
    rename(source = oauth_application_name, target = quality_grade, value = n)
  
  # Create quality -> class links  
  links2 <- df %>%
    count(quality_grade, taxon_class_name) %>%
    rename(source = quality_grade, target = taxon_class_name, value = n)
  
  # Combine links
  all_links <- bind_rows(links1, links2)
  
  # Create nodes (unique across all steps)
  nodes <- data.frame(
    name = unique(c(links1$source, links1$target, links2$target)),
    stringsAsFactors = FALSE
  )
  
  # Add node groups - use cleaned names for internal mapping
  nodes <- nodes %>%
    mutate(
      display_name = name,  # Keep beautiful names for display
      group = case_when(
        name %in% class_cols$class ~ name,
        name %in% unique(df$quality_grade) ~ gsub(" ", "_", name),  # Clean spaces
        TRUE ~ name
      )
    )
  
  # Convert names to indices using display names
  all_links$source <- match(all_links$source, nodes$display_name) - 1
  all_links$target <- match(all_links$target, nodes$display_name) - 1
  
  # Add link groups using cleaned names
  all_links <- all_links %>%
    mutate(
      source_name = nodes$display_name[source + 1],
      target_name = nodes$display_name[target + 1],
      link_group = case_when(
        target_name %in% unique(df$quality_grade) ~ paste0("link_", gsub(" ", "_", source_name)),
        target_name %in% class_cols$class ~ paste0("link_", target_name),
        TRUE ~ "other"
      )
    )
  
  return(list(nodes = nodes, links = all_links))
}

# Prepare the data with groups
sankey_data <- prepare_sankey_data_with_groups(df, class_cols)

# Get unique platforms and quality grades for left side
platforms <- unique(df$oauth_application_name)
quality_grades <- unique(df$quality_grade)

# Create color scale - start with left side (platforms and quality grades)
domain <- c()
range <- c()

# Assign distinct greys to platforms (dark shades)
platform_colors <- c("#000000", "#666666", "#D9D9D9")
for(i in 1:length(platforms)) {
  domain <- c(domain, platforms[i])
  range <- c(range, platform_colors[(i-1) %% length(platform_colors) + 1])
  
  domain <- c(domain, paste0("link_", platforms[i]))
  range <- c(range, paste0(platform_colors[(i-1) %% length(platform_colors) + 1], "CC"))
}

# Assign white to quality grades - USE CLEANED NAMES
quality_colors <- c("#FFFFFF", "#FFFFFF") 
for(i in 1:length(quality_grades)) {
  clean_name <- gsub(" ", "_", quality_grades[i])  # Clean the name
  domain <- c(domain, clean_name)
  range <- c(range, quality_colors[(i-1) %% length(quality_colors) + 1])
  
  domain <- c(domain, paste0("link_", clean_name))
  range <- c(range, paste0(quality_colors[(i-1) %% length(quality_colors) + 1], "CC"))
}

# Add class colors
for(i in 1:nrow(class_cols)) {
  class_name <- class_cols$class[i]
  class_color <- class_cols$col[i]
  
  domain <- c(domain, class_name)
  range <- c(range, class_color)
  
  transparent_color <- paste0("rgba(", 
                              paste(col2rgb(class_color)[,1], collapse = ","), 
                              ",0.99)")
  domain <- c(domain, paste0("link_", class_name))
  range <- c(range, transparent_color)
}

# Create the color scale JavaScript
my_color <- JS(paste0('
d3.scaleOrdinal()
  .domain([', paste0('"', domain, '"', collapse = ", "), '])
  .range([', paste0('"', range, '"', collapse = ", "), '])
'))

# Create the Sankey diagram - use display_name for beautiful labels
lol <- sankeyNetwork(
  Links = sankey_data$links,
  Nodes = sankey_data$nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "display_name",  # Use beautiful display names
  NodeGroup = "group",       # Use cleaned groups for internal mapping
  LinkGroup = "link_group",
  colourScale = my_color,
  fontSize = 44,
  fontFamily = "sans-serif",
  nodeWidth = 30,
  nodePadding = 10,
  sinksRight = FALSE,
  iterations = 0
)

# Save to HTML
library(htmlwidgets)
saveWidget(lol, "sankey_diagram.html")