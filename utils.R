######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : Store all homemade functions used in the project
# Functions are not the main product of this project

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ In Luxembourg or not ----
luxornot <- function(coord){
  # Split and convert to numeric (note: sf uses lon, lat order)
  latlon <- as.numeric(strsplit(coord, ",")[[1]])
  point_sf <- st_sf(
    geometry = st_sfc(st_point(c(latlon[2], latlon[1])), crs = 4326)
  )
  
  return(c(st_within(point_sf, lux_borders, sparse = FALSE)))
}

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ Circular concentration ----
seasonality <- function(user_df) {
  if (nrow(user_df) == 0 || all(is.na(user_df$observed_on_details.date))) {
    return(data.frame(
      mean_month_deg = NA_real_,
      mean_month_numeric = NA_real_,
      rho = NA_real_
    ))
  }
  
  # Clean and ensure proper Date format, remove NAs
  user_df <- user_df %>%
    mutate(observed_on_details.date = as.Date(observed_on_details.date)) %>%
    filter(!is.na(observed_on_details.date))
  
  if (nrow(user_df) == 0) {
    return(data.frame(
      mean_month_deg = NA_real_,
      mean_month_numeric = NA_real_,
      rho = NA_real_
    ))
  }
  
  month_counts <- user_df %>%
    mutate(month = lubridate::month(observed_on_details.date)) %>%
    count(month)
  
  months_rep <- rep(month_counts$month, month_counts$n)
  
  # Use radians for correct circular math
  circ_months_rad <- circular(
    months_rep * 2 * pi / 12,
    units = "radians",
    template = "clock12",
    type = "angles"
  )
  
  mean_rad <- as.numeric(mean(circ_months_rad))
  mean_numeric <- (mean_rad %% (2 * pi)) * 12 / (2 * pi) + 1
  rho_val <- as.numeric(rho.circular(circ_months_rad))
  
  return(data.frame(
    mean_month_deg = mean_rad * 180 / pi,
    mean_month_numeric = mean_numeric,
    rho = rho_val
  ))
}

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ Rounded mean columns removing NAs and without scientific notation ----
numeric_column_means <- function(df) {
  df_num <- df[sapply(df, is.numeric)]
  means <- colMeans(df_num, na.rm = TRUE)
  means <- round(means, 2)
  options(scipen = 999)  # disable scientific notation globally
  return(means)
}

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ Cluster plots with stacked graphs like Di Cecco 2022 ----
taxo_spe <- function(inat_rat, cluster_n = 10, top_n = NULL, title = "") {
  inat_dist <- dist(inat_rat)
  hier <- hclust(inat_dist)
  sub_grp <- cutree(hier, k = cluster_n)
  
  cluster_sizes <- sort(table(sub_grp), decreasing = TRUE)
  
  # Create a mapping: old cluster ID → new cluster ID
  old_ids <- as.numeric(names(cluster_sizes))
  new_ids <- seq_along(old_ids)
  map <- setNames(new_ids, old_ids)
  
  # Apply mapping to rename clusters
  sub_grp_renamed <- map[as.character(sub_grp)]
  
  # Decide how many groups to keep
  if (is.null(top_n) || top_n > cluster_n) {
    top_n <- cluster_n
  }
  
  keep_ids <- seq_len(top_n)
  
  # Compute normalized means for each cluster
  data_matrix <- do.call(rbind, lapply(keep_ids, function(k) {
    v <- numeric_column_means(inat_rat[sub_grp_renamed == k, ])
    v / sum(v)  # normalize so row sums = 1
  }))
  
  # Convert to data frame and add row IDs
  data_df <- as.data.frame(data_matrix)
  data_df$row_id <- factor(keep_ids)
  
  # Reshape to long format for ggplot
  data_long <- data_df %>%
    pivot_longer(cols = -row_id, names_to = "class", values_to = "proportion")
  
  data_long$row_id <- factor(data_long$row_id, levels = rev(keep_ids))
  
  # Define colors
  class_cols <- data.frame(
    class = c("Liliopsida", "Magnoliopsida", "Agaricomycetes", "Arachnida",
              "Insecta", "Actinopterygii", "Amphibia", "Reptilia", "Aves", "Mammalia"),
    col = c("#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
            "#FF7F00", "#CAB2D6", "#6A3D9A", "#A6CEE3", "#1F78B4"),
    stringsAsFactors = FALSE
  )
  
  data_long$class <- factor(data_long$class, levels = class_cols$class)
  
  # Plot
  stacked_plot <- ggplot(data_long, aes(x = proportion, y = row_id, fill = class)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = setNames(class_cols$col, class_cols$class)) +
    labs(title = title,
         x = "Proportion of observations per class",
         y = "User group",
         fill = "Class") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(expand = c(0, 0))
  
  return(stacked_plot)
}

taxo_spe <- function(inat_rat, cluster_n = 10, top_n = NULL, title = "") {
  inat_dist <- dist(inat_rat)
  hier <- hclust(inat_dist)
  sub_grp <- cutree(hier, k = cluster_n)
  
  cluster_sizes <- sort(table(sub_grp), decreasing = TRUE)
  
  # Create mapping old cluster ID → new cluster ID
  old_ids <- as.numeric(names(cluster_sizes))
  new_ids <- seq_along(old_ids)
  map <- setNames(new_ids, old_ids)
  
  # Apply mapping
  sub_grp_renamed <- map[as.character(sub_grp)]
  
  # Decide how many groups to keep
  if (is.null(top_n) || top_n > cluster_n) {
    top_n <- cluster_n
  }
  keep_ids <- seq_len(top_n)
  
  # Compute normalized means for each cluster
  data_matrix <- do.call(rbind, lapply(keep_ids, function(k) {
    v <- numeric_column_means(inat_rat[sub_grp_renamed == k, ])
    v / sum(v)
  }))
  
  data_df <- as.data.frame(data_matrix)
  data_df$row_id <- factor(keep_ids)
  
  # Compute % observers per cluster
  total_obs <- length(sub_grp_renamed)
  cluster_perc <- cluster_sizes[keep_ids] / total_obs * 100
  perc_labels <- paste0(round(cluster_perc, 1), "%")
  label_df <- data.frame(
    row_id = factor(keep_ids, levels = keep_ids),
    perc = perc_labels,
    x_pos = 1.02  # position slightly beyond stacked bar
  )
  
  # Long format for ggplot
  data_long <- data_df %>%
    pivot_longer(cols = -row_id, names_to = "class", values_to = "proportion")
  
  data_long$row_id <- factor(data_long$row_id, levels = rev(keep_ids))
  label_df$row_id <- factor(label_df$row_id, levels = rev(keep_ids))
  
  # Colors
  class_cols <- data.frame(
    class = c("Liliopsida", "Magnoliopsida", "Agaricomycetes", "Arachnida",
              "Insecta", "Actinopterygii", "Amphibia", "Reptilia", "Aves", "Mammalia"),
    col = c("#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
            "#FF7F00", "#CAB2D6", "#6A3D9A", "#A6CEE3", "#1F78B4"),
    stringsAsFactors = FALSE
  )
  data_long$class <- factor(data_long$class, levels = class_cols$class)
  
  # Plot
  stacked_plot <- ggplot(data_long, aes(x = proportion, y = row_id, fill = class)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = setNames(class_cols$col, class_cols$class)) +
    geom_text(data = label_df, aes(x = x_pos, y = row_id, label = perc),
              inherit.aes = FALSE, hjust = 0, size = 3.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1.15)) + # extra space for labels
    labs(title = title,
         x = "Proportion of observations per class",
         y = "User group",
         fill = "Class") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  return(stacked_plot)
}

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ Opposite of %in% ----
'%!in%' <- function(x,y)!('%in%'(x,y))

###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
###############################################################################.
############ Find proportions of quality grade ----
qual_prop <- function(data, taxon){
  table(data[data$taxon_class_name==taxon,"quality_grade"])["research"]/sum(table(data[data$taxon_class_name==taxon,"quality_grade"]))
}


