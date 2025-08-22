################################### TEST OF MY CLUSTERING WORKFLOW

############ Simulate users with strong preferences ---
###### Function: generate skewed probability vector
skewed_probs <- function(k, n = 10, alpha_base = 0.5, alpha_focus = 200) {
  alpha <- rep(alpha_base, n)
  alpha[k] <- alpha_focus
  as.numeric(rdirichlet(1, alpha))
}

### Test
skewed_probs(k = 7)
# Sum to 1?
sum(skewed_probs(k=7))

###### Create 1300 users 
set.seed(666)

library(MCMCpack)

make_skewed_matrix <- function(n_rows = 1300, n_cols = 10,
                               target_s = 0.8, alpha_base = 0.5, scale = 1,
                               seed = NULL, return_k = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  alpha_focus <- target_s * ((n_cols - 1) * alpha_base) / (1 - target_s)
  alpha_base <- alpha_base * scale
  alpha_focus <- alpha_focus * scale
  
  mat <- matrix(0, nrow = n_rows, ncol = n_cols)
  ks <- integer(n_rows)
  for (i in seq_len(n_rows)) {
    k <- sample.int(n_cols, 1)
    ks[i] <- k
    alpha <- rep(alpha_base, n_cols)
    alpha[k] <- alpha_focus
    mat[i, ] <- rdirichlet(1, alpha)
  }
  if (return_k) return(list(mat = mat, k = ks,
                            alpha_base = alpha_base, alpha_focus = alpha_focus))
  mat
}

prob_matrix <- as.data.frame(make_skewed_matrix(target_s = 0.5))
dim(prob_matrix)

taxa_gmm_df <- prob_matrix[,1:10]
colnames(taxa_gmm_df) <- c("Liliopsida", "Magnoliopsida", "Agaricomycetes",
                           "Arachnida", "Insecta","Actinopterygii",
                           "Amphibia", "Reptilia", "Aves","Mammalia")

gmm_test <- Mclust(taxa_gmm_df , G=10)
gmm_test$BIC

summary(gmm_test)
plot(gmm_test$BIC)
adjustedRandIndex(gmm_test$classification, prob_matrix$k)

cluster_n <- gmm_test$d

data_matrix <- do.call(rbind, lapply(1:cluster_n, function(k) {
  v <- numeric_column_means(taxa_gmm_df[gmm_test$classification == k, ])
  v / sum(v)  # normalize so row sums = 1
}))

# Convert to data frame and add row IDs
data_df <- as.data.frame(data_matrix)
data_df$row_id <- factor(1:cluster_n)

# Reshape to long format for ggplot
data_long <- data_df %>%
  pivot_longer(cols = -row_id, names_to = "class", values_to = "proportion")

# Define colors
class_cols <- data.frame(class = c("Liliopsida", "Magnoliopsida", 
                                   "Agaricomycetes", "Arachnida",
                                   "Insecta","Actinopterygii",
                                   "Amphibia", "Reptilia", "Aves","Mammalia"),
                         col = c("#B2DF8A", "#33A02C", "#FB9A99",
                                 "#E31A1C", "#FDBF6F", "#FF7F00",
                                 "#CAB2D6", "#6A3D9A", "#A6CEE3","#1F78B4"), stringsAsFactors = FALSE)

data_long$class <- factor(data_long$class, levels = class_cols$class)

# Create the horizontal stacked bar plot
ggplot(data_long, aes(x = proportion, y = row_id, fill = class)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = setNames(class_cols$col, class_cols$class)) +
  labs(title = "Proportion of Taxonomic Classes by Row",
       x = "Proportion",
       y = "Row",
       fill = "Class") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0))  # Remove padding on x-axis
