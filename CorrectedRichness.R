######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load communes data and calculate metrics for each 

###### Loading the required dataset with commune information ----
#source("Code/Communes.R")

###### Creating the species x sites data frame ----
# Species by site df
speXsite <- st_drop_geometry(joined) # keeping sf creates issues/slows
speXsite <- speXsite %>%
  mutate(scientific_name = str_squish(scientific_name)) %>% # remove extra whitespace
  filter(str_detect(scientific_name, "^[A-Za-z]+ [A-Za-z]+$")) %>% # exactly two words
  count(scientific_name, COMMUNE) %>%
  pivot_wider(names_from = COMMUNE,
              values_from = n,
              values_fill = 0) %>%
  column_to_rownames("scientific_name")

###### Running the species richness estimators (iNEXT package)
result <- iNEXT(speXsite, q = c(0, 1, 2), datatype = "abundance", knots = 100) # 100 to avoid issues with slice_min for commune with huge numbers

###### Find reasonable endpoint
### Species observed
hist(result$DataInfo$S.obs)
summary(result$DataInfo$S.obs)
### Observation number
hist(result$DataInfo$n)
summary(result$DataInfo$n) # Median: 1722

################################################################################
###### Finding exactly the one we want

# Get coverage at reference sample size for each site
site_info <- result$DataInfo %>%
  dplyr::select(Assemblage, n, SC)

# Plot to find natural break (as we discussed)
site_info %>%
  ggplot(aes(x = n, y = SC)) +
  geom_point() +
  geom_text(aes(label = Assemblage), size = 2, hjust = -0.1) +
  scale_x_log10() +
  labs(x = "Sample size (log scale)", y = "Coverage at reference n")

# Then apply your chosen threshold, e.g. SC > 0.6
good_sites <- site_info %>%
  filter(SC > 0.2) %>%   
  pull(Assemblage)

# Hill numbers (q=1/Shannon entropy) per municipality but comparing size-based estimates

# Find your rarefaction target
target_n <- site_info %>%
  pull(n) %>%
  quantile(0.05)  # 5th percentile of retained sites

result.targets <- iNEXT(speXsite, q = c(0, 1, 2), datatype = "abundance", size = c(370,1722,33822))
result.targets$iNextEst$size_based[result.targets$iNextEst$size_based=="Luxembourg",]
result.targets$iNextEst$size_based[result.targets$iNextEst$size_based=="Biwer",]
# Extract size-based estimates at that n
m0.05 <- result.targets$iNextEst$size_based %>%
  filter(Assemblage %in% good_sites,
         Order.q == 1) %>%
  group_by(Assemblage) %>%
          slice(1) %>%
  dplyr::select(Assemblage, m, Method, SC, qD, qD.LCL, qD.UCL)

mmed <- result.targets$iNextEst$size_based %>%
  filter(Assemblage %in% good_sites,
         Order.q == 1) %>%
  group_by(Assemblage) %>%
  slice(2) %>%
  dplyr::select(Assemblage, m, Method, SC, qD, qD.LCL, qD.UCL)

mmax <- result.targets$iNextEst$size_based %>%
  filter(Assemblage %in% good_sites,
         Order.q == 1) %>%
  group_by(Assemblage) %>%
  slice(3) %>%
  dplyr::select(Assemblage, m, Method, SC, qD, qD.LCL, qD.UCL)

result$DataInfo <- result$DataInfo[order(result$DataInfo$Assemblage),]
m0.05 <- m0.05[order(m0.05$Assemblage),]
mmed <- mmed[order(mmed$Assemblage),]
mmax <- mmax[order(mmax$Assemblage),]
plot(result$DataInfo$S.obs, m0.05$qD)
plot(result$DataInfo$S.obs, mmed$qD)
plot(result$DataInfo$S.obs, mmax$qD)

# Remove the bot and top X most extremes
extreme.com <- c(slice_min(result$DataInfo, order_by = n, n=5)$Assemblage,
  slice_max(result$DataInfo, order_by = n, n=5)$Assemblage)

m0.05.noextrm <- m0.05[m0.05$Assemblage %!in% extreme.com,]
res.noextrm <- result$DataInfo[result$DataInfo$Assemblage %!in% extreme.com,]
plot(res.noextrm$S.obs, m0.05.noextrm$qD, xlim=c(0,2100))


# Add it back to the spatial file

com <- st_join(com, standardized_n[,c("m", "qD" "Assemblage")], left=FALSE)   