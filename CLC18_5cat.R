################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Load CLC 2018 data and reclassify into 5 categories 

############ Load data and crop it

### Load
# We need to remove Z dimension because it messed up later processing
clc_gr <- st_zm(st_read("~/Data/ENV_DATA_EUROPE/corine-land-cover-2018-1872-classes-0.geojson"))

### Crop
lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
clc_gr <- st_transform(clc_gr, crs="EPSG:2169")
clc_lux <- st_intersection(clc_gr, lux_borders)

############ Reclassify and group adjacent polygons of the same new category

### Reclassify
clc_lux_5cat <- clc_lux %>%
  mutate(
    Reclassified_LC = case_when(
      Code_18 %in% c("111", "112", "121", "122", "123", "124", "131", "132", "133", "141", "142") ~ 'Artificial',
      Code_18 %in% c("211", "212", "213", "221", "222", "223", "231", "241", "242", "243", "244") ~ 'Agriculture',
      Code_18 %in% c("311", "312", "313") ~ 'Natural/Semi-natural Forest',  # Removed "324" from forest
      Code_18 %in% c("321", "322", "323", "324", "331", "332", "333", "334", "335", "231", "411") ~ 'Natural/Semi-natural Non-forest',  # Included 324 and 231
      Code_18 %in% c("511", "512", "521", "522", "523") ~ 'Water bodies',
      TRUE ~ 'Unclassified'  # Optional catch-all for any other categories
    )
  )

clc_lux_5cat <- clc_lux_5cat %>%
  mutate(
    geometry = if_else( Reclassified_LC == "Water bodies", st_buffer(geometry, dist = 25), geometry)
  )


clc_lux_5cat <- clc_lux_5cat %>%
  group_by(Reclassified_LC) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

### Plot checkpoint
ggplot(clc_lux_5cat) +
  geom_sf(aes(fill = Reclassified_LC)) +  # Fill polygons based on the land cover category
  scale_fill_manual(values = c(
    'Artificial' = 'red',
    'Agriculture' = 'yellow',
    'Natural/Semi-natural Forest' = 'darkgreen',
    'Natural/Semi-natural Non-forest' = 'green',
    'Water bodies' = 'blue',
    'Unclassified' = 'gray'  # Optional for unclassified areas
  )) +
  theme_minimal() +
  labs(title = "Reclassified Corine Land Cover",
       fill = "Land Cover Category")

# ### Check Unclassified stuff
# # Get it
# unclassified_polygons <- clc_lux_5cat %>%
#   filter(Reclassified_LC == 'Unclassified')
# 
# # Identify unique Code_18 values in the Unclassified category
# unclassified_codes <- unclassified_polygons %>%
#   distinct(Code_18) %>%
#   pull(Code_18)
# 
# # Print the unique Code_18 values
# print(unclassified_codes)

### Group

############ Area

### Calculate area
clc_lux_5cat <- clc_lux_5cat %>%
  mutate(area_m2 = as.numeric(st_area(geometry)))

# Aggregate total area by category
area_by_category <- clc_lux_5cat %>%
  group_by(Reclassified_LC) %>%
  summarise(total_area_m2 = sum(area_m2, na.rm = TRUE))

# Calculate the total area of all categories
total_area <- sum(area_by_category$total_area_m2, na.rm = TRUE)

# Calculate percentage area by category
percentage_by_category <- area_by_category %>%
  mutate(percentage = (total_area_m2 / total_area) * 100)

# Print the results
print(percentage_by_category)

############ Export

st_write(clc_lux_5cat, "CLC18_vec_reclassified_5cat.geojson", delete_layer = TRUE)
