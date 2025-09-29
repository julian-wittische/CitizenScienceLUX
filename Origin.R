######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out origin of visitors

# 1. Load raw
world <-  st_read("/media/MNHN-SUPERCOMPUTER/jwittische/_ENV_DATA_WRLD/GADM&EEZ.gpkg")
#world <- ne_countries(scale = "medium", returnclass = "sf")
#world2 <- st_cast(world, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

# 2. Cast MULTISURFACE → MULTIPOLYGON (safe conversion)
world_cast <- st_cast(world, "MULTIPOLYGON")

# 3. Clean invalid geometries
world_clean <- st_make_valid(world_cast)

# 4. Check geometry types
table(st_geometry_type(world_clean))

# keep only non-empty polygonal features
world_poly <- world_clean[st_geometry_type(world_clean) %in% c("POLYGON","MULTIPOLYGON"), ]

################################################################################
################################################################################
################################################################################
################################################################################

ggplot(world2) + geom_sf()

obs_vis <- all[all$user.id %in% vis,]

# Split and convert to numeric (note: sf uses lon, lat order)
latlon <- do.call(rbind, strsplit(obs_vis$location, ","))
latlon <- apply(latlon, 2, as.numeric)
latlon <- cbind(obs_vis$user.id, latlon)
colnames(latlon) <- c("user.id", "latitude", "longitude")
point_sf <- st_as_sf(as.data.frame(latlon), coords = c("longitude", "latitude"), crs = 4326)

world_poly_buffer <- st_buffer(world_poly, dist = 1e-5)
obs_with_country <- as.data.frame(st_join(point_sf, world_poly_buffer["Sovereign"]))


sum(is.na(obs_with_country$Sovereign))
which(is.na(obs_with_country$Sovereign))

### PROBLEM LOCATIONS (NO COUNTRIES): OFTEN WATER/OCEAN
pts_prob <- st_as_sf(obs_with_country[is.na(obs_with_country$Sovereign),])
mv <- mapview(pts_prob, alpha.regions = 0.15, col.regions = "darkviolet")
mv

### WRITE FUNCTION TO FIND DISTANCE TO NEAREST COUNTRY POLYGON

obs_vis$country <- obs_with_country$Sovereign

home <- obs_vis %>%
  group_by(user.id) %>%
  count(country, sort = TRUE) %>%   # count occurrences per user.id / name
  slice_max(n, n = 2, with_ties = FALSE) %>%  # keep most common name per user
  ungroup()





totals <- obs_vis %>% count(user.id, name = "total_obs")

# add total to home
home <- home %>% left_join(totals, by = "user.id")

home50 <- home[home$total_obs>50,]
home50[home50$country=="Luxembourg",]

length(table(home50$country))
sort_countr <- as.data.frame(sort(table(home50$country), decreasing = TRUE))
colnames(sort_countr) <- c("Tourist region", "Number")
sum(table(home50$country), decreasing = TRUE)



############ Highest use relative to number of tourists ----
# This is likely overestimated for tourists from neighboring countries
### Load data from STATEC
tou23 <- read_xlsx(paste0(DATAPATH,"Tourists2023.xlsx"))
# Remove all non-letters at the beginning
tou23$`Tourist region` <- sub("^[^A-Za-z]+", "", tou23$`Tourist region`)
# Merge
tou23 <- merge(tou23, sort_countr, by = "Tourist region", all.x = TRUE)
# Check
View(tou23)

### Calculate perc of use
tou23$perc <- tou23$Number/tou23$Total*100
# Check
View(tou23)

############ Plot of observations from visitors with colors for country of origin ----
###
obs_with_country <- merge(home50, latlon, by = "user.id", all.x = TRUE)

pts <- st_as_sf(obs_with_country, coords=c("longitude", "latitude"), crs=crs(lux_borders))
pts <- pts[st_within(pts,lux_borders, sparse = FALSE), ]

top_count <- c("Belgium","France", "Germany", "Netherlands", "Other countries", "United States of America")

pts$country_red <- ifelse(pts$country %in% top_count, pts$country, "Other countries")

color_countries <- c("red", "blue", "pink", "orange", "grey", "black")

ggplot(pts) +
  geom_sf(aes(color = country_red), size = 0.5, alpha=0.8) +
  scale_color_manual(values = color_countries) +
  theme_minimal()

ggplot(pts) +
  geom_sf(aes(color = country_red), size = 1) +
  theme_minimal()


color_countries <- c("red", "blue", "pink", "orange", "grey", "black")

mv <- mapview(pts, alpha.regions = 0.15, col.regions = "lightblue",
              layer.name = "Luxembourg", zcol = origin,
              col.regions = color_countries)
mv

top_count <- c("Belgium","France", "Germany", "Netherlands", "Other countries", "United States of America")

pts$name2 <- ifelse(pts$name %!in% top_count, pts$name, "Other country") 

############ Most active visitors relative to number of observers ----
# vis_act <- aggregate(n ~ country, data = home50, 
#                             FUN = function(x) c(sum = sum(x), n = length(x)))
# 
# # Flatten the result
# agg_out <- data.frame(
#   country = agg$country,
#   sum = agg$value[ , "sum"],
#   n   = agg$value[ , "n"]
# )
# agg_out
