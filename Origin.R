######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out origin of visitors

world <- ne_countries(scale = "medium", returnclass = "sf")

obs_vis <- all[all$user.id %in% vis,]

# Split and convert to numeric (note: sf uses lon, lat order)
latlon <- do.call(rbind, strsplit(obs_vis$location, ","))
latlon <- apply(latlon, 2, as.numeric)
colnames(latlon) <- c("latitude", "longitude") 
point_sf <- st_as_sf(as.data.frame(latlon), coords = c("longitude", "latitude"), crs = 4326)

obs_with_country <- as.data.frame(st_join(point_sf, world["name"]))

obs_vis$country <- obs_with_country$name

home <- obs_vis %>%
  group_by(user.id) %>%
  count(country, sort = TRUE) %>%   # count occurrences per user.id / name
  slice_max(n, n = 1, with_ties = FALSE) %>%  # keep most common name per user
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

############ Most active visitors relative to number of observers ----
vis_act <- aggregate(n ~ country, data = home50, 
                            FUN = function(x) c(sum = sum(x), n = length(x)))

# Flatten the result
agg_out <- data.frame(
  country = agg$country,
  sum = agg$value[ , "sum"],
  n   = agg$value[ , "n"]
)
agg_out

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
