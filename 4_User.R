######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the user focus section

############ Local configuration ----
source("config.R")

############ Loading libraries ----
source("0_Libraries.R")

############ Loading functions ----
source("utils.R")

############ Load data ----
load(paste0(DATAPATH,"all_obs/all.RData"))
lux_borders <- readRDS("lux_borders.RDS")
all <- all[!is.na(all$location),]
latlon_mat <- do.call(rbind, strsplit(all$location, ","))
latlon_mat <- apply(latlon_mat, 2, as.numeric)
points_sf <- st_as_sf(
  data.frame(lon = latlon_mat[,2], lat = latlon_mat[,1]),
  coords = c("lon", "lat"),
  crs = 4326
)
luxornot <-st_within(points_sf, lux_borders, sparse = FALSE)[,1]

############ Activity metrics ----

###### Initialize
observers <- sort(unique(all$user.id))

### TESTING (REMOVE) ###
observers <- observers[1:10]

num <- numeric(length=length(observers))
chr <- character(length=length(observers))

user_char <- data.frame(active_days = num, #number of active days (observed on)
                        active_days_lu = num, #number of active days in Luxembourg (observed on)
                        active_period = num, #full period of activity (uploaded at); we use created at to avoid issues with scanning old photos
                        perc_o_lu = num, #% of observations in Luxembourg (compared to global)
                        perc_d_lu = num, #% of active days in Luxembourg (compared to global)
                        h = num, #residency index based on Dimson & Gillespie (2023); between 0 and 1, >0.5 means resident
                        RorV = chr, #residency interpretation: resident or visitor
                        maxobs = num) #maximum observations in one day

for (i in 1: length(observers)){
  user_data <- all %>% filter(user.id == observers[i])
  user_char[i, "active_days"] <- length(table(user_data$observed_on_details.date))
  user_char[i, "active_days_lu"] <- length(table(user_data[user_data$luxornot==TRUE,]$observed_on_details.date))
  user_char[i, "active_period"] <- as.numeric(max(as.Date(user_data$created_at_details.date)) - min(as.Date(user_data$created_at_details.date)))
  user_char[i, "perc_o_lu"] <- round(sum(user_data$luxornot)/nrow(user_data), 5) 
  user_char[i, "maxobs"] <- max(table(user_data$observed_on_details.date))
  cat(paste("User",i,observers[i]),"---","\n")
}
user_char$perc_d_lu <- round((user_char$active_days_lu / user_char$active_days), 5)
user_char$h <- user_char$perc_o_lu*0.4 + user_char$perc_d_lu*0.6
user_char$RorV <- ifelse(user_char$h > 0.5, "Resident", "Visitor")

############ Taxonomic observer specialization ----

### ###### Split observers into resident and visitors
source("ResidentVisitor.R")

# Data-driven way to find inflexion point

length(table(all$user.id))
sum(table(all$user.id)>100)
hist(table(all$user.id), breaks=3000, xlim=c(0,1500))

library(inflection)
user_obs_sorted <- log(sort(table(all$user.id)))
names(user_obs_sorted) <- NULL
infl <- inflection::uik(1:length(user_obs_sorted), user_obs_sorted)
threshold <- user_obs_sorted[infl]
exp(threshold)

hist(user_obs_sorted, type = "l", main = "Sorted User Observation Counts")
abline(v = threshold, col = "red", lty = 2)

sum(table(all$user.id)>75)



############ Identifiers ----


############ Activity clustering ----
source("Activity.R")


############ Territory ----



###### For locals (residents), define their "territory" in terms of size and 





