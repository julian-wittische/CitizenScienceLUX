######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the user focus section

############ Local configuration ----
source("config.R")

############ Load data ----
#load(paste0(DATAPATH,"iNat.RData")) # Created using 1_Data.R

###### All observations from Luxembourg users

# load(paste0(DATAPATH,"all_obs/ALL_OBS.RData"))
# 
# col_sizes <- sapply(ALL_OBS, object.size)
# col_sizes_mb <- sort(col_sizes / (1024^2), decreasing = TRUE)
# print(round(col_sizes_mb, 2))
# print(round(col_sizes_mb, 2)/sum(col_sizes_mb)*100)

# Find all observation objects 
lf <- list.files(path = paste0(DATAPATH,"all_obs/"), pattern = "^ALL_OBS", full.names = TRUE)
# Initialize list
obj_ls <- vector("list", length(lf)) 
# Loop to go through each and remove useless but sizable columns
for (i in 1:length(lf)){
  print(lf[i])
  load(lf[i])
  obj_ls[[i]] <- ALL_OBS[ , !(names(ALL_OBS) %in% c("identifications", "non_owner_ids",
                                                    "observation_photos", "photos", "annotations",
                                                    "project_observations", "outlinks",
                                                    "comments", "votes", "faves"))]
  rm(ALL_OBS)
  gc()
}

lapply(obj_ls, dim)

keep_common_columns <- function(df_list) {
  # Find common column names across all list elements
  common_cols <- Reduce(intersect, lapply(df_list, colnames))
  
  # Subset each data frame to those common columns
  lapply(df_list, function(df) df[, common_cols, drop = FALSE])
}

obj_ls <- keep_common_columns(obj_ls)

all <- do.call(rbind, obj_ls)

save(all, file=paste0(DATAPATH,"all_obs/all.RData"))

############ Local configuration ----
source("config.R")

############ Loading libraries ----
source("0_Libraries.R")

############ Identifiers ----


############ Activity clustering ----
source("Activity.R")


############ Territory ----

###### Split observers into resident and visitors
source("ResidentVisitor.R")

###### For locals (residents), define their "territory" in terms of size and 

############ User specialization ----

### Taxonomic

# How many users have more than 100

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





