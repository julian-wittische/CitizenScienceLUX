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

############ Load data ----
load(paste0(DATAPATH,"all_obs/all.RData"))

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





