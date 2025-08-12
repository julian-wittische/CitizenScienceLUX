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

###### We need to get rid of users below a certain number of observations
# Di Cecco 2022 suggest 50 without any justification
# Data-driven way to find inflection point

library(inflection)
user_obs_sorted <- log(sort(table(all$user.id)))
names(user_obs_sorted) <- NULL
infl <- inflection::uik(1:length(user_obs_sorted), user_obs_sorted)
threshold <- user_obs_sorted[infl]
exp(threshold)

hist(exp(user_obs_sorted), main = "Sorted User Observation Counts", xlim=c(1,100), breaks=20000)
abline(v = exp(threshold), col = "red", lty = 2)
abline(v = 50, col = "blue", lty = 2)

sum(table(all$user.id)>85)


############ Activity metrics ----
source("Activity.R")

      
############ Taxonomic observer specialization ----
source("TaxoClusteringSpe.R")


# Split observers into resident and visitors
resid <- all[all$user.id %in% user_char[user_char$RorV=="Resident", "user.id"],]
dim(resid)

######################################################################################
######################################################################################
######################################################################################
######################################################################################







############ Identifiers ----


############ Activity clustering ----
source("Activity.R")


############ Territory ----



###### For locals (residents), define their "territory" in terms of size and 





