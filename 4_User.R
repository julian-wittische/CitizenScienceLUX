######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the user focus section

############ Load data ----
load("iNat.RData") # Created using 1_Data.R

###### All observations from Luxembourg users

all <- load("ALL_OBS.RData")

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



