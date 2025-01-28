######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the user focus section

############ Load data ----
load("iNat.RData") # Created using 1_Data.R

############ Loading libraries ----
source("0_Libraries.R")

############ Identifiers ----
id <- read.csv(paste0(DATAPATH, "identifications.csv"))

######

### Find out how many IDs each identifier has done
count <- as.data.frame(table(id$user_id))
colnames(count) <- c("User", "ID_number" )

### Sort count
sorted_count <- count[order(count$ID_number),]

### Divide by total number of IDs

### How many single ID-users?
sum(sorted_count==1)
# %
sum(sorted_count==1)/nrow(sorted_count)*100

###### Self-identifications

### Overall %
sum(id$own_observation=="True")/nrow(id)*100

### How many users only have self-identifications?



############ User specialization ----

############ Territory ----

############ Temporal ----