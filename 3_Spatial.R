######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Analyses of the spatial context focus section 

############ Local configuration ----
source("config.R")

############ Load data ----
#load("iNat.RData") # Created using 1_Data.R

############ Loading libraries ----
#source("0_Libraries.R")

############ Accessibility ----
source("Accessibility.R")

############ Land cover analysis----
source("LandCover.R")

############ Communes ----
source("Communes.R")

############ Protected areas ----
source("ProtectedAreas.R")