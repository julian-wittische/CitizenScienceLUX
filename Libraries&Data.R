################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE: Load libraries and data and clean up data

############ Loading libraries

### Reading files
library(readxl)

### GIS 
library(sf)
library(raster)
library(terra)
library(rgeoboundaries)

### GIS and plotting
library(ggplot2)
library(units)
library(sjPlot)

### Data manipulation
library(magrittr)
library(dplyr)
library(tidyverse)

### Loading data
library(osmdata)
library(rinat)

### Stats
library(fitdistrplus)
library(car)

# ### Analysis
# library(DHARMa)
# library(lme4)
# library(spaMM)
# library(INLA)

############ Working directories for the data
os <- Sys.info()
### RStudio server
if (os[1]=="Linux"){
  setwd("/home/jwittische/Data/")
}
### Windows work
if (os[1]=="Windows"&os[4]=="MC232706"){
  setwd("W://01-Service/SCR/Julian/")
}
### Windows home
if (os[1]=="Windows"&os[4]!="MC232706"){
  setwd("old")
}

############ Load and preprocess iNaturalist observations
# New data
inat <- read_excel("./ENV_DATA_EUROPE/MASTER_inat-lux-combined.xlsx", sheet = 2)
inat <- as.data.frame(inat)

# Incomplete coordinates
inat  <- inat[complete.cases(inat$longitude),]
inat  <- inat[complete.cases(inat$latitude),]

# Verifiable, not obscured, and with geoaccuracy below 250m
verif250notobsc <- inat[inat$quality_grade!="casual" &
                          inat$coordinates_obscured==FALSE & (
                          inat$public_positional_accuracy<=250|is.na(inat$public_positional_accuracy)),]

# Crop observations to only include ones from Luxembourg
lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
coords <- verif250notobsc[,c("longitude","latitude")]
coords <- st_as_sf(x = coords, coords = c("longitude", "latitude"), crs = "EPSG:4326")
coords <- st_transform(coords, crs="EPSG:2169")
coords <- st_intersection(coords, lux_borders)
crop_logical <- st_contains(lux_borders, coords, sparse=FALSE)
verif250notobsc <- verif250notobsc[which(crop_logical==TRUE),]

# Check
ggplot() + geom_sf(data=coords)
# PASSED

# Add info back to sf object as fields
coords2 <-cbind(coords, verif250notobsc)

# Check
ggplot() + geom_sf(data=coords2)
# PASSED

