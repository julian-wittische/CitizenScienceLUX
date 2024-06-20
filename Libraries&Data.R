################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ Loading libraries
library(sf)
library(raster)
library(ggplot2)
library(rgeoboundaries)

############ Load and preprocess iNaturalist observations
# Load data (check if it is the latest that Paul wants)
inat <- read.csv("S:/BDPatNat/_Julian/ENV_DATA_EUROPE/inat-lux-combined.csv", comment.char="#")
head(inat)

# Incomplete coordinates
inat  <- inat[complete.cases(inat$longitude),]
inat  <- inat[complete.cases(inat$latitude),]

# Research-grade observations only
res <- inat[inat$quality_grade=="research",]

# verifiable observations only
verif <- inat[inat$quality_grade!="casual",]




