######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load libraries

############ Reading files ----
library(readxl) #keep

############ GIS ----
library(sf) #keep
#library(raster) #remove if not used
library(terra) #keep
library(rgeoboundaries) #keep - not on cran anymore; use archive
library(sp) #for use with the adehabitatHR package

############ Plotting ----
library(ggplot2) #keep
#library(units) #remove if not used
library(sjPlot) # keep: plot_model

############ Data manipulation ----
#library(magrittr)
library(dplyr)
library(tidyverse) #keep

############ Loading data ----
#library(osmdata) #remove if not used
library(rinat) #remove if not used
library(rgbif)
library(httr) #

############ Stats ----
library(fitdistrplus) #keep: descdist()
library(car) #keep: qqPlot()
library(adehabitatHR) #for user home range estimation

############ Date  ----
library(lubridate) #remove if not used
library(circular) #consider dates in a circular fashion
############ Development help ----
library(beepr)
