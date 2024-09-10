################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################
############ SCRIPT OBJECTIVE:

############ Load road network

## osmdata package
# bb <- getbb("Grand Duché de Luxembourg", featuretype="country")
 roads <- opq(bb) %>% add_osm_feature(key = 'highway',
                                                value=c("motorway",
                                                        "trunk",
                                                        "primary",
                                                        "secondary",
                                                        "tertiary",
                                                        "unclassified",
                                                        "residential",
                                                        "road",
                                                        "motorway_link",
                                                        "trunk_link",
                                                        "primary_link",
                                                        "secondary_link",
                                                        "tertiary_link")) %>% osmdata_sf()


 roads <- st_transform(roads$osm_lines, crs="EPSG:2169")
 # there was an error due to field counted reached so remove some
 roads <- roads[,1:10]
 ggplot() + geom_sf(data=roads)
 st_write(roads, "S:/BDPatNat/_Julian/ENV_DATA_EUROPE/roadsLUX2169_1.geojson", delete_layer = TRUE)