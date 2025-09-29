######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : Combine countries and their EEZ

library(esri2sf)

url <- "https://wwf-sight-maps.org/arcgis/rest/services/Global/Administrative_Boundaries_GADM/MapServer/3"

countries_eez <- esri2sf(url)

library(sf)
library(dplyr)

# 1) Read data (use the world-wide files)
eez <- st_read("eez_v12.gpkg")                    # layer name is typically "eez"
land <- st_read("ne_10m_admin_0_countries.gpkg")  # or GADM Admin 0

# 2) Harmonize fields: get ISO3/Sovereign keys
# EEZ: choose sovereign to attribute overseas EEZs (e.g., FR includes Polynesia)
eez_key <- eez %>%
  select(iso_sov = ISO_SOV1, sovereign = SOVEREIGN1, eez_geom = geom)

# Land: some datasets have multiple admin types; keep sovereign/ISO3
land_key <- land %>%
  select(iso3 = ISO_A3, sovereignt = SOVEREIGNT, land_geom = geom)

# 3) Normalize keys (fallback to name join when ISO missing)
# Here we prefer ISO; join by ISO where available
eez_key <- eez_key %>% mutate(iso3 = iso_sov)
land_key <- land_key %>% mutate(iso3 = if_else(iso3 %in% c("-99", NA), NA_character_, iso3))

# 4) Dissolve EEZ polygons by sovereign ISO (handles overseas territories)
eez_diss <- eez_key %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  summarise(eez = st_union(eez_geom), .groups = "drop") %>%
  st_make_valid()

# 5) Dissolve land polygons by sovereign ISO
land_diss <- land_key %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  summarise(land = st_union(land_geom), .groups = "drop") %>%
  st_make_valid()

# 6) Combine: union land ∪ EEZ per country
both <- land_diss %>%
  full_join(eez_diss, by = "iso3") %>%
  mutate(
    combo = case_when(
      !st_is_empty(land) & !st_is_empty(eez) ~ st_union(st_make_valid(land), st_make_valid(eez)),
      st_is_empty(land)  & !st_is_empty(eez) ~ eez,
      !st_is_empty(land) & st_is_empty(eez)  ~ land,
      TRUE                                  ~ st_sfc()
    )
  ) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")  # drop exotic geometry types if any

# 7) (Optional) clean slivers and overlaps at world scale
both$combo <- st_buffer(both$combo, 0)

# Result: one polygon per ISO3 combining land + all sovereign EEZs
country_plus_eez <- both %>% select(iso3, combo) %>% st_set_geometry("combo")

# Save
st_write(country_plus_eez, "country_plus_eez.gpkg", delete_dsn = TRUE)
