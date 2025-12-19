######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : Load communes data and calculate metrics for each 

############ Loading libraries ----

df_accum <- inat %>%
  filter(!is.na(taxon_species_name)) %>%        # only rows with a species
  arrange(observed_on) %>%                       # chronological
  mutate(
    new_species = !duplicated(taxon_species_name),
    n_species = cumsum(new_species),
    n_obs = row_number()
  )

# Find the observation number for a specific date
target_date <- as.Date("2019-09-26")
obs_at_date <- df_accum %>%
  filter(observed_on == target_date) %>%
  slice(1) %>%           # in case multiple obs on same day, take first
  pull(n_obs)

target_date2 <- as.Date("2022-09-26")
obs_at_date2 <- df_accum %>%
  filter(observed_on == target_date2) %>%
  slice(1) %>%           # in case multiple obs on same day, take first
  pull(n_obs)

ggplot(df_accum, aes(x = n_obs, y = n_species)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = obs_at_date, linetype = "dashed", color = "black") +
  geom_vline(xintercept = obs_at_date2, linetype = "dashed", color = "black") +
      labs(x = "Number of observations",
       y = "Cumulative number of species") +
  theme_bw()
