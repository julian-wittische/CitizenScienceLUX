# ID analysis

# Trying to get what we need using the rinat package
inat2 <- get_inat_obs(query = NULL,
                      taxon_name = NULL,
                      taxon_id = NULL,
                      place_id = "120582",
                      quality = NULL,
                      geo = NULL,
                      annotation = NULL,
                      year = 2024,
                      month = 9,
                      day = 9,
                      bounds = NULL,
                      maxresults = 100,
                      meta = FALSE)
user <- get_inat_user_stats()
