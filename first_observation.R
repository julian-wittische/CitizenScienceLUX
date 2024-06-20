inat <- read.csv("S:/BDPatNat/_Julian/ENV_DATA_EUROPE/inat-lux-combined.csv", comment.char="#")
head(inat)

# Format date to perform operations/comparisons
date <- inat$observed_on
date <- stringr::str_trunc(date, 10, "right", ellipsis="")
date <- as.Date(date, "%d/%m/%Y")

# Combine with user_id
user_added_time <- data.frame(id=inat$user_id, date=date)
split <- split(user_added_time$date, user_added_time$id)
first_obs <- sapply(split, min)
class(first_obs) <- "Date"

# Which first observations fall within the CNC 2023 days

start <- as.Date("2023-04-28")
end <- as.Date("2023-05-01")

first_CNC23 <- first_obs[first_obs >= start& first_obs <= end]
length(first_CNC23)

start <- as.Date("2023-01-01")
end <- as.Date("2023-12-31")

first_CNC23 <- first_obs[first_obs >= start& first_obs <= end]
length(first_CNC23)
