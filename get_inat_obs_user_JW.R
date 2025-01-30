get_inat_obs_user_JW <- function (username){
 # Check if there an internet connection
   if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
   }
# Check if website responds
  base_url <- "http://www.inaturalist.org/"
  if (httr::http_error(base_url)) {
    message("iNaturalist API is unavailable.")
    return(invisible(NULL))
  }
# Check if user_name exists and if it has observations
  q_path <- paste0(username, ".csv")
  ping_path <- paste0(username, ".json")
  ping_query <- "&per_page=1&page=1"
  ping <- GET(base_url, path = paste0("observations/", ping_path), 
              query = ping_query)
  total_res <- as.numeric(ping$headers$`x-total-entries`)
  print(total_res)
  if (total_res == 0) {
    stop("Your search returned zero results. Perhaps your user does not exist.")
  }
  data_out <- NULL
  if (total_res >= 10000) {
   cat("This user has more observations that the native function could handle. We have to divide it by years to combine afterwards.")
    for (year in 2000:2025){
      page_query <- "&per_page=200&page=1"
      dat <- GET(base_url, path = paste0("observations/", q_path, "?d1=", year,"-01-01&d2=", year,"-12-31"), query = page_query)
      data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text"))))
      if (total_res > 200) {
        for (i in 2:ceiling(total_res/200)) {
          page_query <- paste0("&per_page=200&page=", i)
          dat <- GET(base_url, path = paste0("observations/", q_path, "?d1=", year,"-01-01&d2=", year,"-12-31"), query = page_query)
          data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text"))))
        }
      }
      cat(year)
    }
  }
  else {
    page_query <- "&per_page=200&page=1"
    dat <- GET(base_url, path = paste0("observations/", q_path), query = page_query)
    data_out <- read.csv(textConnection(content(dat, as = "text")))
    if (total_res > 200) {
      for (i in 2:ceiling(total_res/200)) {
        page_query <- paste0("&per_page=200&page=", i)
        dat <- GET(base_url, path = paste0("observations/", q_path), query = page_query)
        data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text"))))
      }
    }
  }
  beep(7)
  return(data_out)
}

# Testing
cw <- get_inat_obs_user_JW("wolffchristiane") #TOO MANY REQUESTS
beep(7)
pb <- get_inat_obs_user_JW("paul_luap") #TOO MANY REQUESTS
beep(7)
mf <- get_inat_obs_user_JW("michelfrisch")
beep(7)
jw <- get_inat_obs_user_JW("julian_wittische")
beep(7)

# 