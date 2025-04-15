get_inat_obs_user_JW <- function (username, delay = 1){
  # Check for internet
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
  
  # Check if user exists and has observations
  q_path <- paste0(username, ".csv")
  ping_path <- paste0(username, ".json")
  ping_query <- "&per_page=1&page=1"
  ping <- GET(base_url, path = paste0("observations/", ping_path), query = ping_query)
  total_res <- as.numeric(ping$headers$`x-total-entries`)
  print(total_res)
  
  data_out <- NULL
  
  if (length(total_res) == 0) {
    cat("Your search returned zero results. Perhaps your user does not exist.")
  } else if (total_res >= 10000) {
    cat("User has too many observations. Splitting by year...\n")
    for (year in 2000:2025) {
      # Check how many obs in this year
      ping_year <- GET(base_url, path = "observations.json", query = list(
        user_id = username,
        d1 = paste0(year, "-01-01"),
        d2 = paste0(year, "-12-31"),
        per_page = 1, page = 1
      ))
      Sys.sleep(delay)
      total_year_obs <- as.numeric(ping_year$headers$`x-total-entries`)
      
      if (is.na(total_year_obs) || total_year_obs == 0) {
        cat(sprintf("Skipping %d (0 obs)\n", year))
        next
      }
      
      cat(sprintf("Fetching %d (%d obs)\n", year, total_year_obs))
      
      # First page
      dat <- GET(base_url, path = paste0("observations/", q_path, "?d1=", year,"-01-01&d2=", year,"-12-31"), query = "&per_page=200&page=1")
      Sys.sleep(delay)
      data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text", encoding = "UTF-8")), fileEncoding = "UTF-8"))
      
      # Other pages
      if (total_year_obs > 200) {
        for (i in 2:ceiling(total_year_obs / 200)) {
          page_query <- paste0("&per_page=200&page=", i)
          dat <- GET(base_url, path = paste0("observations/", q_path, "?d1=", year,"-01-01&d2=", year,"-12-31"), query = page_query)
          Sys.sleep(delay)
          data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text", encoding = "UTF-8")), fileEncoding = "UTF-8"))
        }
      }
    }
  } else {
    # Simpler fetch if < 10,000 obs
    dat <- GET(base_url, path = paste0("observations/", q_path), query = "&per_page=200&page=1")
    Sys.sleep(delay)
    data_out <- read.csv(textConnection(content(dat, as = "text", encoding = "UTF-8")), fileEncoding = "UTF-8")
    
    if (total_res > 200) {
      for (i in 2:ceiling(total_res / 200)) {
        page_query <- paste0("&per_page=200&page=", i)
        dat <- GET(base_url, path = paste0("observations/", q_path), query = page_query)
        Sys.sleep(delay)
        data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text", encoding = "UTF-8")), fileEncoding = "UTF-8"))
      }
    }
  }
  
  beep(7)
  return(data_out)
}
