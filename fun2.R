library(httr)
library(jsonlite)

get_all_obs_user_json_JW <- function(user_id, delay = 1) {
  if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
  }
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Check if user has any obs
  ping <- GET(base_url, query = list(user_id = user_id, per_page = 1, page = 1))
  if (http_error(ping)) {
    message("iNaturalist API is unavailable or user not found.")
    return(invisible(NULL))
  }
  
  ping_json <- fromJSON(content(ping, as = "text", encoding = "UTF-8"), flatten = TRUE)
  total_res <- ping_json$total_results
  
  if (is.null(total_res) || is.na(total_res)) {
    message("Failed to retrieve total number of observations.")
    return(invisible(NULL))
  }
  
  print(paste("Total observations:", total_res))
  
  data_out <- list()
  
  get_obs_page <- function(user_id, year = NULL, page = 1) {
    q <- list(user_id = user_id, per_page = 200, page = page)
    if (!is.null(year)) {
      q$d1 <- paste0(year, "-01-01")
      q$d2 <- paste0(year, "-12-31")
    }
    res <- GET(base_url, query = q)
    Sys.sleep(delay)
    if (http_error(res)) return(NULL)
    fromJSON(content(res, as = "text", encoding = "UTF-8"))$results
  }
  
  fetch_year <- function(user_id, year) {
    cat(sprintf("Fetching year %d...\n", year))
    first_page <- GET(base_url, query = list(user_id = user_id, d1 = paste0(year, "-01-01"),
                                             d2 = paste0(year, "-12-31"), per_page = 1, page = 1))
    Sys.sleep(delay)
    
    if (http_error(first_page)) {
      cat(sprintf("Error fetching year %d. Skipping...\n", year))
      return(NULL)
    }
    
    json_year <- fromJSON(content(first_page, as = "text", encoding = "UTF-8"))
    total_year_obs <- json_year$total_results
    
    if (is.null(total_year_obs) || is.na(total_year_obs) || total_year_obs == 0) {
      cat(sprintf("Skipping %d (0 obs)\n", year))
      return(NULL)
    }
    
    year_data <- list()
    for (i in 1:ceiling(total_year_obs / 200)) {
      cat(sprintf("  Page %d of %d\n", i, ceiling(total_year_obs / 200)))
      pg_data <- get_obs_page(user_id, year = year, page = i)
      if (!is.null(pg_data)) year_data <- c(year_data, pg_data)
    }
    
    return(year_data)
  }
  
  if (total_res >= 10000) {
    cat("User has too many observations. Splitting by year...\n")
    for (year in 2000:2025) {
      year_obs <- fetch_year(user_id, year)
      if (!is.null(year_obs)) data_out <- c(data_out, year_obs)
    }
  } else {
    for (i in 1:ceiling(total_res / 200)) {
      cat(sprintf("Fetching page %d of %d\n", i, ceiling(total_res / 200)))
      page_data <- get_obs_page(user_id, page = i)
      print(page_data$created_at_details$year)
      cat(sprintf("  Got %s rows\n", if (is.null(page_data)) 0 else if (is.data.frame(page_data)) nrow(page_data) else length(page_data)))
      
      if (!is.null(page_data)) data_out <- c(data_out, page_data)
    }
  }
  
  beep(7)
  return(data_out)
}
