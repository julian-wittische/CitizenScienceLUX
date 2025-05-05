library(httr)
library(jsonlite)

get_obs <- function(user_id, delay = 1) {
  # Verify internet connection
  if (!curl::has_internet()) {
    message("No internet connection available")
    return(NULL)
  }
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Initial test request
  test_req <- GET(
    base_url,
    query = list(
      user_id = user_id,
      per_page = 1,
      page = 1
    )
  )
  
  # Check for HTTP errors
  if (http_error(test_req)) {
    message("API request failed with status: ", status_code(test_req))
    return(NULL)
  }
  
  # Parse response
  test_data <- fromJSON(content(test_req, as = "text", encoding = "UTF-8"))
  
  # Check if we got valid data
  if (is.null(test_data$total_results)) {
    message("API response doesn't contain expected data structure")
    message("Full response:")
    print(test_data)
    return(NULL)
  }
  
  total_obs <- test_data$total_results
  message("Total observations found: ", total_obs)
  
  if (total_obs == 0) {
    message("User has no observations")
    return(NULL)
  }
  
  # Function to fetch a single page
  fetch_page <- function(page) {
    response <- GET(
      base_url,
      query = list(
        user_id = user_id,
        per_page = 200,
        page = page
      )
    )
    
    Sys.sleep(delay)
    
    if (http_error(response)) {
      message("Failed to fetch page ", page)
      return(NULL)
    }
    
    page_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    
    if (is.null(page_data$results)) {
      message("No observations in page ", page)
      return(NULL)
    }
    
    page_data$results
  }
  
  # Calculate total pages needed
  total_pages <- ceiling(total_obs / 200)
  message("Need to fetch ", total_pages, " pages")
  
  # Fetch all pages
  all_obs <- list()
  for (page in 1:total_pages) {
    message("Fetching page ", page, " of ", total_pages)
    page_data <- fetch_page(page)
    
    if (!is.null(page_data)) {
      all_obs <- c(all_obs, list(page_data))
    } else {
      message("Skipping page ", page)
    }
  }
  
  # Combine all pages into one data frame
  if (length(all_obs) > 0) {
    final_data <- do.call(rbind, all_obs)
    message("Successfully retrieved ", nrow(final_data), " observations")
    return(final_data)
  } else {
    message("No observations were retrieved")
    return(NULL)
  }
  cat(sprintf("Returned %d total observations\n", length(data_out)))
}