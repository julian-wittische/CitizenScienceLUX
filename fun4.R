library(httr)
library(jsonlite)
library(dplyr)
library(beepr)

safe_rbind <- function(df1, df2) {
  if (nrow(df1) == 0 && ncol(df1) == 0) return(df2)
  if (nrow(df2) == 0 && ncol(df2) == 0) return(df1)
  
  all_cols <- union(names(df1), names(df2))
  for (col in setdiff(all_cols, names(df1))) df1[[col]] <- NA
  for (col in setdiff(all_cols, names(df2))) df2[[col]] <- NA
  df1 <- df1[all_cols]
  df2 <- df2[all_cols]
  rbind(df1, df2)
}

get_all_obs_user_json_JW <- function(user_id, delay = 1, beep=TRUE) {
  if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
  }
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  ping <- try(GET(base_url, query = list(user_id = user_id, per_page = 1, page = 1)), silent = TRUE)
  if (inherits(ping, "try-error") || http_error(ping)) {
    message(sprintf("Ping failed for user '%s'. Skipping user.\n", user_id))
    return(data.frame())  # return empty data frame so your loop over users can proceed
  }
  
  ping_json <- fromJSON(content(ping, as = "text", encoding = "UTF-8"))
  total_res <- ping_json$total_results
  if (is.null(total_res) || is.na(total_res)) {
    message("Failed to retrieve total number of observations.")
    return(invisible(NULL))
  }
  
  cat(paste("Total observations:", total_res), "\n")
  data_out <- data.frame()
  
  get_obs_page <- function(user_id, year = NULL, page = 1) {
    q <- list(user_id = user_id, per_page = 200, page = page)
    if (!is.null(year)) {
      q$d1 <- paste0(year, "-01-01")
      q$d2 <- paste0(year, "-12-31")
    }
    res <- GET(base_url, query = q)
    Sys.sleep(delay)
    if (http_error(res)) return(NULL)
    res_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
    return(as.data.frame(res_json$results))
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
    
    year_data <- data.frame()
    for (i in 1:ceiling(total_year_obs / 200)) {
      cat(sprintf("  Page %d of %d\n", i, ceiling(total_year_obs / 200)))
      pg_data <- get_obs_page(user_id, year = year, page = i)
      if (!is.null(pg_data)) year_data <- safe_rbind(year_data, pg_data)
    }
    
    return(year_data)
  }
  
  if (total_res >= 10000) {
    cat("User has too many observations. Splitting by year...\n")
    for (year in 2000:2025) {
      year_obs <- fetch_year(user_id, year)
      if (!is.null(year_obs)) data_out <- safe_rbind(data_out, year_obs)
    }
  } else {
    num_pages <- ceiling(total_res / 200)
    for (i in 1:num_pages) {
      cat(sprintf("Fetching page %d of %d\n", i, num_pages))
      page_data <- get_obs_page(user_id, page = i)
      if (!is.null(page_data)) data_out <- safe_rbind(data_out, page_data)
    }
  }
  
  if(beep==TRUE) beep(7);
  return(data_out)
}
