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

get_all_obs_user_json_JW <- function(user_id, delay = 1) {
  if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
  }
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Ping to check total results
  ping <- GET(base_url, query = list(user_id = user_id, per_page = 1, page = 1))
  if (http_error(ping)) {
    message("iNaturalist API is unavailable or user not found.")
    return(invisible(NULL))
  }
  
  ping_json <- fromJSON(content(ping, as = "text", encoding = "UTF-8"))
  total_res <- ping_json$total_results
  
  if (is.null(total_res) || is.na(total_res)) {
    message("Failed to retrieve total number of observations.")
    return(invisible(NULL))
  }
  
  cat(paste("Total observations:", total_res),"\n")
  data_out <- list()
  
  get_obs_page <- function(user_id, page = 1) {
    q <- list(user_id = user_id, per_page = 200, page = page)
    res <- GET(base_url, query = q)
    Sys.sleep(delay)
    if (http_error(res)) return(NULL)
    res_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
    return(res_json$results)
  }
  
  # Ensure we are paginating correctly
  num_pages <- ceiling(total_res / 200)
  
  for (page in 1:num_pages) {
    cat(sprintf("Fetching page %d of %d\n", page, num_pages))
    
    page_data <- get_obs_page(user_id, page)
    
    if (!is.null(page_data)) {
      # Convert to data.frame before appending to list
      page_df <- as.data.frame(page_data)
      cat(paste("Got", nrow(page_df), "rows from page", page), "\n")  # Debug output
      data_out[[length(data_out) + 1]] <- page_df
    }
  }
  
  # Bind the data frames into a single final data frame
  final_data <- bind_rows(data_out)
  
  # Add missing columns to be able to use rbind() later
  
  
  
  beep(7)
  return(final_data)
}

# 
# lol <- get_all_obs_user_json_JW(user_id = 305)
# dim(lol)
# 
# lol2 <- get_all_obs_user_json_JW(user_id = observer$user_id[2])
# dim(lol2)
# 
# lol3 <- get_all_obs_user_json_JW(user_id = observer$user_id[3])
# dim(lol3)
