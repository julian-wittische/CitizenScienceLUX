library(rinat)
library(dplyr)

source("get_inat_obs_user_JW.R")
source("get_all_obs_user_json_JW.R")

observer <- data.frame(table(inat_all$user_id))
colnames(observer) <- c("user_id", "num_obs_lux")

# observer <- observer[order(observer$num_obs_lux, decreasing = TRUE),]
# inat_all$user_id <- as.factor(inat_all$user_id)

ALL_OBS <- vector(mode = "list", length = nrow(observer))

cumulative_obs <- 0

for (i in 1:nrow(observer)){
  temp_pre <- Sys.time()
  ALL_OBS[[i]] <- get_all_obs_user_json_JW(observer$user_id[i], delay=0.15)
  cat(paste0(round(i/nrow(observer),2)*100, "% of observers done"),
      paste0(difftime(Sys.time(),temp_pre, units="mins"), " mins"),
      sep="\n")
  cat("\n")
  cumulative_obs <- cumulative_obs + length(ALL_OBS[[i]]$quality_grade)
  cat(paste(cumulative_obs, "observations"))
  cat("\n")
}

################################################################################
################################################################################
################################################################################

observer$num_obs_wld <- numeric(length=length(observer$user))
to_remove_index <-  numeric(0)
for (i in 1:length(observer$user)){
  temp <- get_inat_user_stats(uid=observer$user[i],
                              date_range = c("1900-01-01","2024-05-11"))$most_observations$count
  print(temp)
  if (is.null(temp)){
    #to_remove_index <- c(to_remove_index, i)
  } else {
    observer$num_obs_wld[i] <- temp
  }
    Sys.sleep(2)
  print(paste(round(i/length(observer$user)*100),"%",";", i, "th user"))
}
observer <- observer[!(1:nrow(observer) %in% to_remove_index),]

all_data_obs <- NULL
for (i in 1:length(observer$user)){
  all_data_obs <- rbind(all_data_obs, get_inat_obs_user_JW(observer$)

###### The first 4 ones are beyond 10 000 so we have to change the function
