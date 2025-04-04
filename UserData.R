########## REMOVE WHEN EVERYTHING WORKS
TESTING <- 10 ########## REMOVE WHEN EVERYTHING WORKS
########## REMOVE WHEN EVERYTHING WORKS


library(rinat)
library(dplyr)

observer <- data.frame(table(inat_all$user_id))
colnames(observer) <- c("user_id", "num_obs_lux")
observer <- observer[order(observer$num_obs_lux, decreasing = TRUE),]
inat_all$user_id <- as.factor(inat_all$user_id)

observer <- observer %>%
  left_join(inat_all %>% select(user_id, user_login), by = "user_id")


lol <- get_inat_obs_user("cobymeester", maxresults = 50000)

########## REMOVE WHEN EVERYTHING WORKS
observer <- observer[1:TESTING,] ########## REMOVE WHEN EVERYTHING WORKS
########## REMOVE WHEN EVERYTHING WORKS

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
