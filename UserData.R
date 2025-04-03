library(rinat)

observer <- data.frame(table(inat_all$user_id))
colnames(observer) <- c("user", "num_obs_lux")
observer <- observer[order(observer$num_obs_lux, decreasing = TRUE),]

observer$num_obs_wld <- numeric(length=length(observer$user))
to_remove_index <-  numeric(0)
for (i in 1:length(observer$user)){
  temp <- get_inat_user_stats(uid=observer$user[i])$most_observations$count
  if (is.null(temp)){
    to_remove_index <- c(to_remove_index, i)
  } else {
    observer$num_obs_wld[i] <- get_inat_user_stats(uid=observer$user[i])$most_observations$count
  }
    Sys.sleep(3)
  print(paste(round(i/length(observer$user)*100),"%",";", i, "th user"))
}
observer <- observer[-to_remove_index,]
