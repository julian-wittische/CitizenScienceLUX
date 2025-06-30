library(rinat)
library(dplyr)

source("fun3.R")
source("fun4.R")

observer <- data.frame(table(inat_all$user_id))
colnames(observer) <- c("user_id", "num_obs_lux")

observer <- observer[order(observer$num_obs_lux, decreasing = TRUE),]
inat_all$user_id <- as.factor(inat_all$user_id)

################################################################################
################################################################################
################################################################################

#rm(inat, inat_all, coords, coords2, verif250notobsc)
# gc()

range_observers <- (4501+783):nrow(observer)

####### TESTING
observer2 <- observer[range_observers,]
#######

ALL_OBS <- data.frame()

for (i in 1:nrow(observer2)){
  temp_pre <- Sys.time()
  cat(paste0("User number ", i, " out of ", nrow(observer2), " (ID: ",observer2$user_id[i], ")"), sep="\n")
  ALL_OBS <- safe_rbind(ALL_OBS, get_all_obs_user_json_JW(observer2$user_id[i], delay=1, beep=FALSE))
  cat(paste0(round(i/nrow(observer2),2)*100, "% of observers done"),
      paste0(difftime(Sys.time(),temp_pre, units="mins"), " mins"),
      sep="\n")
  cat(paste(nrow(ALL_OBS), "observations"), "\n")
  cat(strrep("-", 50), "\n")
}
save(ALL_OBS, file="ALL_OBS13.RData")
rm(ALL_OBS)
gc()

################################################################################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
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
