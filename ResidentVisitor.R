######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : finding out whether a user is a resident or a visitor


###### 

activity_ratio
first_upload <- as.Date("2011-05-26")
rel_act_duration <- active_days/(as.Date("2025-06-30")-as.Date("2011-05-26"))
obvs_log <- log(nrow(user_data))
log_minmaxscaled_total <- (obvs_log - min(obvs_log)) / (max(obvs_log) - min(obvs_log))

class(ALL_OBS$user.id)




############ h scale
# (h = 0.4(oh) + 0.6(dh); Dimson & Gillespie 2023)

###### oh: # Lux / # global
user$oh <- country=="Luxembourg" / nrow(user)

###### dh:  proportion of active days in Luxembourg / active days global
user$active <- unique(user$date)
user$activeLux <- unique(userINLUX$date)
user$dh <- length(user$active)/length(user$activeLux)
  
###### Calculating h
user$h <- 0.4*oh + 0.6*dh

