######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Summer 2025
# Data: MNHNL
# Script objective : Analyses of observer activity (temporal aspect)

###### Initialize
observers <- sort(unique(all$user.id))

num <- numeric(length=length(observers))
chr <- character(length=length(observers))
date <- as.Date(chr)

user_char <- data.frame(user.id = num,
                        user.login = chr,
                        total_obs = num, #total number of observations
                        active_days = num, #number of active days (observed on)
                        active_days_lu = num, #number of active days in Luxembourg (observed on)
                        first_upl = date, #date of first upload
                        active_period = num, #full period of activity (uploaded at); we use created at to avoid issues with scanning old photos
                        perc_o_lu = num, #% of observations in Luxembourg (compared to global)
                        perc_d_lu = num, #% of active days in Luxembourg (compared to global)
                        h = num, #residency index based on Dimson & Gillespie (2023); between 0 and 1, >0.5 means resident
                        RorV = chr, #residency interpretation: resident or visitor
                        maxobs = num, #maximum observations in one day
                        obs_1st_mth = num, #number of observations in the first month
                        perc_early = num, #percentage of observations made in the first month
                        early_cat = chr, #categorization of users as early enthusiasts, other, and NA (too recent to be considered)
                        mean_month_numeric = num, # circular mean of activity months (1-12, not degrees)
                        rho =num, #concentration of seasonality
                        rel_act = num) #relative activity (active days/active period)

###### Metrics
user_char$user.id <- observers
user_char$user.login <- all$user.login[match(observers, all$user.id)]
most_recent <- max(as.Date(all$created_at_details.date))
most_recent <- as.Date("2024-05-13")

for (i in 1: length(observers)){
  user_data <- all %>% filter(user.id == observers[i])
  user_char[i, "total_obs"] <- nrow(user_data)
  user_char[i, "active_days"] <- length(table(user_data$observed_on_details.date))
  user_char[i, "active_days_lu"] <- length(table(user_data[user_data$luxornot==TRUE,]$observed_on_details.date))
  user_char[i, "first_upl"] <-  min(as.Date(user_data$created_at_details.date))
  user_char[i, "active_period"] <- as.numeric(max(as.Date(user_data$created_at_details.date)) - user_char[i, "first_upl"]+1)
  user_char[i, "perc_o_lu"] <- round(sum(user_data$luxornot)/user_char[i, "total_obs"], 5) 
  user_char[i, "maxobs"] <- max(table(user_data$observed_on_details.date))
  user_char[i, "obs_1st_mth"] <- sum(as.Date(user_data$created_at_details.date) <= (user_char[i, "first_upl"] + 100))
  user_char[i, c("mean_month_numeric", "rho")] <- seasonality(user_data)[2:3]
  cat(paste("User",i,observers[i]),"---","\n")
}

user_char$perc_d_lu <- round((user_char$active_days_lu / user_char$active_days), 5)
user_char$h <- user_char$perc_o_lu*0.4 + user_char$perc_d_lu*0.6
user_char$RorV <- ifelse(user_char$h > 0.5, "Resident", "Visitor")
user_char$rel_act <- user_char$active_days/user_char$active_period
user_char$perc_early <- user_char$obs_1st_mth/user_char$total_obs
user_char$early_cat <- ifelse((most_recent - user_char$first_upl)<=365, "Too new to tell",
                              ifelse(user_char$perc_early==1, "Drop-off", "Not drop-off"))

View(user_char)

###### Clustering

### NA ISSUE TO BE RESOLVED LATER (probably because of how I set the max date for activity period)
apply(act_clust_df, 2, FUN=function(x) sum(is.na(x)))
user_char[which(is.na(user_char$mean_month_numeric)),]
user_charnoNA <-user_char[-which(is.na(user_char$mean_month_numeric)),]
### Remove people with less than 50 observations
user_charnoNA <- user_charnoNA[user_charnoNA$total_obs>50,]

removed_cols <- which(names(user_char) %in% c("user.id",
                                              "user.login",
                                              "RorV",
                                              "early_cat",
                                              "first_upl",
                                              "perc_o_lu",
                                              "perc_d_lu",
                                              "active_days_lu"))

act_clust_df <- as.data.frame(apply(user_charnoNA[,-removed_cols], 2, scale))

gmm_act <- Mclust(act_clust_df, G=1:20)
summary(gmm_act)
plot(gmm_act)

###### Plot uncertainty
hist(gmm_act$uncertainty,
     main = "Histogram of Classification Uncertainty",
     xlab = "Uncertainty",
     breaks = 30,
     col = "skyblue")

###### Plot BIC and ICL
plot(gmm_act$BIC)
ICL_act <- mclustICL(act_clust_df, G=1:20)
plot(ICL_act)
summary(gmm_act$BIC)
summary(ICL_act)

EntropyGMM(gmm_act)
EntropyGMM(act_clust_df)

mod <- Mclust(act_clust_df, G = 1:20)  # Fit 1 to 20 components

# Extract BIC values
bic_values <- mod$BIC  # matrix: models x G
best_model <- mod


#####


View(user_charnoNA[which(gmm_fit$classification==1),])
View(user_charnoNA[which(act_clust$cluster==2),])
View(user_charnoNA[which(act_clust$cluster==3),])

sort(user_charnoNA[which(act_clust$cluster==1),]$user.login)
sort(user_charnoNA[which(act_clust$cluster==2),]$user.login)
sort(user_charnoNA[which(act_clust$cluster==3),]$user.login)

cbind(numeric_column_means(user_charnoNA[which(act_clust$cluster==1),]),
      numeric_column_means(user_charnoNA[which(act_clust$cluster==2),]),
      numeric_column_means(user_charnoNA[which(act_clust$cluster==3),]))

table(user_charnoNA[which(act_clust$cluster==1),]$early_cat)