######################## PROJECT: citizen science data analysis
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
# Script objective : finding out how mnay people identify x% of identifications

############ Load identification data ----

### Read
id <- read.csv(paste0(DATAPATH, "identifications.csv"))

### Sort by user id
id_sort <- id[order(id$user_id),]
head(id_sort)

############ Identifier metrics ----

### Find out how many IDs each identifier has done
count <- as.data.frame(table(id_sort$user_id))
colnames(count) <- c("user_id", "count")

### Add name
count <- merge(count, id[, c("user_id", "user_name", "user_login")], by = "user_id", all.x = TRUE)
count  <- count [!duplicated(count $user_id), ]

### Sort count
sorted_count <- count[order(count$count, decreasing = TRUE),]

### Percentage of IDs done by each user
sorted_count$percent <- sorted_count$count/nrow(id)*100 

# Plot showing the quickly saturating empirical "curve": most identifications are made by few people
plot(cumsum(sorted_count$percent), type="s", xlim=c(1,length((sorted_count$percent))),
     ylim=c(0,100), col="red", ylab="Proportion of identifications",
     xlab="Identifiers sorted by # of identifications",  yaxs="i")
abline(h=79, col="black", lty=5)
abline(v=500, col="black", lty=5)
abline(h=sorted_count$percent[1], lty=5)

# Top 500
cumsum(sorted_count$percent)[1:500]
# 78.34%

500/length(sorted_count$percent)

5/length(sorted_count$percent)

### How many single ID-users?
sum(sorted_count$count==1)
# %
sum(sorted_count$count==1)/nrow(sorted_count)*100

###### Self-identifications

### Overall %
sum(id$own_observation=="True")/nrow(id)*100

### How many users only have self-identifications?
only1 <- id[id$user_id%in%sorted_count[sorted_count$count==1,]$user_id,]
sum(only1$own_observation=="True")/nrow(id)*100

############ Territory stuff ----
