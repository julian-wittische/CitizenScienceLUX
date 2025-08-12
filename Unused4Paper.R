######################################################################################

act_clust <- hdbscan(act_clust_df, minPts=10)
act_clust 
plot(act_clust)

###### WEIRD STUFF considering 13/05/2024
user_char[which(user_char$first_upl=="2025-05-29"),]
user_char[which(user_char$first_upl=="2024-06-10"),]

###### List of acquaintances
acq <- c("cpepin", "julian_wittische", "paul_luap", "callcc", "vitalfranz",
         "axel_hochkirch", "sleguil", "guypopbio", "wollef", "thelminger",
         "taniaw", "taniawalisch", "atur", "cecellina", "ykrippel", "hinatea",
         "cobymeester", "francisbirlenbach", "pinkgrasshopper", "wolffchristiane",
         "claudekolwelter", "tastyrna", "raedwulf68", "marielouise2", "georges3",
         "michelfrisch", "bee-together", "sylvie393", "jpir", "feitzfern",
         "luciamia", "tastyrna", "matteobellu239")

acq <- sort(acq)
user_char[user_char$user.login %in% acq,]
View(user_char[user_char$user.login %in% acq,])


taxa_clust_df <- as.data.frame(vegan::decostand(inat_classes_wide[,4:13],
                                                method = "hellinger"))

taxa_clust <- hdbscan(taxa_clust_df, minPts=2, cluster_selection_epsilon = 0.20)
taxa_clust 
plot(taxa_clust)

cbind(numeric_column_means(user_charnoNA[which(act_clust$cluster==1),]),
      numeric_column_means(user_charnoNA[which(act_clust$cluster==2),]),
      numeric_column_means(user_charnoNA[which(act_clust$cluster==3),]))

###### Percentage of protected area in commune
com2 <- com
com2$area_municipality <- as.numeric(st_area(com2))/1000000
intersection <- st_intersection(com2, st_union(prot.areas))
intersection$area_intersection <- st_area(intersection)

intersection_summary <- intersection %>%
  group_by(COMMUNE) %>% summarise(area_prot_in_municipality = sum(area_intersection))

inter <- st_drop_geometry(intersection_summary)
inter$area_prot_in_municipality <- as.numeric(inter$area_prot_in_municipality)/1000000
inter <- rbind(inter, c("Diekirch", 0))
inter$area_prot_in_municipality <- as.numeric(inter$area_prot_in_municipality)

com2 <- left_join(com2, inter)

com2 <- com2 %>%
  mutate(percentage_protected = (area_prot_in_municipality / area_municipality) * 100)

ggplot() +
  geom_sf(data = com2, aes(fill = percentage_protected), color = "black") +  # Fill by percentage
  scale_fill_viridis_c(option = "plasma", name = "% Protected") +  # Use a color scale for the percentage
  theme_minimal() +
  labs(title = "Percentage of Municipality Area Covered by Protected Areas",
       subtitle = "Each municipality is shaded by the percentage of its area in a protected area",
       fill = "Protected %") +
  theme(legend.position = "right")

com$percent.protected <- com2$percentage_protected

# Analysis
com_df <- st_drop_geometry(com)
mod <- glm(speciesPobs ~ as.numeric(area) + as.numeric(percent.protected) + observers, data=com_df)
vif(mod)
summary(mod)

plot_model(mod, type="pred")

ggplot(data=com) + geom_sf(aes(fill=pop)) +
  scale_fill_viridis_c(option = "viridis")
ggplot(data=com) + geom_sf(aes(fill=percent.protected)) +
  scale_fill_viridis_c(option = "viridis")

############################
### Questions Claude

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Sonnebierg",], coords)
sort(table(res[unlist(claude), "user_name"]), decreasing=TRUE)

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Sonnebierg",], coords)
sort(table(res[unlist(claude), "user_id"]), decreasing=TRUE)

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Aarnescht",], coords)
sort(table(res[unlist(claude), "user_name"]), decreasing=TRUE)

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Massif forestier du Stiefeschboesch",], coords)
sort(table(res[unlist(claude), "user_name"]), decreasing=TRUE)

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Schimpach - Carrières de Schimpach",], coords)
sort(table(res[unlist(claude), "user_name"]), decreasing=TRUE)

claude <- st_contains(prot.areas[prot.areas$SITENAME=="Schimpach - Carrières de Schimpach",], coords)
sort(table(res[unlist(claude), "user_name"]), decreasing=TRUE)

##############################
# exploring natura 2000
over <- st_sym_difference(ludh, ludo)
area_over <- st_area(over)

st_area(st_sym_difference(ludh[ludh$SITECODE=="LU0001028",], ludo[ludo$SITECODE=="LU0002008",]))
st_join(ludh, ludo, join=st_overlaps)

lol <- st_join(ludh, ludo, join=st_equals, left=TRUE)

ggplot() +
   geom_sf(data=lux_borders, fill=NA) +
   geom_sf(data=ludh, fill="red", alpha=0.5) +
   geom_sf(data=ludo, fill="yellow", alpha=0.5)

# Very bad SITECOD1, bad NATCODE, I have to use the name (NOM)
# Trying to get what we need using the rinat package
inat2 <- get_inat_obs(query = NULL,
                      taxon_name = NULL,
                      taxon_id = NULL,
                      place_id = "120582",
                      quality = NULL,
                      geo = NULL,
                      annotation = NULL,
                      year = 2024,
                      month = 9,
                      day = 9,
                      bounds = NULL,
                      maxresults = 100,
                      meta = FALSE)

################################################################################
# # Any left
# table(inatf$taxon_figure_name, useNA = "ifany")
# 
# # Which are there
# table(inatf[inatf$taxon_figure_name=="", "taxon_kingdom_name"])

###### For special graph for Paul
# write.csv(inatf[,c("oauth_application_name", "quality_grade", "taxon_figure_name")], "Data4Fig.csv")


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
