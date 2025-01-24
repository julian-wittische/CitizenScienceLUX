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
