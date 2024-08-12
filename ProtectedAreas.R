################################################################################
################### GOAL: citizen science data analysis ########################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: self/Paul Braun
# Start: Spring 2024
# Data: MNHNL
################################################################################

############ TO DO LIST

##### Load Natura 2000 and ZPIN data
ludh <- st_read(dsn="/vsizip/ludh-20231006.zip", options = "ENCODING=UTF8")
ludo <- st_read(dsn="/vsizip/ludo-20231006.zip",options = "ENCODING=UTF8")

ludh <- ludh[,2:3]
ludo <- ludo[,2:3]

# over <- st_sym_difference(ludh, ludo)
# area_over <- st_area(over)
# 
# st_area(st_sym_difference(ludh[ludh$SITECODE=="LU0001028",], ludo[ludo$SITECODE=="LU0002008",]))
# st_join(ludh, ludo, join=st_overlaps)
# 
# lol <- st_join(ludh, ludo, join=st_equals, left=TRUE)
# 
# ggplot() + 
#   geom_sf(data=lux_borders, fill=NA) +
#   geom_sf(data=ludh, fill="red", alpha=0.5) + 
#   geom_sf(data=ludo, fill="yellow", alpha=0.5)

# Very bad SITECOD1, bad NATCODE, I have to use the name (NOM)

zpin <- st_read(dsn="/vsizip/zpin-declarees.zip",options = "ENCODING=UTF8")
zpin <- st_zm(zpin)
# ERROR: at least 1 geometry not valid
zpin <- st_make_valid(zpin)
# ERROR: A\r\n and 1 instead of A
# Correction
zpin[107,"SOUSZONE"] <- "A"
zpin[120, "SOUSZONE"] <- "A"

# Problem Laangmuer
ggplot() + geom_sf(data=zpin[zpin$NOM=="Laangmuer",])

# zpin$NATCODE <- gsub(" ", "", zpin$NATCODE, fixed = TRUE)
# which(zpin$NATCODE=="")
zpin <- zpin[order(zpin$PRIE_ID),]
zpin_temp <- zpin %>%   group_by(PRIE_ID) %>%   summarise(SITECODE= first(PRIE_ID), SITENAME=first(NOM), geometry = st_union(geometry))

# Checkpoint
ggplot() + geom_sf(data=zpin_temp[zpin_temp$SITENAME=="Laangmuer",])

zpin <- zpin_temp
zpin <- zpin[,c("SITECODE","SITENAME")]
zpin$SITECODE <- as.character(zpin$SITECODE)

prot.areas <- rbind(ludh, ludo, zpin)
# prot.areas <- dplyr::bind_rows(ludh, ludo, zpin)


##### Count observations in relation to protected areas

### How many observations 

# Total number of observations in natural/semi-natural areas
# Ask Paul for shapefiles that he needs to do again

# Number of observations in protected areas
in.prot <- st_intersects(prot.areas, coords)

# Number of observations per protected area
per.prot <- lengths(in.prot)

# Observations/km^2 per protected area
area.per.prot <- as.numeric(st_area(prot.areas))/1000000 # convertin m2 in km2
obs.per.km2 <- per.prot / area.per.prot

index.bot1 <- which(obs.per.km2 < quantile(obs.per.km2, 0.025))
index.top1 <- which(obs.per.km2 > quantile(obs.per.km2, 0.975))

bot1.areas <- prot.areas[index.bot1,]
bot1.areas 
top1.areas <- prot.areas[index.top1,]
top1.areas

centroid.df <- st_centroid(rbind(bot1.areas,top1.areas))

ggplot() +
  geom_sf(data=lux_borders, fill=NA, alpha=0.5) +
  geom_sf(data=bot1.areas, fill="darkred") + 
  geom_sf(data=top1.areas, fill="blue") + 
  geom_sf_text(data=centroid.df, aes(label=SITENAME))

### Question Claude

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


# Desnité espèces protégées dans et hors zone