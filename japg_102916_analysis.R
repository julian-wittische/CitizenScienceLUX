library(dplyr)
library(tidyverse)


# Defining observer behavior ------------------------------ #

# Observer metrics
users <- read_csv("users/user_api_hi.csv")
study_period <- as.Date("2021-12-31") - as.Date("2008-01-01")

users <- users %>%
  mutate(obvs_log = log(total_observations), # total observations
         obvs_log1 = (obvs_log - min(obvs_log)) / (max(obvs_log) - min(obvs_log)), # total observations scaled
         activity_ratio = round(active_days / as.integer(active_period), 5), # activity ratio
         rad = round((active_period / (as.integer(study_period) + 1)), 5), # relative activity duration
         perc_o_hi = round((hi_obvs / total_observations), 5), # percent of observations in Hawaii
         perc_d_hi = round((active_days_hi / active_days), 5)) %>% # percent of active days in Hawaii
  select(-obvs_log)
rm(study_period)

# Primary location classification
users <- users %>% mutate(geo = ifelse(((perc_o_hi*0.4) + (perc_d_hi*0.6)) > 0.5, "R", "V"))

# Activity level clustering analysis
library(cluster)
library(fpc)

## hierarchical clustering analysis with Ward's Minimum Variance method
activity <- users %>% select(user_id, obvs_log1, activity_ratio, rad)
hca <- hclust(dist(activity[, 2:4]), method = "ward.D2")
plot(hca)
rm(hca)

## average width silhouette
aws <- pamk(data = activity[, 2:4], krange = 2:10, criterion = "multiasw", usepam = FALSE, scaling = FALSE, diss = FALSE)
aws # selected k = 3
rm(aws)

# k-means clustering analysis
set.seed(72)
k3 <- kmeans(activity[, 2:4], 3, nstart = 20); k3
activity_clusters <- k3$cluster
users$activity <- activity_clusters

rm(k3, activity_clusters, activity)


# Linear regression for pre-COVID-19 growth in participation (2008-2019) ------------------------------ #

inat0 <- read_csv("inat_classified_u2.csv")

# 1. annual change in observations, new users, and unique users (all groups/classes)

## summarize data by year and quarter
new <- inat0 %>% group_by(user_id) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")), quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter) %>% summarise(n_new = n())
total <- inat0 %>% group_by(year, quarter) %>% summarise(n = n(), n_users = n_distinct(user_id)) %>% ungroup()
inat_qtr <- total %>% left_join(new, by = c("year" = "year", "quarter" = "quarter")) %>%
  mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
  mutate(n_log = log(n), n_user_log = log(n_users), n_new_log = log(n_new)) %>% mutate(n_new_log = ifelse(n_new_log == "-Inf", NA, n_new_log))
rm(new, total)
inat2019 <- inat_qtr %>% filter(year <= 2019)

## test independence
independent <- aov(as.character(quarter) ~ year, data = inat2019); summary(independent) # covariate (year) and treatment/factor (quarter) are independent
rm(independent)

## test normality
qs <- 1:4; vars <- c("n_log", "n_user_log", "n_new_log")
for (var in 1:length(vars)) {
  x <- vars[var]
  for (q in 1:length(qs)) {
    inat_q <- inat2019[inat2019$quarter == qs[q],]; y <- pull(inat_q, var = x)
    norm <- shapiro.test(y); sig <- norm$p.value > 0.05
    print(paste(x, "in quarter", qs[q], "is normal -", sig))
    rm(inat_q, q, y, norm, sig)
  }; rm(var, x)
}
rm(qs, vars)

## test homogeneity of variance
homovar <- bartlett.test(n_log ~ quarter, data = inat2019); homovar$p.value > 0.05 
rm(homovar)

## ANCOVA to test for significant difference among quarters
for (var in 1:length(vars)) {
  x <- vars[var]; y <- pull(inat2019, var = x)
  interaction <- aov(y ~ year*as.character(quarter), data = inat2019)  # model w/interaction. no significance for year:as.character(quarter) suggests that slope of regression is similar for factor groups
  none <- aov(y ~ year+as.character(quarter), data = inat2019)  # test for significant difference in intercepts w/o interaction
  a <- anova(interaction, none) 
  model <- ifelse(a$`Pr(>F)`[[2]] < 0.01, "interaction", "none") # if p is significant, use interaction
  print(paste(vars[var], "removing interaction", ifelse(a$`Pr(>F)`[[2]] < 0.01, "DOES", "DOES NOT"), "affect model"))
  print(ifelse(model == "interaction", 
               ifelse(summary(interaction)[[1]][3,5] < 0.01, paste("Slopes are significantly different for", vars[var], summary(interaction)[[1]][3,5]), paste("Slopes are not significantly different for", vars[var])),
               ifelse(summary(none)[[1]][2,5] <= 0.05, paste("Intercepts are significantly different for", vars[var]), "Intercepts are not significantly different"))) 
  rm(interaction, none, a, x, y, model, var)
}

# linear models of n, n_users, n_new - log transformed
# natural log transformation improved fit (higher r2) and distribution of residuals (more normal/random)

# annual trend (quarter removed)
new <- inat0 %>% group_by(user_id) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>% mutate(year = as.integer(format(as.Date(first_day), "%Y"))) %>%
  group_by(year) %>% summarise(n_new = n())
total <- inat0 %>% group_by(year) %>% summarise(n = n(), n_users = n_distinct(user_id)) %>% ungroup()
inat_yr <- total %>% left_join(new, by = c("year" = "year")) %>%
  mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
  mutate(n_log = log(n), n_user_log = log(n_users), n_new_log = log(n_new)) %>% mutate(n_new_log = ifelse(n_new_log == "-Inf", NA, n_new_log))
rm(new, total)
inat2019 <- inat_yr %>% filter(year <= 2019)

var <- 'n_new_log'
inatlm <- lm(get(var) ~ year, data = inat2019)
print(paste(inatlm$coefficients[2], summary(inatlm)$adj.r.squared, with(summary(inatlm), pf(fstatistic[1],fstatistic[2],fstatistic[3],lower.tail=F)))) # slope; adjusted r-squared; p-value
plot(fitted(inatlm), resid(inatlm))
acf(residuals(inatlm))
rm(var, inatlm)


# 2. by observer primary location

## summarize data by year, quarter, and observer location
new <- inat0 %>% group_by(user_id, geo) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")),
         quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter, geo) %>% summarise(new_u = n()) #%>% group_by(year, quarter) %>% add_tally(new_u, name = "n_new")
total <- inat0 %>% group_by(year, quarter, geo) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_qtr <- total %>% left_join(new, by = c("year" = "year", "quarter" = "quarter", "geo" = "geo")) %>%
  mutate(obvs_log = log(obvs), user_log = log(users), new_log = log(new_u)) # %>% mutate(obvs_log = ifelse(obvs_log == "-Inf", NA, obvs_log), users_log = ifelse(users_log == "-Inf", NA, users_log), new_log = ifelse(new_log == "-Inf", NA, new_log))
rm(new, total)

inat_qtr <- inat_qtr %>% mutate(yq = year + (quarter/4) - 0.25)

## ANCOVA: does change in iNaturalist participation x observer location depend on quarter?
geo <- c("Resident", "Visiting"); vars <- c("obvs_log", "user_log", "new_log")
for (g in 1:length(geo)) {
  inat1 <- inat_qtr[inat_qtr$geo == geo[g],]
  for (var in 1:length(vars)) {
    x <- vars[var]; y <- pull(inat1, var = x)
    interaction <- aov(y ~ year*as.character(quarter), data = inat1)
    none <- aov(y ~ year+as.character(quarter), data = inat1)
    a <- anova(interaction, none) 
    model <- ifelse(a$`Pr(>F)`[[2]] < 0.01, "interaction", "none") # if p is significant, use interaction
    print(paste("for", geo[g], vars[var], "removing interaction", ifelse(a$`Pr(>F)`[[2]] < 0.01, "DOES", "DOES NOT"), "affect model"))
    print(ifelse(model == "interaction", 
                 ifelse(summary(interaction)[[1]][3,5] < 0.01, paste("Slopes are significantly different for", vars[var], summary(interaction)[[1]][3,5]), paste("Slopes are not significantly different for", vars[var])),
                 ifelse(summary(none)[[1]][2,5] < 0.01, paste("Intercepts are significantly different for", vars[var]), "Intercepts are not significantly different"))) 
    rm(interaction, none, a, x, y, model, var)
  }
  rm(g, inat1)
}

## Quarter did not have an effect - proceed with annual analysis

## summarize data by year and observer location 
new <- inat0 %>% group_by(user_id, geo) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y"))) %>%
  group_by(year, geo) %>% summarise(new_u = n()) #%>% group_by(year, quarter) %>% add_tally(new_u, name = "n_new")
total <- inat0 %>% group_by(year, geo) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_yr <- total %>% left_join(new, by = c("year" = "year", "geo" = "geo")) %>%
  mutate(obvs_log = log(obvs), user_log = log(users), new_log = log(new_u)) # %>% mutate(obvs_log = ifelse(obvs_log == "-Inf", NA, obvs_log), users_log = ifelse(users_log == "-Inf", NA, users_log), new_log = ifelse(new_log == "-Inf", NA, new_log))
rm(new, total)
inat2019_yr <- inat_yr %>% filter(year <= 2019)

## test assumption of normality
for (var in 1:length(vars)) {
  x <- vars[var]
  for (g in 1:length(geo)) {
    inat1 <- inat2019_yr[inat2019_yr$geo == geo[g],]; y <- pull(inat1, var = x)
    norm <- shapiro.test(y); sig <- norm$p.value > 0.05
    print(paste(x, "in group", geo[g], "is normal -", sig, norm$p.value))
    rm(inat1, g, y, norm, sig)
  }
  rm(var, x)
}
## test assumption of homogeneity of variance
for (var in 1:length(vars)) {
  x <- vars[var]; y <- pull(inat2019_yr, var = x)
  homovar <- bartlett.test(y ~ inat2019_yr$geo); sig <- homovar$p.value > 0.05
  print(paste(x, "variance is homogeneous -", sig, homovar$p.value))
  rm(x, homovar, sig, var)
}

## ANCOVA: test for significant difference in growth in observations, unique observers, and new observers between observer locations
for (var in 1:length(vars)) {
  x <- vars[var]; y <- pull(inat2019_yr, var = x)
  interaction <- aov(y ~ year*geo, data = inat2019_yr)
  none <- aov(y ~ year+geo, data = inat2019_yr)
  a <- anova(interaction, none) 
  model <- ifelse(a$`Pr(>F)`[[2]] < 0.05, "interaction", "none") # if p is significant, use interaction
  print(paste("for", vars[var], "removing interaction", ifelse(a$`Pr(>F)`[[2]] < 0.05, "DOES", "DOES NOT"), "affect model"))
  print(ifelse(model == "interaction", 
               ifelse(summary(interaction)[[1]][3,5] < 0.05, paste("Slopes are significantly different for", vars[var], summary(interaction)[[1]][3,5]), paste("Slopes are not significantly different for", vars[var])),
               ifelse(summary(none)[[1]][2,5] < 0.05, paste("Intercepts are significantly different for", vars[var]), "Intercepts are not significantly different"))) 
  rm(interaction, none, a, x, y, model, var)
}

# 3. by observer behavior group (primary location x activity level)

## summarize data by year, quarter, and observer behavior
new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")),
         quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter, u_class) %>% summarise(new_u = n()) #%>% group_by(year, quarter) %>% add_tally(new_u, name = "n_new")
total <- inat0 %>%
  group_by(year, quarter, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_qtr <- total %>% left_join(new, by = c("year" = "year", "quarter" = "quarter", "u_class" = "u_class")) %>%
  mutate(obvs_log = log(obvs), user_log = log(users), new_log = log(new_u)) # %>% mutate(obvs_log = ifelse(obvs_log == "-Inf", NA, obvs_log), users_log = ifelse(users_log == "-Inf", NA, users_log), new_log = ifelse(new_log == "-Inf", NA, new_log))
rm(new, total)

inat_qtr <- inat_qtr %>% mutate(yq = year + (quarter/4) - 0.25)

## ANCOVA: does change in iNaturalist participation x observer behavior depend on quarter?
pbs <- unique(inat_qtr$u_class); vars <- c("obvs_log", "user_log", "new_log")
df <- data.frame()
for (p in 1:length(pbs)) {
  inat1 <- inat_qtr[inat_qtr$u_class == pbs[p],]
  for (var in 1:length(vars)) {
    x <- vars[var]; y <- pull(inat1, var = x)
    interaction <- aov(y ~ year*as.character(quarter), data = inat1)
    none <- aov(y ~ year+as.character(quarter), data = inat1)
    a <- anova(interaction, none) 
    model <- ifelse(a$`Pr(>F)`[[2]] < 0.01, "interaction", "none") # if p is significant, use interaction
    output <- c(pbs[p], vars[var], 
                ifelse(a$`Pr(>F)`[[2]] < 0.01, "interaction", "none"),
                ifelse(model == "interaction", 
                       ifelse(summary(interaction)[[1]][3,5] < 0.01, "Significantly different", "No difference"),
                       "NA"),
                ifelse(model == "interaction", summary(interaction)[[1]][3,5], "NA"),
                ifelse(summary(none)[[1]][2,5] < 0.01, "Significantly different", "No difference"),
                summary(none)[[1]][2,5])
    df <- rbind(df, output)
    rm(interaction, none, a, x, y, model, var, output)
  }; rm(p, inat1)
}
colnames(df)<-c("u_class", "variable", "int_effect", "slopes", "interaction_p", "intercepts", "none_p")
rm(df)

## Quarter did not have an effect - proceed with annual analysis

## summarize data by year and observer location 
new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y"))) %>%
  group_by(year, u_class) %>% summarise(new_u = n()) #%>% group_by(year, quarter) %>% add_tally(new_u, name = "n_new")
total <- inat0 %>%
  group_by(year, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_yr <- total %>% left_join(new, by = c("year" = "year", "u_class" = "u_class")) %>%
  mutate(obvs_log = log(obvs), user_log = log(users), new_log = log(new_u))
rm(new, total)
inat2019_yr <- inat_yr %>% filter(year <= 2019)

## test assumption of normality
df <- data.frame()
for (var in 1:length(vars)) {
  x <- vars[var]
  for (p in 1:length(pbs)) {
    inat1 <- inat2019_yr[inat2019_yr$u_class == pbs[p],]; y <- pull(inat1, var = x)
    norm <- shapiro.test(y); sig <- norm$p.value > 0.05
    output <- c(x, pbs[p], sig, norm$p.value)
    df <- rbind(df, output)
    rm(inat1, p, y, norm, sig, output)
  }; rm(var, x)
}
colnames(df) <- c("variable", "u_class", "normal", "p")
rm(df)
## test assumption of homogeneity of variance
for (var in 1:length(vars)) {
  x <- vars[var]; y <- pull(inat2019_yr, var = x)
  homovar <- bartlett.test(y ~ inat2019_yr$u_class); sig <- homovar$p.value > 0.01
  print(paste(x, "variance is homogeneous -", sig, homovar$p.value))
  rm(x, homovar, sig)
}; rm(var)

## ANCOVA: test for significant difference in growth in observations, unique observers, and new observers between observer locations
for (var in 1:length(vars)) {
  x <- vars[var]; y <- pull(inat2019_yr, var = x)
  interaction <- aov(y ~ year*u_class, data = inat2019_yr)
  none <- aov(y ~ year+u_class, data = inat2019_yr)
  a <- anova(interaction, none) 
  model <- ifelse(a$`Pr(>F)`[[2]] < 0.05, "interaction", "none") # if p is significant, use interaction
  print(paste("for", vars[var], "removing interaction", ifelse(a$`Pr(>F)`[[2]] < 0.05, "DOES", "DOES NOT"), "affect model"))
  print(ifelse(model == "interaction", 
               ifelse(summary(interaction)[[1]][3,5] < 0.05, paste("Slopes are significantly different for", vars[var], summary(interaction)[[1]][3,5]), paste("Slopes are not significantly different for", vars[var])),
               ifelse(summary(none)[[1]][2,5] < 0.05, paste("Intercepts are significantly different for", vars[var]), "Intercepts are not significantly different"))) 
  rm(interaction, none, a, x, y, model, var)
}

## If removing interaction has an effect, conduct post-hoc interaction analysis
library(emmeans)
interaction <- aov(obvs_log ~ year*u_class, data = inat2019_yr)  
emtrends(interaction, pairwise ~ u_class, var = "year")
rm(interaction)


# Difference between expected and observed 2020-2021 participation ------------------------------ #
# Based on Crimmins et al. (2021)

# 1. overall number of observations, new users, and unique users by quarter

## summarize by year and quarter
new <- inat0 %>% group_by(user_id) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")), quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter) %>% summarise(new = n())
total <- inat0 %>% group_by(year, quarter) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_qtr <- total %>% left_join(new, by = c("year" = "year", "quarter" = "quarter")) %>%
  pivot_longer(3:5, names_to = "variable", values_to = "value") %>%
  mutate(log = log(value)); rm(new, total)

# predict 2020 and 2021 participation; determine significance using 95% confidence interval
quarters <- 1:4; vars <- unique(inat_qtr$variable); pred <- data.frame()
for (q in 1:length(quarters)) {
  inat1 <- inat_qtr %>% filter(quarter == quarters[q])
  for (var in 1:length(vars)) {
    inat2 <- inat1 %>% filter(variable == vars[var]); yrs <- inat2$year
    inat2019 <- inat2 %>% filter(year <= 2019) %>% select(year, log)
    inatlm <- lm(log ~ year, data = inat2019)
    inat3 <- as.data.frame(predict(object = inatlm, newdata = inat2, interval = 'prediction', level = 0.95)) %>% mutate(year = yrs, variable = vars[var], quarter = quarters[q])
    pred <- rbind(pred, inat3)
    rm(inat2, inat2019, inat3, inatlm, var, yrs)
  }; rm(q, inat1)
}; rm(quarters, vars)
pred2 <- inat_qtr %>% left_join(pred, by = c("year" = "year", "quarter" = "quarter", "variable" = "variable")) %>% 
  mutate(sig = ifelse((log < lwr | log > upr), TRUE, FALSE), sig_label = ifelse(sig == TRUE, "*", ""),
         pc_log = ((log - fit)/ fit)*100,
         value_pred =(exp(1))^fit, pc = ((value - value_pred)/ value_pred)*100) ## pred_n = variable back-transformed; pc = percent change in non-transformed/back-transformed variable
rm(pred)

# 2. overall number of observations, new users, and unique users by year
  
new <- inat0 %>% group_by(user_id) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y"))) %>%
  group_by(year) %>% summarise(new = n())
total <- inat0 %>%
  group_by(year) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_yr <- total %>% left_join(new, by = c("year" = "year")) %>%
  pivot_longer(2:4, names_to = "variable", values_to = "value") %>%
  mutate(log = log(value)); rm(new, total)

vars <- unique(inat_yr$variable); pred <- data.frame()
for (var in 1:length(vars)) {
  inat2 <- inat_yr %>% filter(variable == vars[var]); yrs <- inat2$year
  inat2019 <- inat2 %>% filter(year <= 2019) %>% select(year, log)
  inatlm <- lm(log ~ year, data = inat2019)
  inat3 <- as.data.frame(predict(object = inatlm, newdata = inat2, interval = 'prediction', level = 0.95)) %>% mutate(year = yrs, variable = vars[var])
  pred <- rbind(pred, inat3)
  rm(inat2, inat2019, inat3, inatlm, var, yrs)
}; rm(vars)
pred2 <- inat_yr %>% left_join(pred, by = c("year" = "year", "variable" = "variable")) %>% 
  mutate(sig = ifelse((log < lwr | log > upr), TRUE, FALSE), sig_label = ifelse(sig == TRUE, "*", ""),
         pc_log = ((log - fit)/ fit)*100,
         value_pred =(exp(1))^fit, pc = ((value - value_pred)/ value_pred)*100) ## pred_n = variable back-transformed; pc = percent change in non-transformed/back-transformed variable
rm(pred)

# 3. participation x observer behavior by year

new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y"))) %>%
  group_by(year, u_class) %>% summarise(new = n()) 
total <- inat0 %>%
  group_by(year, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_yr <- total %>% left_join(new, by = c("year" = "year", "u_class" = "u_class")) %>%
  pivot_longer(3:5, names_to = "variable", values_to = "value") %>%
  mutate(log = log(value)); rm(new, total)

pbs <- unique(inat_yr$u_class); vars <- unique(inat_yr$variable); pred <- data.frame()
for (p in 1:length(pbs)) {
  inat1 <- inat_yr %>% filter(u_class == pbs[p])
  for (var in 1:length(vars)) {
    inat2 <- inat1 %>% filter(variable == vars[var]); yrs <- inat2$year
    inat2019 <- inat2 %>% filter(year <= 2019) %>% select(year, log)
    inatlm <- lm(log ~ year, data = inat2019)
    inat3 <- as.data.frame(predict(object = inatlm, newdata = inat2, interval = 'prediction', level = 0.95)) %>% mutate(year = yrs, variable = vars[var], u_class = pbs[p])
    pred <- rbind(pred, inat3)
    rm(inat2, inat2019, inat3, inatlm, var, yrs)
  }; rm(p, inat1)
}; rm(pbs, vars)
pred2 <- inat_yr %>% left_join(pred, by = c("year" = "year", "u_class" = "u_class", "variable" = "variable")) %>% 
  mutate(sig = ifelse((log < lwr | log > upr), TRUE, FALSE), sig_label = ifelse(sig == TRUE, "*", ""),
         pc_log = ((log - fit)/ fit)*100,
         value_pred =(exp(1))^fit, pc = ((value - value_pred)/ value_pred)*100) ## pred_n = variable back-transformed; pc = percent change in non-transformed/back-transformed variable
rm(pred)

# 4. participation x observer behavior by quarter
new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")),
         quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter, u_class) %>% summarise(new = n())
total <- inat0 %>%
  group_by(year, quarter, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup()
inat_qtr <- total %>% left_join(new, by = c("year" = "year", "quarter" = "quarter", "u_class" = "u_class")) %>%
  pivot_longer(4:6, names_to = "variable", values_to = "value") %>%
  mutate(log = log(value)) %>% mutate(log = ifelse(is.na(log), 0, log)) %>% filter(u_class != "f_V1")
rm(new, total)
 
quarters <- 1:4; pbs <- unique(inat_qtr$u_class); vars <- unique(inat_qtr$variable); pred <- data.frame()
for (q in 1:length(quarters)) {
  qq <- inat_qtr %>% filter(quarter == quarters[q])
  for (p in 1:length(pbs)) {
    inat1 <- qq %>% filter(u_class == pbs[p])
    for (var in 1:length(vars)) {
      inat2 <- inat1 %>% filter(variable == vars[var]); yrs <- inat2$year
      inat2019 <- inat2 %>% filter(year <= 2019) %>% select(year, log)
      print(paste(quarters[q], pbs[p], vars[var])) # errors encountered for visiting short-term users
      inatlm <- lm(log ~ year, data = inat2019)
      inat3 <- as.data.frame(predict(object = inatlm, newdata = inat2, interval = 'prediction', level = 0.95)) %>% mutate(year = yrs, quarter = quarters[q], variable = vars[var], u_class = pbs[p])
      pred <- rbind(pred, inat3)
      rm(inat2, inat2019, inat3, inatlm, var, yrs)
    }; rm(p, inat1)
  }; rm(qq)
}; rm(pbs, vars, quarters)
pred2 <- inat_qtr %>% left_join(pred, by = c("year" = "year", "quarter" = "quarter", "u_class" = "u_class", "variable" = "variable")) %>% 
  mutate(sig = ifelse((log < lwr | log > upr), TRUE, FALSE), sig_label = ifelse(log < lwr, '-', ifelse(log > upr, '+', NA)),
         pc_log = ((log - fit)/ fit)*100,
         value_pred =(exp(1))^fit, pc = ((value - value_pred)/ value_pred)*100) ## pred_n = variable back-transformed; pc = percent change in non-transformed/back-transformed variable
rm(pred)


# Change in participation from 2019, by observer group ------------------------------ #

inat0 <- inat0 %>% filter(year >= 2019)

## annual change
new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")), quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, u_class) %>% summarise(new_u = n())
inat_u <- inat0 %>% group_by(year, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup() %>%
  left_join(new, by = c("year" = "year", "u_class" = "u_class")) %>% ungroup(); rm(new)
inat_u2 <- inat_u %>% gather(key = "variable", value = "count", -year, -u_class) %>% spread(year, count) %>%
  mutate(v_label = ifelse(variable == "obvs", "Observations", ifelse(variable == "users", "Unique Observers", ifelse(variable == "new_u", "New Observers", "Uh oh"))),
         pc_19_20 = (`2020` - `2019`) / `2019` * 100,
         pc_19_21 = (`2021` - `2019`) / `2019` * 100) %>%
  gather(key = "period", value = "pc", -u_class, -variable, -v_label, -`2019`, -`2020`, -`2021`) %>%
  mutate(year = ifelse(period == "pc_19_20", 2020, 2021))

## change per quarter
new <- inat0 %>% group_by(user_id, u_class) %>%
  summarise(first_day = min(as.Date(observed, "%Y-%m-%d"))) %>%
  mutate(year = as.integer(format(as.Date(first_day), "%Y")), month = as.integer(format(as.Date(first_day), "%m")), quarter = ifelse(month <=3, 1, ifelse(month >3 & month <= 6, 2, ifelse(month > 6 & month <= 9, 3, 4)))) %>%
  group_by(year, quarter, u_class) %>% summarise(new_u = n())
inat_u <- inat0 %>% group_by(year, quarter, u_class) %>% summarise(obvs = n(), users = n_distinct(user_id)) %>% ungroup() %>%
  left_join(new, by = c("year" = "year", "quarter" = "quarter", "u_class" = "u_class")) %>% ungroup(); rm(new)

inat_u2 <- inat_u %>% gather(key = "variable", value = "count", -year, -quarter, -u_class) %>% spread(year, count) %>%
  mutate(v_label = ifelse(variable == "obvs", "Observations", ifelse(variable == "users", "Unique Observers", ifelse(variable == "new_u", "New Observers", "Uh oh"))),
         pc_19_20 = (`2020` - `2019`) / `2019` * 100,
         pc_19_21 = (`2021` - `2019`) / `2019` * 100) %>%
  gather(key = "period", value = "pc", -quarter, -u_class, -variable, -v_label, -`2019`, -`2020`, -`2021`) %>%
  mutate(quarter0 = ifelse(period == "pc_19_20", quarter, quarter + 4))
