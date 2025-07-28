###### All observations from Luxembourg users

# load(paste0(DATAPATH,"all_obs/ALL_OBS.RData"))
# 
# col_sizes <- sapply(ALL_OBS, object.size)
# col_sizes_mb <- sort(col_sizes / (1024^2), decreasing = TRUE)
# print(round(col_sizes_mb, 2))
# print(round(col_sizes_mb, 2)/sum(col_sizes_mb)*100)

# Find all observation objects 
lf <- list.files(path = paste0(DATAPATH,"all_obs/"), pattern = "^ALL_OBS", full.names = TRUE)
# Initialize list
obj_ls <- vector("list", length(lf)) 
# Loop to go through each and remove useless but sizable columns
for (i in 1:length(lf)){
  print(lf[i])
  load(lf[i])
  obj_ls[[i]] <- ALL_OBS[ , !(names(ALL_OBS) %in% c("identifications", "non_owner_ids",
                                                    "observation_photos", "photos", "annotations",
                                                    "project_observations", "outlinks",
                                                    "comments", "votes", "faves"))]
  rm(ALL_OBS)
  gc()
}

lapply(obj_ls, dim)

keep_common_columns <- function(df_list) {
  # Find common column names across all list elements
  common_cols <- Reduce(intersect, lapply(df_list, colnames))
  
  # Subset each data frame to those common columns
  lapply(df_list, function(df) df[, common_cols, drop = FALSE])
}

obj_ls <- keep_common_columns(obj_ls)

all <- do.call(rbind, obj_ls)

save(all, file=paste0(DATAPATH,"all_obs/all.RData"))
