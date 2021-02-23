library(tidyverse)
library(reshape2)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

# crep <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_CREP_2013-2019_rename.csv"), na = "")
crep <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_CREP_2013-2020.csv"), na = "")
basin <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_Drake_1991-2007_rename.csv"), na = "")

# id_crep <-read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_CREP_2013-2019.csv"))
id_crep <-read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_CREP_2013-2020.csv"))
id_basin <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_Drake_1991-2007.csv"))

id_basin <- id_basin %>% 
  rename(PU_Gap_Code = pugap_code)


fish_matrix <- bind_rows("crep_monitoring" = crep, "IDNR_basin_surveys_LDrake" = basin, .id = "data_source")
id_key <- bind_rows("crep_monitoring" = id_crep, "IDNR_basin_surveys_LDrake" = id_basin, .id = "data_source")
id_key <- id_key %>% 
  select(2:5,1)

fish_matrix_full <- fish_matrix %>% 
  full_join(id_key, by = c("Site_ID", "data_source")) %>% 
  select(2,1,113,114,115,3:112)

fish_matrix_full$data_source <- as.factor(fish_matrix_full$data_source)

##TODO I am not sure what all of this is about. BElow

# pugap_unique <- fish_matrix_full %>% 
#   select(PU_Gap_Code) %>% 
#   unique()
# 
# pugap.source_unique <- fish_matrix_full %>% 
#   select(PU_Gap_Code, data_source) %>% 
#   unique()
# 
# site_unique <- fish_matrix_full %>% 
#   select(Reach_Name) %>% 
#   unique()
# 
# 
# summary(pugap.source_unique)
# 
# library(plyr)
# kasky.source_aggregate <- fish_matrix_full %>% 
#   select(-c(Site_ID, Reach_Name, Event_Date)) %>% 
#   plyr::ddply(.(data_source, pugap_code), colwise(mean))
# 
# # kasky.source_aggregate <- fish_matrix_full %>% 
# #   select(-c(Site_ID, Reach_Name, Event_Date)) %>% 
# #   group_by(data_source, pugap_code) %>% 
# #   summarize(value = mean(value)) %>% 
# #   ungroup()
# 
# kasky_aggregate <- fish_matrix_full %>% 
#   select(-c(Site_ID, Reach_Name, Event_Date, data_source)) %>% 
#   plyr::ddply(.(pugap_code),colwise(mean))
#   


# write_csv(id_key, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_site_id_key.csv"), na = ".")
write_csv(fish_matrix_full, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_basin_crep_2013-2020.csv"), na = ".")
# write_csv(fish_matrix_full, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"), na = ".")