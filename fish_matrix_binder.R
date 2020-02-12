library(tidyverse)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

crep <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_CREP_2013-2019_final.csv"), na = "")
basin <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_IDNR_Basin_fit.csv"), na = "")


fish_matrix <- bind_rows("crep_monitoring" = crep, "IDNR_basin_surveys" = basin, .id = "data_source")

fish_matrix <- fish_matrix %>% select(2,1,3:112)

write_csv(fish_matrix, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"), na = ".")