library(tidyverse)


## The path for mapping network drives like Bison are not the same in Windows & MacOS so define the network_prefix based on which computer you are using. 
network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- "/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/summary-plots"

#load data 
fish <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"))
paired_crep <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PairedSites_Attributes_LEH.csv"))
sites_list <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Site_List_2013-2019.csv"))

names(paired_crep) <- stringr::str_to_lower(names(paired_crep))
names(sites_list) <- stringr::str_to_lower(names(sites_list))

## clean up down paired site

paired_list <- paired_crep %>% 
  rename(pair_class = crp_class, pair_no = pair) %>% 
  select(pu_gap_code, pair_no, pair_class) %>% 
  drop_na()

# Combine data frames
fish$reach_name <- str_replace_all(fish$reach_name, " ", "")

fish_site_type <- fish %>%
  filter(data_source == "crep_monitoring") %>% 
  left_join(sites_list) %>% 
  select(-stream_name) %>% 
  left_join(paired_list) 

fish_site_type$pair_class<- ifelse(fish_site_type$site_type != "paired", NA, fish_site_type$pair_class)
fish_site_type$pair_no<- ifelse(fish_site_type$site_type != "paired", NA, fish_site_type$pair_no)

# test <- fish_site_type %>% 
#   select(site_id, pu_gap_code, reach_name, event_date, site_type, pair_class, pair_no)

# summary(fish)

#### Plots ####

# Shannon Richness - Paired high low
# Shannon Richness - Random Crep %
# Shannon Richness - CRP Percent

# tolerance values - Paired high low
# tolerance values - Random Crep %
# tolerance values - CRP Percent# Shannon Richness - Paired high low

ggplot2::
