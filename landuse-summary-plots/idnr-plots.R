library(tidyverse)


## The path for mapping network drives like Bison are not the same in Windows & MacOS so define the network_prefix based on which computer you are using. 
network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- "/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/summary-plots"

#compare CRP/CREP percentages
kasky_attributes <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics_revised.csv"))
paired_crep <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PairedSites_Attributes_LEH.csv"))
bm_crep <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PU_Gaps_size_and_CRP_classes.csv"))

names(kasky_attributes) <- stringr::str_to_lower(names(kasky_attributes))
leh <- kasky_attributes %>%
  select(pu_gap_code, w_crepcrp_percent)

bam <- bm_crep %>%
  select(pu_gap_code, prop_local_CRP)

compare <- bam %>% 
  full_join(leh, by = 'pu_gap_code')

compare <- compare %>% 
  mutate(diff = prop_local_CRP - w_crepcrp_percent)

## cut down paired site

paired_list <- paired_crep %>% 
  rename(pair.class = CRP.Class, pair_no = Pair) %>% 
  select(pu_gap_code, pair_no, pair.class) %>% 
  drop_na()

# Load in one file with all of your sample ID, response variables, predictor variables in one df
fish <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"))

fish_pair <- fish %>%
  filter(data_source == "crep_monitoring") %>% 
  full_join(paired_list)

# summary(fish)
