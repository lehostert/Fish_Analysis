library(tidyverse)

metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")

# rural_metrics_envi.dat <- metrics_envi.dat %>% 
#   filter(w_urban <0.02)

#### Freqeuncy Distrubutions ####