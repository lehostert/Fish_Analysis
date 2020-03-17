library(tidyverse)
# library(AICCmodavg)
library(randomForest)
library(vegan)
library(tree)
library(randomForest)
library(randomForestExplainer)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

fish_matrix  <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"))
names(fish_matrix) <- str_to_lower(names(fish_matrix))
fish_matrix <- fish_matrix %>%
  select(-c(data_source, catopind,	centpind,	cyprpind,	leucpind,	xenontax,	xenoptax,	xenopind,	clxpind, ictapind,	percpind,	alienntax, alienptax, alienpind,
          nativntax,	nativptax,	nativpind, intpind, tolrpind	,senspind, intolpind,	pcngospind,	pcngbhpind,	pcgsubpind,	pcnestpind,	pcbearpind, 
          fspind,	lithpind,	carnpind	,invntax	,invptax	,invpind	,herbpind,	omnipind,	algpind,	plantpind,	detpind,	bentinvntax,	bentinvptax,
          bentinvpind,	cosubpind))

fm <- fish_matrix %>% 
  filter(individuals >19, richness > 4)

cut_list <- setdiff(fish_matrix, fm)
rem <- cut_list$site_id

id_key <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_site_id_key.csv"))
names(id_key) <- str_to_lower(names(id_key))
id_key <- id_key %>% 
  filter(!(site_id %in% rem))


habitat <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics_revised.csv"))
names(habitat) <- str_to_lower(names(habitat))
habitat <- habitat %>% 
  full_join(id_key, by = c("pu_gap_code"="pugap_code")) 

fab_hab <- habitat %>% 
  full_join(fm, by = "site_id") %>% 
  drop_na(event_date) 

fab_hab <- fab_hab %>% 
  select(69, 1, 70, 71,72, 73:142, 2:68)

write_csv(fab_hab, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"))