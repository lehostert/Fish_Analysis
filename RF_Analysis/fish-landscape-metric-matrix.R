library(tidyverse)
# library(AICCmodavg)
# library(randomForest)
# library(vegan)
# library(tree)
# library(randomForest)
# library(randomForestExplainer)

## This script bring together the fish metrics and landscape geology metrics to make one matrix. 

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")

#Read in fish matrix data, cut parameters that do to show variation among sites, and cut samples to include data >20 indv & >5 spp. 
fish_matrix <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_basin_crep_2013-2020.csv"), na = "")
id_crep <-read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_CREP_2013-2020.csv"))
id_key <- id_crep

names(fish_matrix) <- str_to_lower(names(fish_matrix))
names(id_key) <- str_to_lower(names(id_key))

fish_matrix <- fish_matrix %>%
  select(-c(data_source, catopind,	centpind,	cyprpind,	leucpind,	xenontax,	xenoptax,	xenopind,	clxpind, ictapind,	percpind,	alienntax, alienptax, alienpind,
          nativntax,	nativptax,	nativpind, natintpind, tolrpind	,intolpind, modtolpind,	pcngospind,	pcngbhpind,	pcgsubpind,	pcnestpind,	pcbearpind,
          fspind,	lithpind,	carnpind	,invntax	,invptax	,invpind	,herbpind,	omnipind,	algpind,	plantpind,	detpind,	bentinvntax,	bentinvptax,
          bentinvpind,	cosubpind))

fm <- fish_matrix %>%
  filter(individuals >19, richness > 4)

## Compile list of samples that were removed from the filtering above and cut the id_key list to match
cut_list <- setdiff(fish_matrix, fm)
rem <- cut_list$site_id

names(id_key) <- str_to_lower(names(id_key))
id_key <- id_key %>%
  filter(!(site_id %in% rem))

# Read in habitat data
habitat <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics_revised.csv"))
names(habitat) <- str_to_lower(names(habitat))

fab_hab <- fish_matrix %>%
  # left_join(id_key, by = "site_id") %>% 
  left_join(habitat, by = "pu_gap_code") 

fab_hab <- fab_hab %>% 
  select(1, 112:114, 2:181)
  # select(69, 1, 70:142, 2:68)

stream <- readxl::read_xlsx(path = "~/CREP/Proposals/CREP Goals/Fish_Habitat_Characteristics.xlsx")

interest <- stream %>% 
  select(PU_Gap_Code, Reach_Name, Event_Date, Site_Type, Mean_Width, Mean_Depth, Reach_Length)
names(interest) <- str_to_lower(names(interest))

fab_hab <- left_join(fab_hab, interest)

write_csv(fab_hab, path = "~/CREP/Proposals/CREP Goals/kasky_fish_and_landuse_geology_metrics_20210223.csv")
