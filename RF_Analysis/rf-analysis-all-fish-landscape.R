library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

####### The metrics data set is a combined df of all fish metrics and all landscape metrics. 

metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")

rural_metrics_envi.dat <- metrics_envi.dat %>%
  filter(w_urban < 0.02)

## If you pull in all of your predictor and response variables as one df then you must attach them in order to call them by name like "Metric~link+dlink+c_order"
## otherwise you must 
attach(rural_metrics_envi.dat)


response_metrics <- metrics_envi.dat %>% 
  select(5:74) %>% 
  names() %>% 
  as.matrix()


## The metrics list file must be a matrix in order to use it in the loop in the way this loop was written
## The following loop will take ~2 hours to complete given the number of response variable you are cycling though 
## and the number of mtrys (8) that you need to cycle each of the response variables through. 


sink(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200311.txt"))

for (i in response_metrics)
  
{
  for (j in 1:8)
  {
    
    Metric <- get(paste(i))
    
    Metric.rf <- randomForest(Metric~link+dlink+c_order+dorder+wt_total_sqme+
                            wt_gdd+wt_jul_mnx+wt_prec+
                            c_br50+c_br100+c_brg100+wt_br50+wt_br100+wt_brg100+
                            wt_br_carbonate+wt_br_sandstone+wt_br_shale+wt_rocky+wt_alluvium_fluvial+wt_coarse_moraine+wt_coarse+wt_colluvium+wt_dune+
                            wt_fines+wt_lacustrine+wt_loess+wt_medium_moraine+wt_outwash+wt_peat_muck+wt_icecontact+
                            w_darcyx+wt_darcyx+w_permx+wt_permx+
                            r_open_wet+rt_grassland+w_forest_total+w_agriculture+w_grassland+w_urban+w_open_wet+w_wetland_total+wt_forest_total+wt_urban+wt_grassland+wt_agriculture+
                            bigriver+damdwl+damdw+damupl+damup+missi+pond+pond_area+pondwl+pondwa+ponddw+pondupl+pondupa+pondup+
                            sinuous+w_total_sqm+w_slope+wt_slope+gradient+
                            w_crepcrp_percent+w_hel_percent, 
                            ntree=5000,importance=T, mtry=j)
    
    R_value <- Metric.rf$rsq[5000]
    
    A <- c(i,j,R_value)
    
    print(A)
    
  }
}

sink()

list()


for (i in response_metrics)
{
  print(i)
}

#### Find best mtry per metric####

## Read in the .txt that was created from the full loop of mtrys 1:8, all response metrics (fish metrics) and  predictor variables (landscape and geology)
rf_result <- read.table(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200311.txt"),
                        quote = "\"", col.names = c("x","metric", "mtry", "rsq"))

## Choose the best mtry
## Per each response metric look at when the change in rsq value is <0.01 from one mtry to another then pick the lowest m try when the change in rsq is <0.01
rf_bestmtry <- rf_result %>% 
  select(-c(x)) %>% 
  group_by(metric) %>% 
  arrange(mtry, .by_group = TRUE) %>% 
  mutate(
    diff = round(lead(rsq)-rsq, 4)
  ) %>% 
  filter(diff <= 0.01) %>% 
  summarize(mtry = min(mtry)) %>% 
  ungroup()

## Save the best mtrys for later
write_csv(rf_bestmtry, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200311_bestmtry.csv"))

## If you want to feed it into 'rf-analysis-fish-landscape-best-mtry.R' run this code below
# metrics_list <- rf_bestmtry
