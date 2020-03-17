library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

### This analysis is based off of the environmental variables from 

####### The metrics data set

metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")
attach(metrics_envi.dat)
metrics_list_LEH <- metrics_envi.dat   %>% 
  select(5:74) %>% 
  names() %>% 
  as.matrix()
#### This file must be a matrix in order to use it in the loop
#### try to add data to the RF command in order to get around the "attach" function
# for (variable in vector) {
#   
# }

sink(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200309.txt"))

for (i in metrics_list_LEH)
  
{
  for (j in 1:8)
  {
    
    Metric<-get(paste(i))
    
    Metric.rf<-randomForest(Metric~link+dlink+c_order+dorder+wt_total_sqme+
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
    
    R_value<-Metric.rf$rsq[5000]
    
    A<-c(i,j,R_value)
    
    print (A)
    
  }
}

sink()

list()


for (i in metrics_list_LEH)
{
  print(i)
}

#### Random Forest ####

set.seed(2020)


fish_metric <- "sensptax"
fish_RF <- randomForest(fish.df$sensptax~., data = habitat.df, na.action = na.omit, ntree= 5000, mtry=4, importance= T)

fish_RF
imp_fish_RF <-importance(fish_RF)
habitat_list <- rownames(imp_fish_RF) 

imp_fish_RF <-data.frame(imp_fish_RF)

# Partial Dependancy Plots looping over variable to create for all variables. 
# Remember y-values 
for (habitat_feature in seq_along(habitat_list)) {
  file_out <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_WT_",fish_metric,"/fish_",fish_metric, "_RF_PP_", habitat_list[habitat_feature], ".pdf")
  pdf(file_out)
  partialPlot(fish_RF1, habitat.df, habitat_list[habitat_feature], main = paste("Partial Dependancy Plot on", habitat_list[habitat_feature]), xlab = paste(habitat_list[habitat_feature]))
  dev.off()
}

#PLOT YOUR FORESTS IMPORTANT VARIABLES
pdf(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_WT_",fish_metric,"/fish_",fish_metric, "_RF_VariableImportance.pdf"), width = 9)
varImpPlot(fish_RF2)
dev.off()

#
