library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

## The path for mapping network drives like Bison are not the same in Windows as MacOS so define the network_prefix based on which computer you are using. 
network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- "/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/RF_fish_landscape_best_mtry"

#### Random Forest using best mtry ####

metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")
# metrics_list <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200309.xlsx"), sheet = 2)
metrics_list <- metrics_list %>% as.matrix() 
attach(metrics_envi.dat)

# rural_metrics_envi.dat <- metrics_envi.dat %>% 
#   filter(w_urban <0.02)

## Setting the seed before the rf allows you to get the same randomness every time. 
set.seed(2020)

for (i in 1:nrow(metrics_list))
{
    j <- as.numeric(paste(metrics_list[i,2]))
    
    metric_name <- paste(metrics_list[i,1])
  
    metric <- get(paste(metrics_list[i,1]))
    
    fish.rf <- randomForest(metric~link+dlink+c_order+dorder+wt_total_sqme+
                              wt_gdd+wt_jul_mnx+wt_prec+
                              c_br50+c_br100+c_brg100+wt_br50+wt_br100+wt_brg100+
                              wt_br_carbonate+wt_br_sandstone+wt_br_shale+wt_rocky+wt_alluvium_fluvial+wt_coarse_moraine+wt_coarse+wt_colluvium+wt_dune+
                              wt_fines+wt_lacustrine+wt_loess+wt_medium_moraine+wt_outwash+wt_peat_muck+wt_icecontact+
                              w_darcyx+wt_darcyx+w_permx+wt_permx+
                              r_open_wet+rt_grassland+w_forest_total+w_agriculture+w_grassland+w_urban+w_open_wet+w_wetland_total+wt_forest_total+wt_urban+wt_grassland+wt_agriculture+
                              bigriver+damdwl+damdw+damupl+damup+missi+pond+pond_area+pondwl+pondwa+ponddw+pondupl+pondupa+pondup+
                              sinuous+w_total_sqm+w_slope+wt_slope+gradient+
                              w_crepcrp_percent+w_hel_percent, 
                              data = metrics_envi.dat, na.action = na.omit, ntree=5000,importance=T, mtry=j)
    
    pdf(paste0(network_prefix, analysis_folder,"/fish_RF_VarImportance_",metric_name, ".pdf"), width = 9)
    varImpPlot(fish.rf)
    dev.off()
    
    imp_fish_rf <- importance(fish.rf)
    imp_fish_rf <- data.frame(imp_fish_rf)
    imp_fish_rf <- tibble::rownames_to_column(imp_fish_rf , "landscape_metric")
    imp_fish_rf$fish_metric <- paste(metrics_list[i,1])
    imp_fish_rf$mtry <- j
    
    write.csv(imp_fish_rf, paste0(network_prefix, analysis_folder,"/fish_RF_VarImportance_",metric_name, ".csv"), row.names = T)
}


######### Create best mtry .CSV files ##########
library(tidyverse)

rf_filenames <- list.files(path= "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/RF_fish_landscape_best_mtry", pattern= "*.csv")
rf_fullpath = file.path("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry_redo", rf_filenames)
rf_fulldataset <- do.call("rbind",lapply(rf_fullpath, FUN = function(files){read.csv(files, stringsAsFactors = FALSE, na.strings = ".")}))
rf_fulldataset <- rf_fulldataset %>% 
  select(-c(X))
write.csv(rf_fulldataset, file= paste0(network_prefix, analysis_folder,"/fish_landscape_bestmtry_RF_VarImportance_20200318.csv"), na= "", row.names = F)


rf_fulldataset <- read_csv(file = paste0(network_prefix, analysis_folder,"/fish_landscape_bestmtry_RF_VarImportance_20200318.csv") )
rf_fulldataset$landscape_metric <- as.factor(rf_fulldataset$landscape_metric)
rf_fulldataset$fish_metric<- as.factor(rf_fulldataset$fish_metric)

rf_top <- rf_fulldataset %>% 
  arrange(desc(X.IncMSE)) %>% 
  group_by(fish_metric) %>% 
  slice(1:10) %>% 
  ungroup()

top_preditors_ranked <- rf_top %>% 
  group_by(landscape_metric) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

predictor_ranks <- rf_fulldataset %>% 
  select(landscape_metric) %>% 
  unique() %>% 
  full_join(top_preditors_ranked) %>% 
  replace_na(list(count = 0)) %>% 
  arrange(desc(count))
  
#### Create functions for RF summaries ####
library(docstring)

top_ten_predictors <- function(rf_dataset, response_metric) {
  
  #' Create dataframe in which landscape predictors are ranked by # of times they occur in the top ten of each fish metric based on %IncMSE.
  #'
  #' 
  #' @param rf_dataset A dataframe with at least 4 collumns response metric (e.g. fish_metric),predictor metric (e.g. landscape_metric), 
  #' percent increase in MSE (X.IncMSE), and increase in node purity (IncNodePurity), and the mtry put into the random forest (mtry)
  #' 
  #' 
  #' @param response_metric this is the response variable used on the left side of the ~ in the RF. We will group by this parameter to get 
  #' the top ten predictors per this metric. 
  
  rf_top <- rf_dataset %>% 
    arrange(desc(X.IncMSE)) %>% 
    group_by(response_metric) %>% 
    slice(1:10) %>% 
    ungroup()
 
}

rf_top_func <- top_ten_predictors(rf_fulldataset, fish_metric)


#### Plot Important Variables ####

##Partial Dependancy Plots looping over variable to create for all variables. Remember y-values
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
