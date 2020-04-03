library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

## The path for mapping network drives like Bison are not the same in Windows as MacOS so define the network_prefix based on which computer you are using. 
network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- "/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry_redo3"

#### Random Forest using best mtry ####

# Load in one file with all of your sample ID, response variables, predictor variables in one df
metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")
# metrics_list <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200311.xlsx"), sheet = 2)
metrics_list <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Metrics_RF_Result_20200311_bestmtry.csv"))
metrics_list <- metrics_list %>% as.matrix() 

## If you do not attach your data the "get()" function will not work in the loop as written below. 
## get() actually grabs the value of a named object so it grabs all of the values associated with whatever is your i-th metric in the metric list
## can can specify the environment in get() but it seems like attaching is easier.
attach(metrics_envi.dat)

# rural_metrics_envi.dat <- metrics_envi.dat %>% 
#   filter(w_urban <0.02)

## Setting the seed before the rf allows you to get the same randomness every time. 
set.seed(2020)

resultslist = list()

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
    
    resultslist[[i]] <- imp_fish_rf
    # write.csv(imp_fish_rf, paste0(network_prefix, analysis_folder,"/fish_RF_VarImportance_",metric_name, ".csv"), row.names = T)
}



rf_result <- do.call(rbind, resultslist)
write.csv(rf_result, file= paste0(network_prefix, analysis_folder,"/fish_landscape_bestmtry_RF_VarImportance_20200403.csv"), na= "", row.names = F)
rf_result$landscape_metric <- as.factor(rf_result$landscape_metric)
rf_result$fish_metric<- as.factor(rf_result$fish_metric)

# if you attach it is good principle to detach before moving on to other analyses
detach(metrics_envi.dat)

######### RF Summaries ##########
# TODO remove these later and replace with functions.

rf_top <- rf_result %>% 
  arrange(desc(X.IncMSE)) %>% 
  group_by(fish_metric) %>% 
  slice(1:10) %>% 
  ungroup()

top_preditors_ranked <- rf_top %>% 
  group_by(landscape_metric) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

predictor_ranks <- rf_result %>% 
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
  #' @param rf_dataset A dataframe with at least 4 collumns response metric (e.g. fish_metric),predictor metric (e.g. landscape_metric), 
  #' percent increase in MSE (X.IncMSE), and increase in node purity (IncNodePurity), and the mtry put into the random forest (mtry)
  #' 
  #' @param response_metric this is the response variable used on the left side of the ~ in the RF. We will group by this parameter to get 
  #' the top ten predictors per this metric. 
  
  response_metric <- enquo(response_metric)
  
  rf_top <- rf_dataset %>% 
    arrange(desc(X.IncMSE)) %>% 
    group_by(!! response_metric) %>% 
    slice(1:10) %>% 
    ungroup()
 
}

rank_predictors <- function(rf_dataset, response_metric, predictor_metric) {
  
  #' Create dataframe in which the number of times each predictor variables are ranked by # of times they occur in the top ten of each fish metric based on %IncMSE.
  #'
  #' @param rf_dataset A dataframe with at least 4 collumns response metric (e.g. fish_metric),predictor metric (e.g. landscape_metric), 
  #' percent increase in MSE (X.IncMSE), and increase in node purity (IncNodePurity), and the mtry put into the random forest (mtry)
  #' 
  #' @param response_metric this is the response variable used on the left side of the ~ in the RF. We will group by this parameter to get 
  #' the top ten predictors per this metric. 
  #' 
  #' @param predictor_metric this is the independant variable used on the right side of the ~ in the RF. We will count the number of time these predictors
  #'  are the top ten predictors per response metric. 
  
  response_metric <- enquo(response_metric)
  predictor_metric <- enquo(predictor_metric)
  
  top_preditors_ranked <- rf_dataset %>% 
    arrange(desc(X.IncMSE)) %>% 
    group_by(!! response_metric) %>% 
    slice(1:10) %>% 
    ungroup() %>% 
    group_by(!! predictor_metric) %>% 
    summarize(count_in_top_ten = n()) %>% 
    ungroup() %>% 
    arrange(desc(count_in_top_ten))
  
  ranks <- rf_dataset %>% 
    select(!! predictor_metric) %>% 
    unique() %>% 
    full_join(top_preditors_ranked) %>% 
    replace_na(list(count_in_top_ten = 0)) %>% 
    arrange(desc(count_in_top_ten))
  
  ranks
}

rf_predictors_topten <- top_ten_predictors(rf_result, fish_metric)
rf_predictors_rank <- rank_predictors(rf_result, fish_metric, landscape_metric)


#### Next Steps ####
# TODO reduce the number of fish metrics so that you have a smaller number of interested/ "good" response variables
# TODO Partial dependancy plots of those metrics with all of the interesting "good" response variables