library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

#### Random Forest ####


# metrics_envi.dat <- read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"), row.names = "site_id")


# set.seed(2020)
# TODO make this a loop able to take in a table of metrics and determined mtry no.

fish_metric <- "leucptax"
var_mtry <- 3
fish_RF <- randomForest(metrics_envi.dat$leucptax~link+dlink+c_order+dorder+wt_total_sqme+
                          wt_gdd+wt_jul_mnx+wt_prec+
                          c_br50+c_br100+c_brg100+wt_br50+wt_br100+wt_brg100+
                          wt_br_carbonate+wt_br_sandstone+wt_br_shale+wt_rocky+wt_alluvium_fluvial+wt_coarse_moraine+wt_coarse+wt_colluvium+wt_dune+
                          wt_fines+wt_lacustrine+wt_loess+wt_medium_moraine+wt_outwash+wt_peat_muck+wt_icecontact+
                          w_darcyx+wt_darcyx+w_permx+wt_permx+
                          r_open_wet+rt_grassland+w_forest_total+w_agriculture+w_grassland+w_urban+w_open_wet+w_wetland_total+wt_forest_total+wt_urban+wt_grassland+wt_agriculture+
                          bigriver+damdwl+damdw+damupl+damup+missi+pond+pond_area+pondwl+pondwa+ponddw+pondupl+pondupa+pondup+
                          sinuous+w_total_sqm+w_slope+wt_slope+gradient+
                          w_crepcrp_percent+w_hel_percent,
                        data = metrics_envi.dat, na.action = na.omit, ntree= 5000, mtry= var_mtry, importance= T)

fish_RF
imp_fish_RF <-importance(fish_RF)
habitat_list <- rownames(imp_fish_RF) 

imp_fish_RF <-data.frame(imp_fish_RF)

# Partial Dependancy Plots looping over variable to create for all variables. 
# Remember y-values 
# for (habitat_feature in seq_along(habitat_list)) {
#   file_out <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_",fish_metric,"/fish_",fish_metric, "_RF_PP_", habitat_list[habitat_feature], ".pdf")
#   pdf(file_out)
#   partialPlot(fish_RF, metrics_envi.dat , habitat_list[habitat_feature], main = paste("Partial Dependancy Plot on", habitat_list[habitat_feature]), xlab = paste(habitat_list[habitat_feature]))
#   dev.off()
# }

#PLOT YOUR FORESTS IMPORTANT VARIABLES
pdf(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry/fish_RF_VarImportance_",fish_metric, ".pdf"), width = 9)
varImpPlot(fish_RF)
dev.off()

imp_fish_RF <- tibble::rownames_to_column(imp_fish_RF , "landscape_metric")
imp_fish_RF <- imp_fish_RF %>% rename( =, )

write.csv(imp_fish_RF, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry/fish_RF_VarImportance_",fish_metric, ".csv"), row.names = T)

######### Read  all CSV##########
library(tidyverse)

### Discharge
rf_filenames <- list.files(path="//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry", pattern= "*.csv")
rf_fullpath = file.path("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry", rf_filenames)
rf_fulldataset <- do.call("cbind",lapply(rf_fullpath, FUN = function(files){read.csv(files, stringsAsFactors = FALSE, na.strings = ".")}))
write.csv(rf_fulldataset, file= paste0("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_landscape_bestmtry_RF_VarImportance.csv"), na= "", row.names = F)

algntax <- read_csv(file = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry/fish_RF_VarImportance_algntax.csv") %>% 
  rename(hab_metric = 1)
algptax <- read_csv(file = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_best_mtry/fish_RF_VarImportance_algptax.csv") %>% 
  rename(hab_metric = 1)

combined <- algntax %>% full_join(algptax, by = "hab_metric")



# tbl <- sapply(rf_filenames, read_csv, simplify=FALSE) %>% 
#   bind_rows(.id = "id")
