library(tidyverse)
# library(AICCmodavg)
library(randomForest)
library(vegan)
library(tree)

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

# id_key <- rename(id_key, site_id = Site_ID)
# 
# identical(id_key$site_id, fm$site_id)
# 
# a <- id_key$site_id 
# b <- fm$site_id
# 
# a <- sort(a)
# b <- sort(b)
# df<- data.frame(a,b)

# hab_1 <- readxl::read_excel(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Habitat_Characteristics.xlsx"), sheet = 1)

habitat_wt <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv"))
names(habitat_wt) <- str_to_lower(names(habitat_wt))
habitat_wt <- habitat_wt %>% 
  full_join(id_key, by = c("pu_gap_code"="pugap_code")) %>% 
  drop_na() %>% 
  select(-c(pu_gap_code, pu_code, gap_code, reach_name, event_date, data_source))

habitat.df <- data.frame(habitat_wt, row.names = 'site_id')
fish.df <- data.frame(fm, row.names = 'site_id')

#### Random Forest ####


set.seed(1340)
fish_RF1 <- randomForest(fish.df$richness~., data = habitat.df, na.action = na.omit, ntree= 5000, mtry=4, importance= T)

fish_RF1
imp_fish_RF1 <-importance(fish_RF1)
habitat_list <- rownames(imp_fish_RF1) 

# Partial Dependancy Plots looping over variable to create for all variables. 
# Remember y-values 
for (habitat_feature in seq_along(habitat_list)) {
  file_out <- paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF_WT_richness/fish_richness_RF1_PP_", habitat_2_list[habitat_feature], ".pdf")
  pdf(file_out)
  partialPlot(fish_RF1, habitat.df, habitat_list[habitat_feature], main = paste("Partial Dependancy Plot on", habitat_2_list[habitat_feature]), xlab = paste(habitat_2_list[habitat_feature]))
  dev.off()
}

#PLOT YOUR FORESTS IMPORTANT VARIABLES
d <- varImpPlot(fish_RF1, type = 1)
varImpPlot(fish_RF1)

### Creates the tree from the first f=placE)
tree_1 <- tree(fish.df$richness~., data = habitat.df)
plot(tree_1)
text(tree(fish.df$richness~., data = habitat.df))

### Prune the tree  to remove over fit vpredictor variables.    
#### First plot the pruned trees then find the ## of terminal branches that reduces the deviance
#### 
plot(cv.tree(tree_1, FUN = prune.tree))
plot(cv.tree(tree_1, FUN = prune.tree(best = 4)))

plot(prune.tree(tree_1,best = 4)) 
text(prune.tree(tree_1,best = 4))
