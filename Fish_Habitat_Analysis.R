library(tidyverse)
library(randomForest)
library(vegan)
library(tree)

### Set up fish and habitat data for comparison.
fish <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data.xlsx", sheet = 1)
habitat <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Habitat_Characteristics.xlsx", sheet = 1)

habitat$Site_ID <-paste(str_replace_all(habitat$Reach_Name, "[:blank:]", ""), str_replace_all(habitat$Event_Date,"-",""), sep = "_") 

habitat <- habitat %>%
  select(-c(PU_Gap_Code, Reach_Name, Event_Date))

habitat <- data.frame(habitat, row.names = 'Site_ID')

fish$Site_ID <-paste(str_replace_all(fish$Reach_Name, "[:blank:]", ""), str_replace_all(fish$Event_Date,"-",""), sep = "_") 

fish_matrix <- fish %>%
  select(-c(Fish_Abundance_ID, PU_Gap_Code, Reach_Name, Event_Date)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)

fish_matrix <- data.frame(fish_matrix, row.names = 'Site_ID')
# write.csv(fish_matrix, "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish.csv", na = ".")
fish_2 <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_2.csv", stringsAsFactors = T, header = T, row.names = 1, na = ".")
# write.csv(habitat, "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Habitat.csv", na = ".")

## Habitat 2 takes habitat data and removes all Water Chemistry Data and ~5 sites that have incomplete habitat data. 
habitat_2 <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Habitat_2.csv", stringsAsFactors = T, header = T, row.names = 1, na = ".")

# habitat_3 <- habitat %>% select(-c("Water_Temperature", "DO", "DO_Saturation", "Conductivity", "pH",
#             "Nitrate", "Ammonia", "Orthophosphate", "Turbidity", "Visual_Water_Clarity", "Chloride", "Chloride_QU"))
# habitat_3 <- na.omit(habitat_3)

habitat_2 <- habitat_2 %>% select(-c("Reach_Length", "Mean_Width", "DS_Width", 
                                     "MD_Width", "US_Width", "Site_Type"))

##### Tree
fish_tree_1 <- tree(fish_2$BAS~., data = habitat_2)
plot(fish_tree_1)
text(tree(fish_2$BAS~., data = habitat_2))

##### Random Forest
set.seed(13)
fish_RF1 <- randomForest(fish_2$BAS~ ., data = habitat_2, na.action = na.omit, ntree= 5000, mtry=4, importance= T)

fish_RF1
imp_fish_RF1 <-importance(fish_RF1)
habitat_2_list <- rownames(imp_fish_RF1) 

# Partial Dependancy Plots looping over variable to create for all variables. 
for (habitat_feature in seq_along(habitat_2_list)) {
  file_out <- paste0("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_RF1/fish_RF1_PP_", habitat_2_list[habitat_feature], ".pdf")
  pdf(file_out)
  partialPlot(fish_RF1, habitat_2, habitat_2_list[habitat_feature], main = paste("Partial Dependancy Plot on", habitat_2_list[habitat_feature]), xlab = paste(habitat_2_list[habitat_feature]))
  dev.off()
}

#PLOT YOUR FORESTS IMPORTANT VARIABLES
varImpPlot(fish_RF1, type = 1)
varImpPlot(fish_RF1)

#######################################
## Take out sites reach length >150m
#######################################
kasky <-cbind(fish_2,habitat_2)

kasky$Site_ID <- row.names(kasky)

kasky_150 <- filter(kasky, Reach_Length <151)
kasky_150 <- data.frame(kasky_150, row.names = 'Site_ID')

feature_names <- names(kasky_150)
first_column_of_interest <- which(feature_names=="BAS")
last_column_of_interest <- which(feature_names=="YLB")
relevant_feature_names <- feature_names[first_column_of_interest:last_column_of_interest]
k150_fish <- kasky_150 %>% select(relevant_feature_names)

first_column_of_interest_habitat <- which(feature_names=="Site_Type")
last_column_of_interest_habitat <- which(feature_names=="QHEI_Score")
relevant_feature_names_habitat <- feature_names[first_column_of_interest_habitat :last_column_of_interest_habitat]
k150_habitat <- kasky_150 %>% select(relevant_feature_names_habitat)

##### Tree
fish_tree_k150 <- tree(k150_fish$BAS~., data = k150_habitat)
plot(fish_tree_k150)
text(tree(k150_fish$BAS~., data = k150_habitat))

##### Random Forest
set.seed(13)
k150_RF1 <- randomForest(k150_fish$BAS~ ., data = k150_habitat, na.action = na.omit, ntree= 5000, mtry=4, importance= T)

k150_RF1
importance(k150_RF1)

varImpPlot(k150_RF1, type = 1)
varImpPlot(k150_RF1)

partialPlot(k150_RF1, habitat_2, R_Bare_Bank)
