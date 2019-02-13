library(tidyverse)
library(randomForest)
library(vegan)

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

## write.csv(habitat, "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Habitat.csv", na = ".")
habitat_2 <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Habitat.csv", stringsAsFactors = F, header = T, row.names = 1, na = ".")

habitat_2 <- habitat_2 %>% select(-c("Water_Temperature", "DO", "DO_Saturation", "Conductivity", "pH",
                                     "Nitrate", "Ammonia", "Orthophosphate", "Turbidity", "Visual_Water_Clarity", "Chloride", "Chloride_QU"))
habitat_3 <- na.omit(habitat_2)

mydata <- merge(fish_matrix, habitat_3, by= "Row.names"  , all.x = TRUE)
mydata <- data.frame(mydata, row.names = 'Site_ID')

# data(iris)
# 
# iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)

set.seed(13)
fish_RF1 <- randomForest(mydata ~ ., data = mydata, na.action = na.omit, ntree= 5000, mtry=4, importance= T)


##### Random Forest
set.seed(13)
fish_RF1 <- randomForest(fish_matrix$BAS ~ ., data = habitat_2, na.action = na.omit, ntree= 5000, mtry=4, importance= T)

#PLOT YOUR FORESTS IMPORTANT VARIABLES
varImpPlot(fish_RF1, type = 1)

# 
# partialPlot(RF1, envi, Wt_Area)
# partialPlot(RF1, envi, WT_Urban)
# partialPlot(RF1, envi, WT_JMin)