library(tidyverse)
library(vegan)
library(stringi)

# Load IL Fish Species Codes and Native Status
il_fish_path_pc <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Fish_Trait_Matrix_Base.csv"
il_fish_spp <- read.csv(il_fish_path_pc, header = T, stringsAsFactors = F, na ="")
# Not all data from various trait datasets is capitalized the same stringing to lower to avoid mismatches.
il_fish_spp$Fish_Species_Common <- str_to_lower(il_fish_spp$Fish_Species_Common)
# Remove Hybrid and Unidentified Species for now 
il_fish_spp <-  il_fish_spp %>% filter(is.na(Unidentified_Species)) %>% filter(is.na(Hybrid)) 
names(il_fish_spp)[names(il_fish_spp) == 'Species_Code'] <- 'Fish_Species_Code'

### Load VT Traits
# VT Fish Traits
# Emmanuel Frimpong, and Paul L. Angermeier, 200811, Fish Traits Database: USGS,  https://doi.org/10.5066/F7WD3ZH8.
# Accessed from USGS sciencebase through https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0 on 2/7/19
VTT_path <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/VT_FishTraits/FishTraits_14.3.csv"
# VTT_path <- "C:/Users/lhostert/Desktop/Fish/VT_FishTraits/FishTraits_14.3.csv"
VTT_dataset <- read.csv(VTT_path, header = T, stringsAsFactors = F, na ="")
names(VTT_dataset)[names(VTT_dataset) == 'COMMONNAME'] <- 'Fish_Species_Common'
VTT_dataset$Fish_Species_Scientific <- paste(VTT_dataset$GENUS, VTT_dataset$SPECIES, sep = " " )
VTT_dataset$Fish_Species_Common <- str_to_lower(VTT_dataset$Fish_Species_Common)

### Compare VTT List to that of IL Fish list and then manually fix errors. 
# sci_names <- left_join(il_fish_spp, VTT_dataset, by = c('Fish_Species_Common')) %>% select(c('Fish_Species_Common', 'Fish_Species_Scientific.x','Fish_Species_Scientific.y'))
# sci_names$Match <- str_detect(sci_names$Fish_Species_Scientific.x, sci_names$Fish_Species_Scientific.y)

### Combine VTT Fish TRaits with IL Fish Species List. 
il_fish_traits <- left_join(il_fish_spp, VTT_dataset, by = c('Fish_Species_Scientific','Fish_Species_Common'))

### Load Tolerance Data
# Tolerance Data available from Ecological National Synthesis (ENS) Project "Fish Traits & Tolerance Data"
# Accessed from https://water.usgs.gov/nawqa/ecology/data.html on 2/21/19
# See M.R. Meador and D.M. Carlisle.  2007.  Quantifying tolerance indicator values for common fish species of the United States. Ecological Indicators, 7:329-338.

tolerance_2 <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/ENS_FishToleranceIndicatorValuesTables.xlsx", 
                                  sheet = "W_Averages_With_Tolerance", col_names = T, na = "")

# Load other tolerance data emailed from M.R. Meador to Dr. Yong Cao 
tol_NRSA <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/NRSA_Tolerant.xlsx", sheet = 1, col_names = T, na = "")
tol_USGS <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/USGS_tolerance_indicators.xlsx", sheet = 1, col_names = T, na = "")

tolerance <- full_join(tol_NRSA, tol_USGS, by= "Common Name") %>% select(-c(Max_Water_Temp))
names(tolerance)[names(tolerance) == 'Common Name'] <- 'Fish_Species_Common'
names(tolerance)[names(tolerance) == 'Scientific Name'] <- 'Fish_Species_Scientific'
tolerance$Fish_Species_Common <- str_to_lower(tolerance$Fish_Species_Common)

il_fish_traits <- left_join(il_fish_traits, tolerance_2, by =c('Fish_Species_Scientific','Fish_Species_Common'))

write.csv(il_fish_traits,"//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Illinois_fish_traits_complete.csv", na = "")

### Load Fish Data from 2014-2018 from CREP_Database 
kasky_fish_table <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data.xlsx", sheet = 1, col_names = T) %>%
  select(-c(Fish_Abundance_ID))
kasky_fish_table$Site_ID <-paste(str_replace_all(kasky_fish_table$Reach_Name, "[:blank:]", ""), str_replace_all(kasky_fish_table$Event_Date,"-",""), sep = "_") 

### Append trait data to fish data from CREP 2014-2018
kasky_fish_table_with_traits <- kasky_fish_table %>% 
  select(c(Site_ID, Fish_Species_Code, Fish_Species_Count))%>%
  left_join(il_fish_traits, by = 'Fish_Species_Code')

### Tranform Fish Data from 2014-2018 CREP_Database into sparse dataframe for diversity and evenness calulation with vegan. 
kasky_fish_matrix <- kasky_fish_table %>%
  select(-c(PU_Gap_Code, Reach_Name, Event_Date)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
### vegan package requires row names so create row names as Site_ID then remove that collumn. 
row.names(kasky_fish_matrix) <- kasky_fish_matrix$Site_ID
kasky_fish_matrix <- kasky_fish_matrix %>% select(-c(Site_ID))

##############################################################
##### Begin calulating values for Site_Metrics_Table
INDIVIDUALS <- rowSums(kasky_fish_matrix)
# *RICHNESS <- specnumber(kasky_fish_matrix) produces same as below but requires vegan to work
RICHNESS <- rowSums(kasky_fish_matrix != 0) 
DIVERSITY <- vegan::diversity(kasky_fish_matrix, index = "shannon")
J_evenness  <- function(x) {
  diversity(x)/log(specnumber(x))
  }

EVENNESS <- J_evenness(kasky_fish_matrix)

kasky_site_metrics <- data.frame(INDIVIDUALS,RICHNESS,DIVERSITY,EVENNESS)



##This works but is not what you want for metrics table
# if(il_fish_traits$Family[il_fish_traits$Fish_Species_Code=='WHS']== 'Catostomidae') {print(TRUE)}



