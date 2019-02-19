library(tidyverse)
library(vegan)
library(stringi)

###Load IL Fish Codes and Native Status
il_fish_path <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Fish_Trait_Matrix_Base.csv"
# il_fish_path <- "C:/Users/lhostert/Desktop/Fish/Fish_Trait_Matrix_Base.csv"
il_fish_spp <- read.csv(il_fish_path, header = T, stringsAsFactors = F, na ="")
il_fish_spp$Fish_Species_Common <- str_to_lower(il_fish_spp$Fish_Species_Common)
il_fish_spp <-  il_fish_spp %>% filter(is.na(Unidentified_Species)) %>% filter(is.na(Hybrid)) 
names(il_fish_spp)[names(il_fish_spp) == 'Species_Code'] <- 'Fish_Species_Code'

### VT Traits
# Load VT Fish Traits accessed from USGS sciencebase through this link https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0 on 2/7/19
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

#### Add Tolerance Data
tol_NRSA <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/NRSA_Tolerant.xlsx", sheet = 1, col_names = T, na = "")
tol_USGS <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/USGS_tolerance_indicators.xlsx", sheet = 1, col_names = T, na = "")

tolerance <- full_join(tol_NRSA, tol_USGS, by= "Common Name") %>% select(-c(Max_Water_Temp))
names(tolerance)[names(tolerance) == 'Common Name'] <- 'Fish_Species_Common'
names(tolerance)[names(tolerance) == 'Scientific Name'] <- 'Fish_Species_Scientific'
tolerance$Fish_Species_Common <- str_to_lower(tolerance$Fish_Species_Common)

il_fish_traits <- left_join(il_fish_traits, tolerance, by =c('Fish_Species_Scientific','Fish_Species_Common'))

### Load Fish Data from 2014-2018 from CREP_Database 
kasky_fish_table <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data.xlsx", sheet = 1, col_names = T) %>%
  select(-c(Fish_Abundance_ID))
kasky_fish_table$Site_ID <-paste(str_replace_all(kasky_fish_table$Reach_Name, "[:blank:]", ""), str_replace_all(kasky_fish_table$Event_Date,"-",""), sep = "_") 

### Tranform into full species Matrix
kasky_fish_matrix <- kasky_fish_table %>%
  select(-c(PU_Gap_Code, Reach_Name, Event_Date)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
 
row.names(kasky_fish_matrix) <- kasky_fish_matrix$Site_ID
kasky_fish_matrix <- kasky_fish_matrix %>% select(-c(Site_ID))
kasky_species_list <- colnames(kasky_fish_matrix)

# #If The above did not work so exporting and re-importing 
# write.csv(kasky_fish_matrix,"//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Matrix.csv", na = ".", row.names = F)
# kasky_fish_matrix<- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Fish_Abundance_Matrix.csv", header = T, row.names = 1, na = "." )

### Add traits to kasky_fish_tablet
## Attempt to make a new table with all the fish traits attached to the fish Abundance data. 
## This should help functions like CATONTAX calculate

# kasky_fish_table_with_traits <- kasky_fish_table %>%
#   select(-c(PU_Gap_Code, Reach_Name, Event_Date)) %>%
#   left_join(il_fish_traits, by = "Fish_Species_Code")


INDIVIDUALS <- rowSums(kasky_fish_matrix)
RICHNESS <- rowSums(kasky_fish_matrix != 0) 
# #RICHNESS <- specnumber(kasky_fish_matrix) produces same as above but requires vegan to work
DIVERSITY <- vegan::diversity(kasky_fish_matrix, index = "shannon")
J_evenness  <- function(x) {
  diversity(x)/log(specnumber(x))
  }

EVENNESS <- J_evenness(kasky_fish_matrix)

# CATONTAX <- if spp./ taxa in il_fish_traits is Catostomid then find that Species_Code in kasky_fish_matrix 
# CATONTAX <- ifelse(il_fish_traits$Family = Catostomidae,                   )

##Take 1
CATONTAX <- 0
for(spp in colnames(kasky_fish_matrix)) {
  if(il_fish_traits$Family[il_fish_traits$Fish_Species_Code==spp]== 'Catostomidae'){
    CATONTAX[spp] <- paste("TRUE")
  } else CATONTAX[spp] <- paste("Bad News :(")
}

## Take 2
for(spp in seq_along(kasky_species_list)) {
  if(il_fish_traits$Family[il_fish_traits$Fish_Species_Code==spp]== 'Catostomidae'){
    print("spp, TRUE")
  }else print("yes!")
}


##This works
if(il_fish_traits$Family[il_fish_traits$Fish_Species_Code=='WHS']== 'Catostomidae') {print(TRUE)}

kasky_site_metrics <- data.frame(INDIVIDUALS,RICHNESS,DIVERSITY,EVENNESS)


# kasky_fish_matrix %>% select(-c(kasky_fish_matrix$Site_ID))
# kasky_site_metrics <- kasky_fish_matrix$Site_ID 
# kasky_site_metrics <- as.data.frame(kasky_site_metrics)
# kasky_site_metrics <- data.frame(kasky_fish_matrix$Site_ID,SPECIES,DIVERSITY,EVENESS)

### If you need to test that the Site_ID is unique look at the length of the following dataframes.
# Unique <- kasky_fish %>% distinct(Reach_Name, Event_Date, Site)
# unique_site <- kasky_fish %>% distinct(Site)


# write.csv(DSC_dataset, file= "//INHS-Bison/ResearchData/Groups/Kaskaskia CREP/Data/Data_IN/DB_Ingest/DSC_2018.csv")
# fsh <- read.csv("~/Github/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages.csv", header = T, na = ".", row.names = 1)
# 
# evenness  <- function(x) {
#   diversity(x)/log(specnumber(x))
# }
# 
# f_diversity <- diversity(fsh, index = "shannon")
# J <- evenness(fsh)
# 
# Fish_metrics <- data.frame(J,f_diversity)
# 
