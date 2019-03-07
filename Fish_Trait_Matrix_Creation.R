library(tidyverse)
library(vegan)
library(stringi)

# Load IL Fish Species Codes and Native Status
il_fish_path_pc <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Fish_Trait_Matrix_Base.csv"
il_fish_spp <- read.csv(il_fish_path_pc, header = T, stringsAsFactors = F, na ="")
# Not all data from various trait datasets is capitalized the same stringing to lower to avoid mismatches.
il_fish_spp$Fish_Species_Common <- str_to_lower(il_fish_spp$Fish_Species_Common)

### Load VT Traits
# VT Fish Traits
# Emmanuel Frimpong, and Paul L. Angermeier, 200811, Fish Traits Database: USGS,  https://doi.org/10.5066/F7WD3ZH8.
# Accessed from USGS sciencebase through https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0 on 2/7/19
VTT_path <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/VT_FishTraits/FishTraits_14.3.csv"
VTT_dataset <- read.csv(VTT_path, header = T, stringsAsFactors = F, na ="")
names(VTT_dataset)[names(VTT_dataset) == 'COMMONNAME'] <- 'Fish_Species_Common'
VTT_dataset$Fish_Species_Scientific <- paste(VTT_dataset$GENUS, VTT_dataset$SPECIES, sep = " " )
VTT_dataset$Fish_Species_Common <- str_to_lower(VTT_dataset$Fish_Species_Common)

### Compare VTT List to that of IL Fish list and then manually fix errors. 
# sci_names <- left_join(il_fish_spp, VTT_dataset, by = c('Fish_Species_Common')) %>% select(c('Fish_Species_Common', 'Fish_Species_Scientific.x','Fish_Species_Scientific.y'))
# sci_names$Match <- str_detect(sci_names$Fish_Species_Scientific.x, sci_names$Fish_Species_Scientific.y)

### Combine VTT Fish TRaits with IL Fish Species List. 
il_fish_traits <- left_join(il_fish_spp, VTT_dataset, by = c('Fish_Species_Scientific','Fish_Species_Common'))

# Load USGS Tolerance Data
# M.R. Meador and D.M. Carlisle.  2007.  Quantifying tolerance indicator values for common fish species of the United States. Ecological Indicators, 7:329-338.
# Available from USGS Ecological National Synthesis (ENS) Project "Fish Traits & Tolerance Data"  https://water.usgs.gov/nawqa/ecology/data.html 
# Specifically <https://water.usgs.gov/nawqa/ecology/pubs/FishToleranceIndicatorValuesTables.xls> (accessed 2019-02-21)

tolerance_2 <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/ENS_FishToleranceIndicatorValuesTables.xlsx", 
                                  sheet = "W_Averages_With_Tolerance", col_names = T, na = "")

il_fish_traits <- left_join(il_fish_traits, tolerance_2, by =c('Fish_Species_Scientific','Fish_Species_Common'))

### Load other tolerance data 
# These are non-public data emailed from M.R. Meador to Dr. Yong Cao
# They are some raw values that were used in prepr of Meador's 2007 paper
# not used here so program can continue to use public data sources that are citeable.

# tol_NRSA <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/NRSA_Tolerant.xlsx", sheet = 1, col_names = T, na = "")
# tol_USGS <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Tolerance_Tables/USGS_tolerance_indicators.xlsx", sheet = 1, col_names = T, na = "")
# tolerance <- full_join(tol_NRSA, tol_USGS, by= "Common Name") %>% select(-c(Max_Water_Temp))
# names(tolerance)[names(tolerance) == 'Common Name'] <- 'Fish_Species_Common'
# names(tolerance)[names(tolerance) == 'Scientific Name'] <- 'Fish_Species_Scientific'
# tolerance$Fish_Species_Common <- str_to_lower(tolerance$Fish_Species_Common)
# 
# il_fish_traits <- left_join(il_fish_traits, tolerance, by =c('Fish_Species_Scientific','Fish_Species_Common'))


###Calulate Additional Binary Categories that will be used for metric calculations later

il_fish_traits$HERBIVORE <- ifelse(il_fish_traits$ALGPHYTO == '1',
                                   1,
                                   ifelse(il_fish_traits$MACVASCU == '1',
                                          1,
                                          ifelse(il_fish_traits$DETRITUS == '1',
                                                 1,
                                                 0
                                          )
                                   )
)

il_fish_traits$OMNIVORE <- ifelse((il_fish_traits$HERBIVORE + il_fish_traits$INVLVFSH + il_fish_traits$FSHCRCRB + il_fish_traits$BLOOD + il_fish_traits$EGGS + il_fish_traits$OTHER) >1,
                                  1,
                                  0
)

il_fish_traits$LITHOPHILIC <- ifelse (il_fish_traits$GRAVEL == '1',
                                      1,
                                      ifelse(il_fish_traits$COBBLE == '1',
                                             1,
                                             ifelse(il_fish_traits$BOULDER == '1',
                                                    1,
                                                    0
                                             )
                                      )
)

il_fish_traits$BENTHIC_INSECTIVORE <- ifelse((il_fish_traits$BENTHIC + il_fish_traits$INVLVFSH) == 2,
                                             1,
                                             0
)



il_fish_traits<- il_fish_traits %>% 
  tidyr::replace_na(list(Nonnative = 0, Hybrid = 0, Unidentified_Species = 0))

il_fish_traits$Nonnative <- ifelse(il_fish_traits$Nonnative == 'N', 1, 0)
il_fish_traits$Hybrid <- ifelse(il_fish_traits$Hybrid == 'H', 1, 0)
il_fish_traits$Unidentified_Species <- ifelse(il_fish_traits$Unidentified_Species == 'U', 1, 0)

# Write it up
write.csv(il_fish_traits,"//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Illinois_fish_traits_complete.csv", na = "", row.names = F)

# END #
#############################################
