library(tidyverse)
library(randomForest)
library(vegan)

alt_df <- readxl::read_excel("C:/Users/lhostert/Desktop/Fish_Habitat_Characteristics_New.xlsx", sheet = 1)
new_df <- readxl::read_excel("C:/Users/lhostert/Desktop/Fish_Habitat_Characteristics.xlsx", sheet = 1)
# old_df <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Invert/CREP_Invert_Habitat_Data.csv", stringsAsFactors = F, na = ".") 

# names(new_df)
# names(old_df)

new_df <- new_df %>% select(-c(Habitat_IHI_ID, Habitat_IHI_PU_Gap_Code, Habitat_IHI_Reach_Name, Habitat_IHI_Event_Date,
                               Habitat_QHEI_ID, Habitat_QHEI_PU_Gap_Code, Habitat_QHEI_Event_Date, Habitat_QHEI_Reach_Name, 
                               Water_Chemistry_Field_ID, Water_Chemistry_Field_PU_Gap_Code, Water_Chemistry_Field_Reach_Name, Water_Chemistry_Field_Event_Date))

# alt_df <- alt_df %>% select(-c(Site_Type))
# old_df <- old_df %>% select(-c(DOY))

names(new_df)[names(new_df) == 'Fish_Abundance_PU_Gap_Code'] <- 'PU_Gap_Code'
names(new_df)[names(new_df) == 'Fish_Abundance_Reach_Name'] <- 'Reach_Name'
names(new_df)[names(new_df) == 'Fish_Abundance_Event_Date'] <- 'Event_Date'

# old_df$Event_Date <- as.Date(old_df$Event_Date, format = "%m/%d/%Y")
# setequal(new_df, old_df)

setequal(new_df, alt_df)
## Compare VTT List to that of IL Fish list and then manually fix errors. 

# old_site <- paste(old_df$PU_Gap_Code, old_df$Reach_Name, old_df$Event_Date, sep = "_")
new_site <- paste(new_df$PU_Gap_Code, new_df$Reach_Name, new_df$Event_Date, sep = "_")
alt_site <- paste(alt_df$PU_Gap_Code, alt_df$Reach_Name, alt_df$Event_Date, sep = "_")
compare <- data.frame(alt_site, new_site)
# compare$Match <- str_detect(compare$new_site, compare$old_site)
compare$Match <- ifelse(compare$alt_site == compare$new_site, "TRUE", "FALSE")