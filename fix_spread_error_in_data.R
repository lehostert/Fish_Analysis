
fd <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data.csv"), na = "", stringsAsFactors = F)


fish_data_cleaned <- fd %>% select(-c(Fish_Abundance_ID)) %>% 
  group_by(PU_Gap_Code, Reach_Name, Event_Date, Fish_Species_Code) %>% 
  summarise(Fish_Species_Count = sum(Fish_Species_Count))

write_csv(fish_data_cleaned, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data_CREP_2013-2019.csv"), na = "")



fish_data <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/idnr_kaskaskia_basin_survey_data_1997-2012_READY_ef.csv"), na = "", stringsAsFactors = F)
names(fish_data) <- names(fish_data) %>% 
  stringr::str_replace_all("_", " ") %>% 
  stringr::str_to_title() %>% 
  stringr::str_replace_all("[:blank:]", "_")

fish_data$Event_Date <- fish_data$Date
fish_data <- rename(fish_data, Fish_Species_Count = Count)

fd <- fish_data

fish_data_cleaned <- fd %>% 
  group_by(Pugap_Code, Reach_Name, Event_Date, Fish_Species_Code) %>% 
  summarise(Fish_Species_Count = sum(Fish_Species_Count))

write_csv(fish_data_cleaned, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/idnr_kaskaskia_basin_survey_data_1997-2012_cleaned.csv"), na = "")


# FYI When year is full specified (ie. 1999) you should use %Y otherwise %y (ie. 99)