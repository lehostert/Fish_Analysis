library(tidyverse)

sites_db <-read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Established_Locations_db.csv"))
sites_18 <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Data/Data_IN/Established_Locations.xlsx"))
sites_19 <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Data/Data_IN/Sites_2019.xlsx"))

sites_18$Reach_Name <- str_replace_all(sites_18$Reach_Name, " ", "")

### Understand what is going on
sites_18_name <- sites_18 %>% 
  select(PU_Gap_Code, Reach_Name)

sites_19_name <- sites_19 %>% 
  select(PU_Gap_Code, Reach_Name)  

sites_18_loc <- sites_18 %>% 
  select(PU_Gap_Code, Reach_Name, Latitude, Longitude)

sites_19_loc <- sites_19 %>% 
  select(PU_Gap_Code, Reach_Name, Latitude, Longitude)  
  
repeat_sites <- dplyr::intersect(sites_18_name, sites_19_name)
repeat_location <- dplyr::intersect(sites_18_loc, sites_19_loc)

combined <- full_join(sites_18, sites_19, by = c("PU_Gap_Code", "Reach_Name"))

combined_simple <- combined %>%
  select(PU_Gap_Code, Reach_Name, Latitude.x, Latitude.y, Longitude.x, Longitude.y, Stream_Name.x, Stream_Name.y, Site_Type.x, Site_Type.y) %>% 
  drop_na(Latitude.y)

combined_simple$Lat_same <- near(combined_simple$Latitude.x, combined_simple$Latitude.y)
combined_simple$Lon_same <- near(combined_simple$Longitude.x, combined_simple$Longitude.y)
combined_simple$type_same <- combined_simple$Site_Type.x == combined_simple$Site_Type.y

combined_simple_filt <- combined_simple %>% 
  filter(Lat_same == "FALSE")

## keep all db version
sites <- sites_18 %>% 
  full_join(sites_19, by = c("PU_Gap_Code", "Reach_Name")) %>% 
  mutate(Site_Type = coalesce(Site_Type.x, Site_Type.y)) %>% 
  select(PU_Gap_Code, Reach_Name, Site_Type)

#Update db version with 
library(data.table)
site_test <- sites_18
setDT(site_test)[setDT(sites_19), on= c("PU_Gap_Code","Reach_Name"), c("Latitude", "Longitude", "Stream_Name","Site_Type"):=.(i.Latitude, i.Longitude, i.Stream_Name, i.Site_Type)][]
 
site_test_final <- full_join(site_test, sites_19) %>% 
  select(PU_Code, Gap_Code, PU_Gap_Code, Reach_Name, Latitude, Longitude, Stream_Name, Site_Type)

write_csv(site_test_final, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Site_List_2013-2019.csv"))
