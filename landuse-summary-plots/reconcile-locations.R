library(tidyverse)

sites_db <-read.csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Established_Locations_db.csv"))
sites_18 <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Data/Data_IN/Established_Locations.xlsx"))
sites_19 <- readxl::read_xlsx(path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Data/Data_IN/Sites_2019.xlsx"))

sites_18$Reach_Name <- str_replace_all(sites_18$Reach_Name, " ", "")

# sites_18 <- sites_18 %>% 
#   rename(old_reach_name = Reach_Name, Reach_Name = reach_name_new)

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


library(data.table)
sites <- sites_18
sites[sites_19, on=.(Reach_Name), c("L", "N"):=.(i.L, i.N)][]