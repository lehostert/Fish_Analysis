library(tidyverse)

ds <- fish_data %>% distinct(fish_data$Site_ID)
set.seed(2020)
qc_list <- ds %>% sample_n(5, replace = F)

#TODO 


qc1 <- fish_table %>% 
  filter(fish_table$Site_ID == qc_list$`fish_data$Site_ID`[1])

qc2 <- fish_table %>% 
  filter(fish_table$Site_ID == qc_list$`fish_data$Site_ID`[2])

qc3 <- fish_table %>% 
  filter(fish_table$Site_ID == qc_list$`fish_data$Site_ID`[3])

qc4 <- fish_table %>% 
  filter(fish_table$Site_ID == qc_list$`fish_data$Site_ID`[4])

qc5 <- fish_table %>% 
  filter(fish_table$Site_ID == qc_list$`fish_data$Site_ID`[5])



write_csv()