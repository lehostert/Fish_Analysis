library(tidyverse)

ds <- fish_data %>% distinct(fish_data$Site_ID)
set.seed(2020)
qc_list <- ds %>% sample_n(5, replace = F)
