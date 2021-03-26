library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")
analysis_folder <- paste0(network_path,"/Analysis/Fish/Output/annual_analysis")

#What I want to understand is that average value of each various fish metric by year are they changing throughout time annually 

df <- read_csv(paste0(network_path, "/Analysis/Fish/Data/kasky_fish_ibi_and_landuse_geology_metrics_20210303.csv"))

df$event_date <- as.Date(df$event_date, format = "%m/%d/%Y")

fish_df <- df %>% 
  filter(individuals >19, richness > 4) %>% 
  drop_na(w_hel_percent) 


# fish_df$event_year <- lubridate::year(fish_df$event_date)

summary(fish_df)

#### Plots ####
theme_update(plot.title = element_text(hjust = 0.5))

#### Year ####

##### All Sites #####
# fish_df %>% 
#   ggplot2::ggplot(aes(x = lubridate::year(event_date) , y = richness, group = lubridate::year(event_date))) +
#   geom_violin()+
#   labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring Random Sites")
# 
# ggsave("richness_random_sites_scatter.pdf", path = analysis_folder)
  
##### Random #####
fish_df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x = lubridate::year(event_date), y = richness, group = lubridate::year(event_date))) +
  geom_violin()+
  labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring Random Sites")

ggsave("richness_random_sites_violin.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

fish_df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x = lubridate::year(event_date), y = richness, group = lubridate::year(event_date))) +
  geom_boxplot()+
  labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring Random Sites")

ggsave("richness_random_sites_boxplot.pdf", width = 8, height = 8, path = analysis_folder, units = "in")


##### All CREP Sites #####
fish_df %>% 
  filter(data_source == "crep_monitoring") %>% 
  ggplot2::ggplot(aes(x = lubridate::year(event_date), y = richness, group = lubridate::year(event_date))) +
  geom_boxplot()+
  labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring All Sites")

ggsave("richness_all_crep_sites_boxplot.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

fish_df %>% 
  filter(data_source == "crep_monitoring") %>% 
  ggplot2::ggplot(aes(x = lubridate::year(event_date), y = richness, group = lubridate::year(event_date))) +
  geom_violin()+
  labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring All Sites")
ggsave("richness_all_crep_sites_violin.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

### Cut down to only fish metrics ###

<- fish_df %>% 
  

## Calculate the metrics for each of the random sites and look at how those are changing throughout time. 
## So per year