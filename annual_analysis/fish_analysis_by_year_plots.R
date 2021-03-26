library(dplyr)
library(ggplot2)
library(purrr)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")
analysis_folder <- paste0(network_path,"/Analysis/Fish/Output/annual_analysis")

#Read in data
df <- read_csv(paste0(network_path, "/Analysis/Fish/Data/kasky_fish_ibi_and_landuse_geology_metrics_20210303.csv"))
df$event_date <- as.Date(df$event_date, format = "%m/%d/%Y")

df <- df %>%
  mutate(phase = if_else(data_source == "crep_monitoring" & lubridate::year(event_date) == 2013|lubridate::year(event_date) == 2014|lubridate::year(event_date) == 2015, 
                         "1", 
                         if_else(data_source == "crep_monitoring" & lubridate::year(event_date) == 2016|lubridate::year(event_date) == 2017,
                                 "2",
                                 if_else(data_source == "crep_monitoring" & lubridate::year(event_date) == 2018|lubridate::year(event_date) == 2019|lubridate::year(event_date) == 2020,
                                         "3",
                                         "IDNR"))))%>% 
  select(1,2,148,144,3:143,145:147)


#Clean data
fish_df <- df %>% 
  filter(individuals >19, richness > 4) %>% 
  drop_na(w_hel_percent)


response = names(fish_df)[8:78]
r = purrr::set_names(response)

expl = names(fish_df)[79:148]
e = purrr::set_names(expl)

fish_df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x = lubridate::year(event_date), y = richness, group = lubridate::year(event_date))) +
  geom_boxplot()+
  labs(x = "Year", y = "Shannon Richness", title = "Shannon Richness per CREP Monitoring Random Sites")

# ggsave("richness_random_sites_boxplot.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

# ##### Function Trial
# 
# fish_df_random <- fish_df %>%
#   filter(crep_site_type == "random")
# 
# boxplot_fun = function(y) {
#   ggplot(fish_df_random, aes(x = lubridate::year(event_date), y = .data[[y]],  group = lubridate::year(event_date))) +
#     geom_boxplot()+
#     labs(x = "Year", y = paste0(y), title = paste0("CREP Monitoring Random Sites- ", stringr::str_to_title(y), " by Year"))
# }
# 
# 
# boxplot_fun(y = "richness")


### By Year
boxplot_fun = function(dat, y) {
  ggplot(dat, aes(x = lubridate::year(event_date), y = .data[[y]],  group = lubridate::year(event_date))) +
    geom_boxplot()+
    labs(x = "Year", y = paste0(y), title = paste0("CREP Monitoring Random Sites- ", stringr::str_to_title(y), " by Year"))
}


boxplot_fun(dat = fish_df_random, y = "richness")

map(r, ~boxplot_fun(dat = fish_df_random, .x))

###By Phase

boxplot_fun_phase = function(dat, y) {
  ggplot(dat, aes(x = phase, y = .data[[y]],  group = phase)) +
    geom_boxplot()+
    labs(x = "Phase", y = paste0(y), title = paste0("CREP Monitoring Random Sites- ", stringr::str_to_title(y), " by Phase"))
}


boxplot_fun_phase(dat = fish_df_random, y = "richness")

map(r, ~boxplot_fun_phase(dat = fish_df_random, .x))

proposal <- df %>% 
  filter(data_source == "crep_monitoring")

proposal_sites <- df %>%
  filter(data_source == "crep_monitoring") %>% 
  select(reach_name) %>% 
  unique()

