library(tidyverse)
library(ggplot2)
library(viridis)
library(GGally)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")
network_path <- paste0(network_prefix, "/ResearchData/Groups/Kaskaskia_CREP")

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- paste0(network_path,"/Fish/Output/il-afs-plots")

## Read in data
df <- read_csv(paste0(network_path, "/Analysis/Fish/Data/kasky_fish_ibi_and_landuse_geology_metrics_20210303.csv"))

fish_df <- df %>% 
  filter(individuals >19, richness > 4) %>% 
  drop_na(w_hel_percent) %>% 
  select(-c(contains("ntax"), contains("toltax"))) %>% 
  select(-c(contains("wt_"))) %>% 
  select(-c(missi, pond, pond_area, damdwl, damdw, bigriver)) %>% 
  select(-c(mean_width, mean_depth, reach_length))

summary(fish_df)

ggcorr(fish_df[,6:77], size = 1, layout.exp = 10)



#### Plots ####
theme_update(plot.title = element_text(hjust = 0.5))

#### Richness ####
# Shannon Richness - CRP % Random Sites
fish_df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = richness, color = c_order)) +
  geom_point()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP % in Local Watersheds- Random Sites",
       caption = "Scatterplot ")

ggsave("richness_random_sites_scatter.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

# Shannon Richness - CRP % All Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = richness, color = site_type)) +
  geom_point()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP % in Local Watersheds- All Sampling Sites", color = "Site Type")

ggsave("richness_all_sites_scatter.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

# Shannon Richness - CRP % All Sites
fish_df$crep_category <- cut(fish_df$w_crepcrp_percent, 
                             breaks = c(-Inf, 0.01, 0.10, 0.20, 0.30, Inf),
                             labels = c("<1%","1-10%","10-20%","20-30%", ">30%"))
fish_df %>% 
  ggplot2::ggplot(aes(x= crep_category, y = richness)) +
  geom_boxplot()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP in Local Watersheds")

ggsave("richness_all_sites_boxplot_by_CRPpercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

## HEL Soils %  vs DO Tolerance
fish_df %>% 
  ggplot2::ggplot(aes(x= w_hel_percent, y = dotolind, color = crep_site_type)) +
  geom_point()+
  labs(x = "HEL Percent", y = "DO Tolerance", title = "HEL % vs Average DO tolerance- All Sampling Sites", color = "Site Type")

ggsave("hel_do_all_sites.pdf", width = 8, height = 8, path = analysis_folder, units = "in")

fish_df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_hel_percent, y = dotolind, color = c_order)) +
  geom_point()+
  labs(x = "HEL Percent", y = "DO Tolerance", title = "HEL % vs Average DO tolerance- Random Sites", color = "Order")+
  geom_smooth(method="lm")

fish_df_random <- fish_df %>% 
  filter(crep_site_type == "random")
cor.test(fish_df_random$w_hel_percent, fish_df_random$dotolind, method = "pearson", conf.level = 0.95)

df %>% 
  filter(crep_site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_slope, y = dotoltax, color = c_order)) +
  geom_point()+
  labs(x = "Slope", y = "DO Tolerance", title = "Slope vs Average DO tolerance- All Sampling Sites", color = "Order")+
  geom_smooth(method="lm")

df_random <- df %>% 
  filter(crep_site_type == "random")
cor.test(df_random$w_hel_percent, df_random$dotolind, method = "pearson", conf.level = 0.95)


# ____
random <- df %>% 
  filter(crep_site_type == "random")
