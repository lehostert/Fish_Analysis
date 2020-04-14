library(tidyverse)


## The path for mapping network drives like Bison are not the same in Windows & MacOS so define the network_prefix based on which computer you are using. 
network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

## Analysis folder is the fold for saving _this_ particular run
analysis_folder <- "/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/idnr-summary-plots"

#load data 
fish <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"))
paired_crep <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PairedSites_Attributes_LEH.csv"))
sites_list <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Site_List_2013-2019.csv"))

names(paired_crep) <- stringr::str_to_lower(names(paired_crep))
names(sites_list) <- stringr::str_to_lower(names(sites_list))

## clean up down paired site

paired_list <- paired_crep %>% 
  rename(pair_class = crp_class, pair_no = pair) %>% 
  select(pu_gap_code, pair_no, pair_class) %>% 
  drop_na()

# Combine data frames
fish$reach_name <- str_replace_all(fish$reach_name, " ", "")

fish_df <- fish %>%
  filter(data_source == "crep_monitoring") %>% 
  left_join(sites_list) %>% 
  select(-stream_name) %>% 
  left_join(paired_list) 

fish_df$pair_class<- ifelse(fish_df$site_type != "paired", NA, fish_df$pair_class)
fish_df$pair_no<- ifelse(fish_df$site_type != "paired", NA, fish_df$pair_no)

# test <- fish_df %>% 
#   select(site_id, pu_gap_code, reach_name, event_date, site_type, pair_class, pair_no)

# summary(fish)

#### Plots ####
theme_update(plot.title = element_text(hjust = 0.5))

### Richness

# Shannon Richness - Paired high low
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = richness, color = pair_class)) +
  geom_boxplot()+
  labs(x = "CRP Level", y = "Shannon Richness", title = "Shannon Richness in High and Low CREP Sites", color = "CRP Level")

ggsave("richness_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Shannon Richness - CRP % Random Sites
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = richness)) +
  geom_point()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP % in Local Watersheds- Random Sites")

ggsave("richness_random_sites_scatter.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Shannon Richness - CRP % All Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = richness, color = site_type)) +
  geom_point()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP % in Local Watersheds- All Sampling Sites", color = "Site Type")

ggsave("richness_all_sites_scatter.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Shannon Richness - CRP % All Sites
fish_df$crep_category <- cut(fish_df$w_crepcrp_percent, 
                             breaks = c(-Inf, 0.01, 0.10, 0.20, 0.30, Inf),
                             labels = c("<1%","1-10%","10-20%","20-30%", ">30%"))
fish_df %>% 
  ggplot2::ggplot(aes(x= crep_category, y = richness)) +
  geom_boxplot()+
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP in Local Watersheds")

ggsave("richness_all_sites_boxplot_by_CRPpercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

### Intolerant Species

# Sensitive Species Number of Taxa- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = sensntax, color = pair_class)) +
  geom_boxplot()+
  labs(x = "CRP Level", y = "Number of Sensitive Species", title = "Number of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species Percentage- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = sensptax, color = pair_class)) +
  geom_boxplot()+ 
  labs(x = "CRP Level", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species Percentage- Paired high low Violin 
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = sensptax, color = pair_class)) +
  geom_violin()+
  labs(x = "CRP Level", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_paired_sites_violinplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species by CRP/CREP Percent in Paired Sites
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = sensptax, color = pair_class)) +
  geom_point()+
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent in Paired Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_paired_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = sensptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent- Random Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_random_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = sensptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent- All Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_all_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# tSensitive Species - Random Crep %
fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = sensptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_groupedbypercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

## Histogram of Sensitive Species #
fish_df %>% 
  filter(site_type == "random", sensptax > 0) %>% 
  ggplot2::ggplot(aes(x = sensptax, fill = crep_category)) +
  geom_histogram()+ 
  labs(x = "Sensitive Species Percent", y = "Count", title = "Percent of Sensitive Species Histogram (0 Sensitive Species Removed)", fill = "CRP/CREP Level")

#### Moderate/Intermediate Tolerance Species ####

# Moderately Sensitive Species Number of Taxa- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolntax, color = pair_class)) +
  geom_boxplot()+
  labs(x = "CRP Level", y = "Number of Moderately Sensitive Species", title = "Number of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species Percentage- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolptax, color = pair_class)) +
  geom_boxplot()+ 
  labs(x = "CRP Level", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species Percentage- Paired high low Violin 
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolptax, color = pair_class)) +
  geom_violin()+
  labs(x = "CRP Level", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_paired_sites_violinplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species by CRP/CREP Percent in Paired Sites
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = pair_class)) +
  geom_point()+
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent in Paired Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_paired_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Moderately Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent- Random Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_random_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Moderately Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent- All Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_all_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species - Random Crep %
fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  geom_jitter()
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")

  fish_df %>% 
    filter(w_crepcrp_percent > 0) %>% 
    ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category, fill= crep_category)) +
    geom_boxplot()+ 
    geom_jitter()+
    labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")+
    geom_text(label = length(crep_category))
  
ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_groupedbypercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

## Histogram of Moderately Sensitive Species #
fish_df %>% 
  filter(site_type == "random", intolptax > 0) %>% 
  ggplot2::ggplot(aes(x = intolptax, fill = crep_category)) +
  geom_histogram()+ 
  labs(x = "Moderately Sensitive Species Percent", y = "Count", title = "Percent of Moderately Sensitive Species Histogram (0 Sensitive Species Removed)", fill = "CRP/CREP Level")


##### TODO ####
fish_df %>% 
  filter(w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  geom_jitter()+
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")+
  geom_text(label = length(crep_category))

test <-fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0) %>% 
  group_by(crep_category) %>% 
  summarise(
    mean_sensitive = mean(sensptax),
    mean_moderate = mean(intolptax),
    count = n()
            )
test


#TODO combine CREP categories to look at sensitive spp. in  CRP/CREP % 0, 0-1, 1-20, 20-40 >40. There should be no values >40 but check anyway. 
