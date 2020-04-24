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

#### Richness ####

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
  labs(x = "CREP/CRP Percent", y = "Shannon Richness", title = "Shannon Richness vs CREP % in Local Watersheds- Random Sites",
       caption = "Scatterplot ")

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

#### Intolerant Species ####

# Sensitive Species Number of Taxa- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolntax, color = pair_class)) +
  geom_boxplot()+
  labs(x = "CRP Level", y = "Number of Sensitive Species", title = "Number of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species Percentage- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolptax, color = pair_class)) +
  geom_boxplot()+ 
  labs(x = "CRP Level", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species Percentage- Paired high low Violin 
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = intolptax, color = pair_class)) +
  geom_violin()+
  labs(x = "CRP Level", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_paired_sites_violinplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species by CRP/CREP Percent in Paired Sites
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = pair_class)) +
  geom_point()+
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent in Paired Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_paired_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent- Random Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_random_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent- All Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_all_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Sensitive Species - Random Crep %
fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_groupedbypercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

## Recategorized Groups
fish_df$crep_category_large <- cut(fish_df$w_crepcrp_percent, 
                                   breaks = c(-Inf, 0, 0.01, 0.20, 0.40, Inf),
                                   labels = c("0","0-1%","1-20%","20-40%", ">40%"))


fish_df$crep_category_larger <- cut(fish_df$w_crepcrp_percent, 
                                   breaks = c(-Inf, 0.20, 0.40, Inf),
                                   labels = c("0.1-20%","20-40%", ">40%"))

fish_df$crep_category_small <- cut(fish_df$w_crepcrp_percent, 
                                    breaks = c(-Inf, 0.05, 0.20, 0.40, Inf),
                                    labels = c("0-5%","5-20%","20-40%", ">40%"))

## Sensitive Species- Random CREP % large binned categories
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category_large, fill= crep_category_large)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Spp.", title = "Percent of Sensitive Species vs. CRP Percent Sites- Random Sites", color = "CRP Level")

fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0.001) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category_larger, fill= crep_category_larger)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Species", title = "Percent of Sensitive Species vs. CRP Percent Sites- Random Sites", fill = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_randomsites_largebins.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

intol.mod1 = lm(intolptax ~ crep_category_larger, data = fish_df)
summary(intol.mod1)
anova(intol.mod1)

fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category_small, fill= crep_category_small)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Species", title = "Percent of Sensitive Species vs. CRP Percent Sites- Random Sites", fill = "CRP Level")

ggsave("sensitive_taxa_percentage_by_CRPpercent_randomsites_smallerbins.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

intol.mod2 = lm(intolptax ~ crep_category_small, data = fish_df)
summary(intol.mod2)
anova(intol.mod2)
confint(intol.mod2)

## Histogram of Sensitive Species #
fish_df %>% 
  filter(site_type == "random", intolptax > 0) %>% 
  ggplot2::ggplot(aes(x = intolptax, fill = crep_category)) +
  geom_histogram()+ 
  labs(x = "Sensitive Species Percent", y = "Count", title = "Percent of Sensitive Species Histogram (0 Sensitive Species Removed)", fill = "CRP/CREP Level")

#### Moderate/Intermediate Tolerance Species ####



# Moderately Sensitive Species Number of Taxa- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = modtolntax, color = pair_class)) +
  geom_boxplot()+
  labs(x = "CRP Level", y = "Number of Moderately Sensitive Species", title = "Number of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")


# Moderately Sensitive Species Percentage- Paired high low boxplot
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = modtolptax, color = pair_class)) +
  geom_boxplot()+ 
  labs(x = "CRP Level", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species Percentage- Paired high low Violin 
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= pair_class, y = modtolptax, color = pair_class)) +
  geom_violin()+
  labs(x = "CRP Level", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species in High and Low CREP Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_paired_sites_violinplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species by CRP/CREP Percent in Paired Sites
fish_df %>% 
  filter(site_type == "paired") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = modtolptax, color = pair_class)) +
  geom_point()+
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent in Paired Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_paired_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Moderately Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  filter(site_type == "random") %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = modtolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent- Random Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_random_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

#Moderately Sensitive Species by CRP/CREP Percent in Random Sites
fish_df %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = modtolptax, color = crep_category)) +
  geom_point()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent- All Sites", color = "CRP Level")

ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_all_sites.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

# Moderately Sensitive Species - Random Crep %
fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = modtolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  geom_jitter()
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")

  ggsave("intermediate_toleranace_taxa_percentage_by_CRPpercent_groupedbypercent.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")

## Histogram of Moderately Sensitive Species #
fish_df %>% 
  filter(site_type == "random", modtolptax > 0) %>% 
  ggplot2::ggplot(aes(x = modtolptax, fill = crep_category)) +
  geom_histogram()+ 
  labs(x = "Moderately Sensitive Species Percent", y = "Count", title = "Percent of Moderately Sensitive Species Histogram (0 Sensitive Species Removed)", fill = "CRP/CREP Level")


##### TODO ####
fish_df %>% 
  filter(w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = modtolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  geom_jitter()+
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")+
  geom_text(label = length(crep_category))

# test <-fish_df %>% 
#   filter(site_type == "random", w_crepcrp_percent > 0) %>% 
#   group_by(crep_category) %>% 
#   summarise(
#     mean_sensitive = mean(sensptax),
#     mean_moderate = mean(modtolptax),
#     count = n()
#             )
# test

fish_df %>% 
  filter(w_crepcrp_percent > 0) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category, fill= crep_category)) +
  geom_boxplot()+ 
  geom_jitter()+
  labs(x = "CRP/CREP Percent", y = "Percent Moderately Sensitive Spp.", title = "Percent of Moderately Sensitive Species vs. CRP Percent Sites- Paried Sites", color = "CRP Level")+
  geom_text(label = length(crep_category))


#TODO combine CREP categories to look at sensitive spp. in  CRP/CREP % 0, 0-1, 1-20, 20-40 >40. There should be no values >40 but check anyway. 

##### PLOTS for YONG IDNR flier ####
fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0.001) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category_larger, fill= crep_category_larger)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Species", title = "Percent of Sensitive Species vs. CRP/CREP Percent in Local Watershed", fill = "CRP/CREP Level")+
  

ggsave("sensitive_taxa_percentage_by_CRPCREPpercent_largebins.tif", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")


fish_df %>% 
  filter(site_type == "random", w_crepcrp_percent > 0.001) %>% 
  ggplot2::ggplot(aes(x= w_crepcrp_percent, y = intolptax, group = crep_category_larger, fill= crep_category_larger)) +
  geom_boxplot()+ 
  labs(x = "CRP/CREP Percent", y = "Percent Sensitive Species", fill = "CRP/CREP Level", 
       caption = "Percent of species sensitive to sedimentation at stream sites with 
low (red) CRP/CREP Area and high (blue) CRP/CREP Area in 
       local watersheds.")+
  theme(text = element_text( size = 20),
        axis.title = element_text(size = 20),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))

ggsave("sensitive_taxa_percentage_by_CRPCREPpercent_largebins_revised.png", width = 10, height = 10, path = paste0(network_prefix, analysis_folder), units = "in")


fish %>% 
  # filter(site_type == "random", w_crepcrp_percent > 0.001) %>% 
  ggplot2::ggplot(aes(x= c_order, group = data_source, fill= data_source)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5)+ 
  labs(x = "Stream Order", y = "Count", title = "Kaskaskia Basin Fish Community Sampling Locations", fill = "Survey Type")+
  scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
  scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))

ggsave("Kaskaskia_surveys_bystream_order.tif", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")


######