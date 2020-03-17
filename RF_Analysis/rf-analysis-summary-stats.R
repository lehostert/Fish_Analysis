library(tidyverse)
library(viridis)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

fish_envi.dat <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv"))

# rural_metrics_envi.dat <- metrics_envi.dat %>% 
#   filter(w_urban <0.02)

#### Freqeuncy Distrubutions ####

summary(fish_envi.dat)

pdf(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Plots/Landscape_frequency_plots.pdf"))

#RT_Grassland
ggplot2::ggplot(fish_envi.dat, aes(x = rt_grassland)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("RT_Grassland"), y = "Count")

#W_Grassland
ggplot2::ggplot(fish_envi.dat, aes(x = w_grassland)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("W_Grassland"), y = "Count")

#WT_Grassland
ggplot2::ggplot(fish_envi.dat, aes(x = wt_grassland)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("WT_Grassland"), y = "Count")

#W_forest_total
ggplot2::ggplot(fish_envi.dat, aes(x = w_forest_total)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("w_forest_total"), y = "Count")

#WT_forest_total
ggplot2::ggplot(fish_envi.dat, aes(x = wt_forest_total)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("wt_forest_total"), y = "Count")

#W_Urban
ggplot2::ggplot(fish_envi.dat, aes(x = w_urban)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("W_Urban"), y = "Count")

#WT_Urban
ggplot2::ggplot(fish_envi.dat, aes(x = wt_urban)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("WT_Urban"), y = "Count")

#W_agriculture
ggplot2::ggplot(fish_envi.dat, aes(x = w_agriculture)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("W_agriculture"), y = "Count")

#WT_agriculture
ggplot2::ggplot(fish_envi.dat, aes(x = wt_agriculture)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("WT_agriculture"), y = "Count")

#W_crepcrp_percent
ggplot2::ggplot(fish_envi.dat, aes(x = w_crepcrp_percent)) +
  geom_histogram(binwidth = 0.05) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("W_CREP_percent"), y = "Count")

#W_hel_percent
ggplot2::ggplot(fish_envi.dat, aes(x = w_hel_percent)) +
  geom_histogram(binwidth = 0.05) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("W_hel_percent"), y = "Count")

dev.off() 

#### Fish Abundance Metrics ####
fish_envi.dat <- metrics_envi.dat
fdf <- fish_envi.dat
fdf <- tibble::rownames_to_column(fdf, "Site_ID")

fdf$event_date <- as.Date(fdf$event_date)
fdf <- fdf %>% 
  mutate(rich_link = richness/link)

pdf(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Plots/summary-plots.pdf"))

#richness
ggplot2::ggplot(fdf, aes(x = event_date, y = richness)) +
  geom_point()+
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("Spp. Richness"))

#richness adjusted for by link
ggplot2::ggplot(fdf, aes(x = event_date, y = rich_link)) +
  geom_point()+
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("Richness adj by link"))

crep <- fdf %>% 
  filter(data_source == "crep_monitoring")

#richness adjusted for by link Violin with jitter
ggplot2::ggplot(crep, aes(x = factor(lubridate::year(event_date)), y = rich_link)) +
  geom_violin() +
  geom_jitter(aes(color = w_hel_percent))+
  scale_color_viridis(option = "D") +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("Adjusted Richness by Year- jittered"), x = "Sampling Year", y = "Adjusted Richness")

#richness by link Violin with jitter
ggplot2::ggplot(crep, aes(x = factor(lubridate::year(event_date)), y = richness)) +
  geom_violin() +
  geom_jitter(aes(color = w_hel_percent))+
  scale_color_viridis(option = "B") +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("Richness by Year- Jittered"), x = "Sampling Year", y = "Richness")

dev.off()