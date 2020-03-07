library(tidyverse)

network_prefix <- "//INHS-Bison" #Lauren's Desktop PC
# network_prefix <- "/Volumes" #Lauren's Mac Laptop

WT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv"))
W <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED.csv"))
RT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN_TOTAL.csv"))
R <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN.csv"))
C <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_CHANNEL.csv"))
k <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PU_Gaps_size_and_CRP_classes.csv"))
CJ
envi <- C
#bring together all of the metrics from the different layers so that they match the Can Journal of Aq Sci then loop it like the Loop demo
  