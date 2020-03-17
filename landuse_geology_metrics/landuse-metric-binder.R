library(tidyverse)

network_prefix <- "//INHS-Bison" #Lauren's Desktop PC
# network_prefix <- "/Volumes" #Lauren's Mac Laptop

WT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv"))
W <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED.csv"))
RT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN_TOTAL.csv"))
R <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN.csv"))
C <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_CHANNEL.csv"))
# k <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PU_Gaps_size_and_CRP_classes.csv"))
hel <- read_csv(file = "~/ArcGIS/Projects/CREP/HEL_Soils_KaskLocalCatchment.csv")
kasky_Area <- read_csv(file = "~/ArcGIS/Projects/CREP/KaskLocalCatchments_w_Area.csv")
CRP <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Kasky_CRP_Area.csv"))
CREP <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Kasky_CREP_Area.csv"))


#bring together all of the metrics from the different layers so that they match the Can Journal of Aq Sci then loop it like the Loop demo

# k <- k %>% select(c(pu_gap_code, basin, prop_local_CRP)) %>% distinct()

hel <- hel %>%
  select(PUGAP_CODE, Erodibility_Area)

CRP_k <- CRP %>% 
  filter(str_detect(CRP$PUGAP_CODE, "kasky")) %>% 
  select(PUGAP_CODE, CRP_Area)

CREP_k <- CREP %>% 
  filter(str_detect(CREP$PUGAP_CODE, "kasky")) %>% 
  select(PUGAP_CODE, CREP_Area)

CREP_CRP <- CRP_k %>% 
  full_join(CREP_k) %>% 
  replace_na(list(CREP_Area = 0, CRP_Area = 0)) %>%
  mutate(CREP_CRP_sum = rowSums(.[2:3]))

kasky <- kasky_Area %>% 
  filter(str_detect(kasky_Area$PUGAP_CODE, "kasky")) %>% 
  select(PUGAP_CODE,Total_Area, Area_Km2)%>% 
  group_by(PUGAP_CODE) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  full_join(CREP_CRP) %>% 
  replace_na(list(CREP_Area = 0, CRP_Area = 0, CREP_CRP_sum = 0))

kasky <- kasky %>% 
  full_join(hel) %>% 
  filter(str_detect(PUGAP_CODE, "kasky")) %>% 
  replace_na(list(Erodibility_Area = 0))

kasky <- mutate(kasky, W_CREPCRP_Percent = CREP_CRP_sum/Area_Km2)
kasky <- mutate(kasky, W_HEL_Percent = Erodibility_Area/Area_Km2)

kasky_clean <- kasky %>%
  select(PUGAP_CODE, W_CREPCRP_Percent, W_HEL_Percent)


a1 <- full_join(WT,W)
a2 <- full_join(R,RT)
a3 <- full_join(a1, a2)
a4 <- full_join(a3, C)
all <- left_join(a4, kasky_clean, by =c("PU_Gap_Code" = "PUGAP_CODE"))


envi <- all %>% 
  select(PU_Gap_Code, LINK,	DLINK,	C_ORDER,	DORDER,	WT_TOTAL_SQME,	
         WT_GDD,	WT_JUL_MNX,	WT_PREC,	
         C_BR50,	C_BR100,	C_BRG100,	WT_BR50,	WT_BR100,	WT_BRG100,	
         WT_BR_carbonate,	WT_BR_sandstone,	WT_BR_shale,	WT_Rocky,	WT_Alluvium_fluvial,	WT_Coarse_moraine,	WT_Coarse,	WT_Colluvium,	WT_Dune,	
         WT_Fines,	WT_Lacustrine,	WT_Loess,	WT_Medium_moraine,	WT_Outwash,	WT_Peat_muck,	WT_Icecontact,
         W_DARCYX,	WT_DARCYX,	W_PERMX,	WT_PERMX,	
         R_Open_wet,	RT_Grassland,	W_Forest_total,	W_Agriculture,	W_Grassland,	W_Urban,	W_Open_wet,	W_Wetland_total,	WT_Forest_total,	WT_Urban,	WT_Grassland,	WT_Agriculture,	
         BIGRIVER,	DAMDWL,	DAMDW,	DAMUPL,	DAMUP,	MISSI,	POND,	POND_AREA,	PONDWL,	PONDWA,	PONDDW,	PONDUPL,	PONDUPA,	PONDUP,	
         SINUOUS,	W_TOTAL_SQM,	W_SLOPE,	WT_SLOPE,	GRADIENT,	W_CREPCRP_Percent, W_HEL_Percent
         )


write_csv(envi, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics_revised.csv"))