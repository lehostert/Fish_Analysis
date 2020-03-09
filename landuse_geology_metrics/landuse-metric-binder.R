library(tidyverse)

network_prefix <- "//INHS-Bison" #Lauren's Desktop PC
# network_prefix <- "/Volumes" #Lauren's Mac Laptop

WT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv"))
W <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED.csv"))
RT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN_TOTAL.csv"))
R <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN.csv"))
C <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_CHANNEL.csv"))
k <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PU_Gaps_size_and_CRP_classes.csv"))
hel <- read_csv(file = "~/ArcGIS/Projects/CREP/HEL_Soils_KaskLocalCatchment.csv")
kasky <- read_csv(file = "~/ArcGIS/Projects/CREP/KaskLocalCatchments_w_Area.csv")


#bring together all of the metrics from the different layers so that they match the Can Journal of Aq Sci then loop it like the Loop demo

k <- k %>% select(c(pu_gap_code, basin, prop_local_CRP)) %>% distinct()

kasky <- kasky %>% 
  select(PUGAP_CODE, CRP_Area, CREP_Area, Total_Area, Area_Km2)
kasky$CRPCREP_Area <- kasky$CREP_Area + kasky$CRP_Area
kasky$W_CREPCRP_Percent <- kasky$CRPCREP_Area/kasky$Total_Area

hel <- hel %>%
  select(PUGAP_CODE, Erodibility_Area)

kasky <- full_join(kasky, hel)
kasky$W_HEL_Percent <- kasky$Erodibility_Area/kasky$Area_Km2

kasky_clean <- kasky %>% 
  filter(str_detect(kasky$PUGAP_CODE, "kasky")) %>% 
  group_by(PUGAP_CODE) %>% 
  summarise_all(sum) %>% 
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

# envi <- envi %>%  rename(Order = C_ORDER,	Downorder = DORDER,	WT_Area = WT_TOTAL_SQME, WT_JulMin = WT_JUL_MNX,	WT_Precip = WT_PREC,
#                          WT_Carb = WT_BR_carbonate,	WT_Sand = WT_BR_sandstone,	WT_Shale = WT_BR_shale,	WT_Alluv = WT_Alluvium_fluvial,	
#                          WT_CMorai = WT_Coarse_moraine,	WT_Coars = WT_Coarse,	WT_Collu = WT_Colluvium,
#                          WT_Lacus = WT_Lacustrine,	WT_Loess = WT_Loess,	WT_MMora = WT_Medium_moraine, WT_PMuck = WT_Peat_muck,	
#                          WT_ICont = WT_Icecontact,	W_Darcy = W_DARCYX,	WT_Darcy = WT_DARCYX,	W_Perm = W_PERMX,	WT_Perm = WT_PERMX,	R_Water = R_Open_wet,	
#                          RT_Grass = RT_Grassland,	W_Forest = W_Forest_total,	W_Agril = W_Agriculture,	W_Grass = W_Grassland,
#                          W_Water = W_Open_wet,	W_Wet = W_Wetland_total,	WT_Forest = WT_Forest_total, WT_Grass = WT_Grassland,	
#                          WT_Agril = WT_Agriculture,	Bigriver = BIGRIVER,	Damdn_L = DAMDWL,	Damdnst = DAMDW,	Damup_L = DAMUPL,	Damupst = DAMUP,
#                          Missi = MISSI,	Pond = POND,	Pondarea = POND_AREA,	Ponddn_L = PONDWL,	Ponddn_S = PONDWA,	Ponddnst = PONDDW,	Pondup_L = PONDUPL,
#                          Pondupst = PONDDW,	Sinuosity = SINUOUS,	W_Area = W_TOTAL_SQM,	W_Slope = W_SLOPE, WT_Slope = WT_SLOPE,	
#                          Chan-Grad = GRADIENT,	CRP = prop_local_CRP
#                          )
# 
# envi <- envi %>%  rename(Pondup_S = PONDUPA,	Pondupst = PONDDW,	Sinuosity = SINUOUS,	W_Area = W_TOTAL_SQM,	W_Slope = W_SLOPE, WT_Slope = WT_SLOPE,	
#                          Chan-Grad = GRADIENT,	CRP = prop_local_CRP
#                          )
# envi <- envi %>% 
#   select(PONDUPA)
# 
# envi <- envi %>%  rename(Order = C_ORDER,	Downorder = DORDER,	WT_Area = WT_TOTAL_SQME,	WT_GDD = WT_GDD,	WT_JulMin = WT_JUL_MNX,	WT_Precip = WT_PREC,	
#                          C_BR50 = C_BR50,	C_BR100 = C_BR100,	C_BRG100 = C_BRG100,	WT_BR50 = WT_BR50,	WT_BR100 = WT_BR100,	WT_BRG100 = WT_BRG100,	
#                          WT_Carb = WT_BR_carbonate,	WT_Sand = WT_BR_sandstone,	WT_Shale = WT_BR_shale,	WT_Rocky = WT_Rocky,	WT_Alluv = WT_Alluvium_fluvial,	
#                          WT_CMorai = WT_Coarse_moraine,	WT_Coars = WT_Coarse,	WT_Collu = WT_Colluvium,	WT_Dune = WT_Dune,	WT_Fine = WT_Fines,	
#                          WT_Lacus = WT_Lacustrine,	WT_Loess = WT_Loess,	WT_MMora = WT_Medium_moraine,	WT_OWash = WT_Outwash,	WT_PMuck = WT_Peat_muck,	
#                          WT_ICont = WT_Icecontact,	W_Darcy = W_DARCYX,	WT_Darcy = WT_DARCYX,	W_Perm = W_PERMX,	WT_Perm = WT_PERMX,	R_Water = R_Open_wet,	
#                          RT_Grass = RT_Grassland,	W_Forest = W_Forest_total,	W_Agril = W_Agriculture,	W_Grass = W_Grassland,	W_Urban = W_Urban,	
#                          W_Water = W_Open_wet,	W_Wet = W_Wetland_total,	WT_Forest = WT_Forest_total,	WT_Urban = WT_Urban,	WT_Grass = WT_Grassland,	
#                          WT_Agril = WT_Agriculture,	Bigriver = BIGRIVER,	Damdn_L = DAMDWL,	Damdnst = DAMDW,	Damup_L = DAMUPL,	Damupst = DAMUP,	
#                          Missi = MISSI,	Pond = POND,	Pondarea = POND_AREA,	Ponddn_L = PONDWL,	Ponddn_S = PONDWA,	Ponddnst = PONDDW,	Pondup_L = PONDUPL,	
#                          Pondup_S = PONDUPA,	Pondupst = PONDDW,	Sinuosity = SINUOUS,	W_Area = W_TOTAL_SQM,	W_Slope = W_SLOPE, WT_Slope = WT_SLOPE,	
#                          Chan-Grad = GRADIENT,	CRP = prop_local_CRP
# )
# 
# 
# 
# envi$order <- C$C_ORDER
# envi$downorder <- C$DORDER
# envi$WT_Area <- WT$WT_Area
# envi$WT_GDD <- WT$WT_GDD
# envi$WT_JMin <- WT$WT_JUL_MNX
# envi$WT_Precip <- WT$WT_PREC
# envi$C_BR50 <- C$C_BR50
# envi$C_BR100 <- C$C_BR100
# envi$C_BRG100 <- C$C_BRG100
# envi$WT_BR50 <- WT$WT_BR50
# envi$WT_BR100 <- WT$WT_BRG100
# envi$WT_BRG100 <- WT$WT_BRG100
# envi$WT_Carb <- WT$WT_BR_carbonate
# envi$WT_Sand <- WT$WT_BR_sandstone
# envi$WT_Shale <- WT$WT_BR_shale
# envi$WT_Rocky <- WT$WT_Rocky
# envi$WT_Alluv <- WT$WT_Alluvium_fluvial
# envi$WT_CMorai <- WT$WT_Coarse_moraine
# envi$WT_Coars <- WT$WT_Coarse
# envi$WT_Collu <- WT$WT_Colluvium
# envi$WT_Dune <- WT$WT_Dune
# envi$WT_Fine <- WT$WT_Fines
# envi$WT_Lacus <- WT$WT_Lacustrine
# envi$WT_Loess <- WT$WT_Loess
# envi$WT_MMora <- WT$WT_Medium_moraine
# envi$WT_OWash <- WT$WT_Outwash
# envi$WT_PMuck <- WT$WT_Peat_muck
# envi$WT_ICont <- WT$WT_Icecontact
# envi$W_Darcy <- W$W_DARCYX
# envi$WT_Darcy <- WT$WT_DARCYX
# envi$W_Perm <- W$W_PERMX
# envi$WT_Perm <- WT$WT_PERMX
# envi$R_Water <- R$R_Open_wet
# envi$RT_Grass <- RT$RT_Grassland
# envi$W_Forest <- W$W_Forest_total
# envi$W_Agril <- W$W_Agriculture
# envi$W_Grass <- W$W_Grassland
# envi$W_Urban <- W$W_Urban
# envi$W_Water <- W$W_Open_wet
# envi$W_Wet <- W$W_Wetland_total
# envi$WT_Forest <- WT$WT_Forest_total
# envi$WT_Urban <- WT$WT_Urban
# envi$WT_Grass <- WT$WT_Grassland
# envi$WT_Agril <- WT$WT_Agriculture
# envi$Bigriver <- C$BIGRIVER
# envi$Damdn_L <- C$DAMDWL
# envi$Damdnst <- C$DAMDW
# envi$Damup_L <- C$DAMUPL
# envi$Damupst <- C$DAMUP
# envi$Missi <- C$MISSI
# envi$Pond <- C$POND
# envi$Pondarea <- C$POND_AREA
# envi$Ponddn_L <- C$PONDDWL
# envi$Ponddn_S <- C$PONDDWA
# envi$Ponddnst <- C$PONDDW
# envi$Pondup_L <- C$PONDUPL
# envi$Pondup_S <- C$PONDUPA
# envi$Pondupst <- C$PONDDW
# envi$Sinuosity <- C$SINUOUS
# envi$W_Area <- W$W_TOTAL_SQM
# envi$W_Slope <- W$W_SLOPE
# envi$WT_Slope <- WT$WT_SLOPE
# envi$Chan-Grad <- C$GRADIENT
# envi$CRP <- k$prop_local_CRP


write_csv(envi, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics.csv"))