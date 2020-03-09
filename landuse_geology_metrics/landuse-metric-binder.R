library(tidyverse)

network_prefix <- "//INHS-Bison" #Lauren's Desktop PC
# network_prefix <- "/Volumes" #Lauren's Mac Laptop

WT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv"))
W <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED.csv"))
RT <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN_TOTAL.csv"))
R <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN.csv"))
C <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_CHANNEL.csv"))
k <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/PU_Gaps_size_and_CRP_classes.csv"))

#bring together all of the metrics from the different layers so that they match the Can Journal of Aq Sci then loop it like the Loop demo

k <- k %>% select(c(pu_gap_code, basin, prop_local_CRP)) %>% distinct()
a1 <- full_join(WT,W)
a2 <- full_join(R,RT)
a3 <- full_join(a1, a2)
a4 <- full_join(a3, C)
all <- left_join(a4, k, by =c("PU_Gap_Code" = "pu_gap_code"))

unq1 <- unique(all$PU_Gap_Code)
unq2 <- unique(a4$PU_Gap_Code)
unq3 <- unique(k$pu_gap_code)

envi <- data.frame(NULL)
envi$link <- C$LINK
envi$dlink <- C$DLINK
envi$order <- C$C_ORDER
envi$downorder <- C$DORDER
envi$WT_Area <- WT$WT_Area
envi$WT_GDD <- WT$WT_GDD
envi$WT_JMin <- WT$WT_JUL_MNX
envi$WT_Precip <- WT$WT_PREC
envi$C_BR50 <- C$C_BR50
envi$C_BR100 <- C$C_BR100
envi$C_BRG100 <- C$C_BRG100
envi$WT_BR50 <- WT$WT_BR50
envi$WT_BR100 <- WT$WT_BRG100
envi$WT_BRG100 <- WT$WT_BRG100
envi$WT_Carb <- WT$WT_BR_carbonate
envi$WT_Sand <- WT$WT_BR_sandstone
envi$WT_Shale <- WT$WT_BR_shale
envi$WT_Rocky <- WT$WT_Rocky
envi$WT_Alluv <- WT$WT_Alluvium_fluvial
envi$WT_CMorai <- WT$WT_Coarse_moraine
envi$WT_Coars <- WT$WT_Coarse
envi$WT_Collu <- WT$WT_Colluvium
envi$WT_Dune <- WT$WT_Dune
envi$WT_Fine <- WT$WT_Fines
envi$WT_Lacus <- WT$WT_Lacustrine
envi$WT_Loess <- WT$WT_Loess
envi$WT_MMora <- WT$WT_Medium_moraine
envi$WT_OWash <- WT$WT_Outwash
envi$WT_PMuck <- WT$WT_Peat_muck
envi$WT_ICont <- WT$WT_Icecontact
envi$W_Darcy <- W$W_DARCYX
envi$WT_Darcy <- WT$WT_DARCYX
envi$W_Perm <- W$W_PERMX
envi$WT_Perm <- WT$WT_PERMX
envi$R_Water <- R$R_Open_wet
envi$RT_Grass <- RT$Grassland
envi$W_Forest <- W$W_Forest_total
envi$W_Agril <- W$W_Agriculture
envi$W_Grass <- W$W_Grassland
envi$W_Urban <- W$W_Urban
envi$W_Water <- W$W_Open_wet
envi$W_Wet <- W$W_Wetland_total
envi$WT_Forest <- WT$WT_Forest_total
envi$WT_Urban <- WT$WT_Urban
envi$WT_Grass <- WT$WT_Grassland
envi$WT_Agril <- WT$WT_Agriculture
envi$Bigriver <- C$BIGRIVER
envi$Damdn_L <- C$DAMDWL
envi$Damdnst <- C$DAMDW
envi$Damup_L <- C$DAMUPL
envi$Damupst <- C$DAMUP
envi$Missi <- C$MISSI
envi$Pond <- C$POND
envi$Pondarea <- C$POND_AREA
envi$Ponddn_L <- C$PONDDWL
envi$Ponddn_S <- C$PONDDWA
envi$Ponddnst <- C$PONDDW
envi$Pondup_L <- C$PONDUPL
envi$Pondup_S <- C$PONDUPA
envi$Pondupst <- C$PONDDW
envi$Sinuosity <- C$SINUOUS
envi$W_Area <- W$W_TOTAL_SQM
envi$W_Slope <- W$W_SLOPE
envi$WT_Slope <- WT$WT_SLOPE
envi$Chan-Grad <-C$GRADIENT
envi$CRP <- k$prop_local_CRP

write_csv(envi, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics.csv"))