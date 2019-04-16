library(tidyverse)

Gap <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_WATERSHED_TOTAL_minus_ny.xlsx", 
                          sheet = 2, col_types = c("text","text","text","text", "text",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric"
                                                   
                          ))

Gap_group_WT <- Gap %>% 
  filter(PU_CODE == 'kasky')

colnames(Gap_group_WT)[colnames(Gap_group_WT)=="PU_CODE"] <- "PU_Code"
colnames(Gap_group_WT)[colnames(Gap_group_WT)=="GAP_CODE"] <- "Gap_Code"
colnames(Gap_group_WT)[colnames(Gap_group_WT)=="PU_GAP"] <- "PU_Gap_Code"

Gap_group_WT$WT_Urban <- Gap_group_WT$WT_LU11P + Gap_group_WT$WT_LU12P + Gap_group_WT$WT_LU13P + Gap_group_WT$WT_LU14P
Gap_group_WT$WT_Agriculture <- Gap_group_WT$WT_LU21P + Gap_group_WT$WT_LU22P + Gap_group_WT$WT_LU23P
Gap_group_WT$WT_Grassland <- Gap_group_WT$WT_LU30P
#Forested Total = Forested_wetland + Forested_upland (Forest + All wooded wetland including shrubland)
Gap_group_WT$WT_Forest_total <- Gap_group_WT$WT_LU41P + Gap_group_WT$WT_LU42P + Gap_group_WT$WT_LU43P + Gap_group_WT$WT_LU61P + Gap_group_WT$WT_LU610P + Gap_group_WT$WT_LU611P + Gap_group_WT$WT_LU612P + Gap_group_WT$WT_LU613P
Gap_group_WT$WT_Forested_upland <- Gap_group_WT$WT_LU41P + Gap_group_WT$WT_LU42P + Gap_group_WT$WT_LU43P
# Forested_wetland is all wooded wetland including WT_LU610P (Wooded Wetland, shrubland)
Gap_group_WT$WT_Forested_wetland <- Gap_group_WT$WT_LU61P + Gap_group_WT$WT_LU610P + Gap_group_WT$WT_LU611P + Gap_group_WT$WT_LU612P + Gap_group_WT$WT_LU613P
Gap_group_WT$WT_Inland_water <- Gap_group_WT$WT_LU50P
# Open wet in SSA is calculated  as inland water/open water + non-wooded wetland (Wetland-emergent herbaceous)
# In GAP we use  Open Water + Wetland, non-wooded 
Gap_group_WT$WT_Open_wet <- Gap_group_WT$WT_LU50P + Gap_group_WT$WT_LU62P
Gap_group_WT$WT_Wetland_total <- Gap_group_WT$WT_LU61P + Gap_group_WT$WT_LU610P + Gap_group_WT$WT_LU611P + Gap_group_WT$WT_LU612P + Gap_group_WT$WT_LU613P + Gap_group_WT$WT_LU62P
Gap_group_WT$WT_Barren <- Gap_group_WT$WT_LU70P
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_WT$WT_Alluvium_fluvial <- Gap_group_WT$WT_QG12P + Gap_group_WT$WT_QG19P + Gap_group_WT$WT_QG23P
Gap_group_WT$WT_Attenuated_drift <- Gap_group_WT$WT_QG14P + Gap_group_WT$WT_QG22P
Gap_group_WT$WT_Bedrock <- Gap_group_WT$WT_QG99P
Gap_group_WT$WT_Broken_rocky <- Gap_group_WT$WT_QG11P + Gap_group_WT$WT_QG14P + Gap_group_WT$WT_QG22P
# Coarse from SSA does not include Outwash (coarse) A3- WT_QG1P & Attenuated drift (coarse) J3- WT_QG14P 
# Attenuated drift (coarse) is all 0 for kasky
# Not in the SSA layers: Colluvium (coarse) F3- WT_QG32P and Stagnation moraine (coarse) I3- WT_QG13P all 0 for kasky
#### Remove WT_QG1P if you want identical values to Yong/Brians Group Data
Gap_group_WT$WT_Coarse <- Gap_group_WT$WT_QG1P + Gap_group_WT$WT_QG2P + Gap_group_WT$WT_QG5P + Gap_group_WT$WT_QG8P + Gap_group_WT$WT_QG10P + Gap_group_WT$WT_QG13P + 
  Gap_group_WT$WT_QG14P +Gap_group_WT$WT_QG17P + Gap_group_WT$WT_QG19P + Gap_group_WT$WT_QG29P + Gap_group_WT$WT_QG32P
Gap_group_WT$WT_Coarse_moraine <- Gap_group_WT$WT_QG5P + Gap_group_WT$WT_QG8P + Gap_group_WT$WT_QG13P
Gap_group_WT$WT_Colluvium <- Gap_group_WT$WT_QG11P
Gap_group_WT$WT_Dune <- Gap_group_WT$WT_QG29P
Gap_group_WT$WT_Fine_moraine <- Gap_group_WT$WT_QG3P + Gap_group_WT$WT_QG6P + Gap_group_WT$WT_QG30P
# Fine from SSA does not include Lacustrine clay and silt (fine) E1- WT_QG9P
#### Remove WT_QG9P if you want identical values to Yong/Brians Group Data
Gap_group_WT$WT_Fines <- Gap_group_WT$WT_QG3P +Gap_group_WT$WT_QG6P + Gap_group_WT$WT_QG9P + Gap_group_WT$WT_QG24P + Gap_group_WT$WT_QG30P
Gap_group_WT$WT_Icecontact <- Gap_group_WT$WT_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_WT$WT_Lacustrine <- Gap_group_WT$WT_QG9P + Gap_group_WT$WT_QG10P + Gap_group_WT$WT_QG31P
Gap_group_WT$WT_Loess <- Gap_group_WT$WT_QG24P
# Medium from SSA does not include Attenuated-drift (medium) J2- WT_QG22P
# Attenuated drift (medium) is all 0 for kasky
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_WT$WT_Medium <- Gap_group_WT$WT_QG4P + Gap_group_WT$WT_QG7P + Gap_group_WT$WT_QG16P + Gap_group_WT$WT_QG20P + Gap_group_WT$WT_QG22P + Gap_group_WT$WT_QG23P
Gap_group_WT$WT_Medium_moraine <- Gap_group_WT$WT_QG4P + Gap_group_WT$WT_QG7P + Gap_group_WT$WT_QG20P
Gap_group_WT$WT_Outwash <- Gap_group_WT$WT_QG1P
Gap_group_WT$WT_Outwash_icecontact <- Gap_group_WT$WT_QG1P + Gap_group_WT$WT_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_WT$WT_Peat_muck <- Gap_group_WT$WT_QG18P + Gap_group_WT$WT_QG31P
Gap_group_WT$WT_Rocky <- Gap_group_WT$WT_QG11P + Gap_group_WT$WT_QG14P + Gap_group_WT$WT_QG22P + Gap_group_WT$WT_QG99P
# Regroup Bed Rock Depth
Gap_group_WT$WT_BD50 <- Gap_group_WT$WT_BD201P
Gap_group_WT$WT_BD100 <- Gap_group_WT$WT_BD202P
Gap_group_WT$WT_BD200 <- Gap_group_WT$WT_BD203P
Gap_group_WT$WT_BD400 <- Gap_group_WT$WT_BD204P
# Regroup Bed Rock Type
Gap_group_WT$WT_BR_sandstone <- Gap_group_WT$WT_BR1P
Gap_group_WT$WT_BR_shale <- Gap_group_WT$WT_BR2P
Gap_group_WT$WT_BR_carbonate <- Gap_group_WT$WT_BR3P
Gap_group_WT$WT_BR_metamorphic <- Gap_group_WT$WT_BR41P
Gap_group_WT$WT_BR_igneous <- Gap_group_WT$WT_BR42P
Gap_group_WT$WT_BR_unknown <- Gap_group_WT$WT_BR5P
Gap_group_WT$WT_BR_water <- Gap_group_WT$WT_BR6P

Gap_group_WT <- Gap_group_WT %>%
  select(PU_Gap_Code, PU_Code, Gap_Code,
         WT_TOTAL_SQME, WT_PERMX, WT_SLOPE, WT_DARCYX, WT_LENGTH,
         WT_BR_sandstone, WT_BR_shale, WT_BR_carbonate, WT_BR_metamorphic, WT_BR_igneous, WT_BR_unknown, WT_BR_water,
         WT_BD50, WT_BD100, WT_BD200, WT_BD400,
         WT_Urban, WT_Agriculture, WT_Grassland, WT_Forest_total, WT_Forested_upland, WT_Forested_wetland, WT_Inland_water, WT_Open_wet, WT_Wetland_total, WT_Barren,
         WT_Alluvium_fluvial, WT_Attenuated_drift, WT_Bedrock, WT_Broken_rocky, WT_Coarse, WT_Coarse_moraine, WT_Colluvium, WT_Dune, WT_Fine_moraine, WT_Fines,
         WT_Icecontact, WT_Lacustrine, WT_Loess, WT_Medium, WT_Medium_moraine, WT_Outwash, WT_Outwash_icecontact, WT_Peat_muck, WT_Rocky)

write.csv(Gap_group_WT, file = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_WATERSHED_TOTAL.csv", row.names = F)
