library(tidyverse)

Gap <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_CHANNEL_minus_ny.xlsx", 
                          sheet = 2, col_types = c("text","text","text","text", "text",
                                                   "numeric","numeric","numeric", "numeric", "text","numeric","numeric","numeric","numeric", "numeric",
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
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "text"
                          ))

Gap_group_C <- Gap %>% 
  filter(PU_CODE == 'kasky')

colnames(Gap_group_C)[colnames(Gap_group_C)=="PU_CODE"] <- "PU_Code"
colnames(Gap_group_C)[colnames(Gap_group_C)=="GAP_CODE"] <- "Gap_Code"
colnames(Gap_group_C)[colnames(Gap_group_C)=="PU_GAP"] <- "PU_Gap_Code"

Gap_group_C$C_Urban <- Gap_group_C$C_LU11P + Gap_group_C$C_LU12P + Gap_group_C$C_LU13P + Gap_group_C$C_LU14P
Gap_group_C$C_Agriculture <- Gap_group_C$C_LU21P + Gap_group_C$C_LU22P + Gap_group_C$C_LU23P
Gap_group_C$C_Grassland <- Gap_group_C$C_LU30P
#Forested Total = Forested_wetland + Forested_upland (Forest + All wooded wetland including shrubland)
Gap_group_C$C_Forest_total <- Gap_group_C$C_LU41P + Gap_group_C$C_LU42P + Gap_group_C$C_LU43P + Gap_group_C$C_LU61P + Gap_group_C$C_LU610P + Gap_group_C$C_LU611P + Gap_group_C$C_LU612P + Gap_group_C$C_LU613P
Gap_group_C$C_Forested_upland <- Gap_group_C$C_LU41P + Gap_group_C$C_LU42P + Gap_group_C$C_LU43P
# Forested_wetland is all wooded wetland including C_LU610P (Wooded Wetland, shrubland)
Gap_group_C$C_Forested_wetland <- Gap_group_C$C_LU61P + Gap_group_C$C_LU610P + Gap_group_C$C_LU611P + Gap_group_C$C_LU612P + Gap_group_C$C_LU613P
Gap_group_C$C_Inland_water <- Gap_group_C$C_LU50P
# Open wet in SSA is calculated  as inland water/open water + non-wooded wetland (Wetland-emergent herbaceous)
# In GAP we use  Open Water + Wetland, non-wooded 
Gap_group_C$C_Open_wet <- Gap_group_C$C_LU50P + Gap_group_C$C_LU62P
Gap_group_C$C_Wetland_total <- Gap_group_C$C_LU61P + Gap_group_C$C_LU610P + Gap_group_C$C_LU611P + Gap_group_C$C_LU612P + Gap_group_C$C_LU613P + Gap_group_C$C_LU62P
Gap_group_C$C_Barren <- Gap_group_C$C_LU70P
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_C$C_Alluvium_fluvial <- Gap_group_C$C_QG12P + Gap_group_C$C_QG19P + Gap_group_C$C_QG23P
Gap_group_C$C_Attenuated_drift <- Gap_group_C$C_QG14P + Gap_group_C$C_QG22P
Gap_group_C$C_Bedrock <- Gap_group_C$C_QG99P
Gap_group_C$C_Broken_rocky <- Gap_group_C$C_QG11P + Gap_group_C$C_QG14P + Gap_group_C$C_QG22P
# Coarse from SSA does not include Outwash (coarse) A3- C_QG1P & Attenuated drift (coarse) J3- C_QG14P 
# Attenuated drift (coarse) is all 0 for kasky
# Not in the SSA layers: Colluvium (coarse) F3- C_QG32P and Stagnation moraine (coarse) I3- C_QG13P all 0 for kasky
#### Remove C_QG1P if you want identical values to Yong/Brians Group Data
Gap_group_C$C_Coarse <- Gap_group_C$C_QG1P + Gap_group_C$C_QG2P + Gap_group_C$C_QG5P + Gap_group_C$C_QG8P + Gap_group_C$C_QG10P + Gap_group_C$C_QG13P + 
  Gap_group_C$C_QG14P +Gap_group_C$C_QG17P + Gap_group_C$C_QG19P + Gap_group_C$C_QG29P + Gap_group_C$C_QG32P
Gap_group_C$C_Coarse_moraine <- Gap_group_C$C_QG5P + Gap_group_C$C_QG8P + Gap_group_C$C_QG13P
Gap_group_C$C_Colluvium <- Gap_group_C$C_QG11P
Gap_group_C$C_Dune <- Gap_group_C$C_QG29P
Gap_group_C$C_Fine_moraine <- Gap_group_C$C_QG3P + Gap_group_C$C_QG6P + Gap_group_C$C_QG30P
# Fine from SSA does not include Lacustrine clay and silt (fine) E1- C_QG9P
#### Remove C_QG9P if you want identical values to Yong/Brians Group Data
Gap_group_C$C_Fines <- Gap_group_C$C_QG3P +Gap_group_C$C_QG6P + Gap_group_C$C_QG9P + Gap_group_C$C_QG24P + Gap_group_C$C_QG30P
Gap_group_C$C_Icecontact <- Gap_group_C$C_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_C$C_Lacustrine <- Gap_group_C$C_QG9P + Gap_group_C$C_QG10P + Gap_group_C$C_QG31P
Gap_group_C$C_Loess <- Gap_group_C$C_QG24P
# Medium from SSA does not include Attenuated-drift (medium) J2- C_QG22P
# Attenuated drift (medium) is all 0 for kasky
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_C$C_Medium <- Gap_group_C$C_QG4P + Gap_group_C$C_QG7P + Gap_group_C$C_QG16P + Gap_group_C$C_QG20P + Gap_group_C$C_QG22P + Gap_group_C$C_QG23P
Gap_group_C$C_Medium_moraine <- Gap_group_C$C_QG4P + Gap_group_C$C_QG7P + Gap_group_C$C_QG20P
Gap_group_C$C_Outwash <- Gap_group_C$C_QG1P
Gap_group_C$C_Outwash_icecontact <- Gap_group_C$C_QG1P + Gap_group_C$C_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_C$C_Peat_muck <- Gap_group_C$C_QG18P + Gap_group_C$C_QG31P
Gap_group_C$C_Rocky <- Gap_group_C$C_QG11P + Gap_group_C$C_QG14P + Gap_group_C$C_QG22P + Gap_group_C$C_QG99P
# Regroup Bed Rock Depth
Gap_group_C$C_BR50 <- Gap_group_C$C_BD201P
Gap_group_C$C_BR100 <- Gap_group_C$C_BD202P
Gap_group_C$C_BR200 <- Gap_group_C$C_BD203P
Gap_group_C$C_BR400 <- Gap_group_C$C_BD204P
Gap_group_C$C_BRG100 <- Gap_group_C$C_BD203P + Gap_group_C$C_BD204P + Gap_group_C$C_BD205P + Gap_group_C$C_BD206P + Gap_group_C$C_BD207P + Gap_group_C$C_BD208P
# Regroup Bed Rock Type
Gap_group_C$C_BR_sandstone <- Gap_group_C$C_BR1P
Gap_group_C$C_BR_shale <- Gap_group_C$C_BR2P
Gap_group_C$C_BR_carbonate <- Gap_group_C$C_BR3P
Gap_group_C$C_BR_metamorphic <- Gap_group_C$C_BR41P
Gap_group_C$C_BR_igneous <- Gap_group_C$C_BR42P
Gap_group_C$C_BR_unknown <- Gap_group_C$C_BR5P
Gap_group_C$C_BR_water <- Gap_group_C$C_BR6P


Gap_group_C <- Gap_group_C %>%
  select(PU_Gap_Code, PU_Code, Gap_Code,
         C_TOTAL_SQM, C_LENGTH, C_LONG, C_LAT, C_ORDER, LINK, DORDER, DLINK, SINUOUS, GRADIENT, GRADIENT_KM, DLENGTH,
         C_BR_sandstone, C_BR_shale, C_BR_carbonate, C_BR_metamorphic, C_BR_igneous, C_BR_unknown, C_BR_water,
         C_BR50, C_BR100, C_BR200, C_BR400, C_BRG100,
         C_Urban, C_Agriculture, C_Grassland, C_Forest_total, C_Forested_upland, C_Forested_wetland, C_Inland_water, C_Open_wet, C_Wetland_total, C_Barren,
         C_Alluvium_fluvial, C_Attenuated_drift, C_Bedrock, C_Broken_rocky, C_Coarse, C_Coarse_moraine, C_Colluvium, C_Dune, C_Fine_moraine, C_Fines,
         C_Icecontact, C_Lacustrine, C_Loess, C_Medium, C_Medium_moraine, C_Outwash, C_Outwash_icecontact, C_Peat_muck, C_Rocky,
         DCONNECT, DCLENGTH, WT_AREA, BIGRIVER, BR1000, BR2000, BR4000, BRD1000, BRD2000, BRD4000, MISSI, GLAKES, DAMUP, DAMUPL, DAMDW, DAMDWL, DAM,
         PONDUP, PONDUPL, PONDUPA, PONDDW, PONDWL, PONDWA, POND, POND_AREA, 
         C_LSCS1P, C_LSCS2P, C_LSCS3P, C_LSCS4P, C_LSCS5P,
         C_ELEV_X, HYD_CAT)


write.csv(Gap_group_C, file = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_CHANNEL.csv", row.names = F)


