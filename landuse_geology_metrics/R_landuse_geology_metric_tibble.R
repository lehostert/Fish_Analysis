library(tidyverse)

Gap <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_RIPARIAN_minus_ny.xlsx", 
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
                                                   "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric"
                                                   
                          ))

Gap_group_R <- Gap %>% 
  filter(PU_CODE == 'kasky')

colnames(Gap_group_R)[colnames(Gap_group_R)=="PU_CODE"] <- "PU_Code"
colnames(Gap_group_R)[colnames(Gap_group_R)=="GAP_CODE"] <- "Gap_Code"
colnames(Gap_group_R)[colnames(Gap_group_R)=="PU_GAP"] <- "PU_Gap_Code"

Gap_group_R$R_Urban <- Gap_group_R$R_LU11P + Gap_group_R$R_LU12P + Gap_group_R$R_LU13P + Gap_group_R$R_LU14P
Gap_group_R$R_Agriculture <- Gap_group_R$R_LU21P + Gap_group_R$R_LU22P + Gap_group_R$R_LU23P
Gap_group_R$R_Grassland <- Gap_group_R$R_LU30P
#Forested Total = Forested_wetland + Forested_upland (Forest + All wooded wetland including shrubland)
Gap_group_R$R_Forest_total <- Gap_group_R$R_LU41P + Gap_group_R$R_LU42P + Gap_group_R$R_LU43P + Gap_group_R$R_LU61P + Gap_group_R$R_LU610P + Gap_group_R$R_LU611P + Gap_group_R$R_LU612P + Gap_group_R$R_LU613P
Gap_group_R$R_Forested_upland <- Gap_group_R$R_LU41P + Gap_group_R$R_LU42P + Gap_group_R$R_LU43P
# Forested_wetland is all wooded wetland including R_LU610P (Wooded Wetland, shrubland)
Gap_group_R$R_Forested_wetland <- Gap_group_R$R_LU61P + Gap_group_R$R_LU610P + Gap_group_R$R_LU611P + Gap_group_R$R_LU612P + Gap_group_R$R_LU613P
Gap_group_R$R_Inland_water <- Gap_group_R$R_LU50P
# Open wet in SSA is calculated  as inland water/open water + non-wooded wetland (Wetland-emergent herbaceous)
# In GAP we use  Open Water + Wetland, non-wooded 
Gap_group_R$R_Open_wet <- Gap_group_R$R_LU50P + Gap_group_R$R_LU62P
Gap_group_R$R_Wetland_total <- Gap_group_R$R_LU61P + Gap_group_R$R_LU610P + Gap_group_R$R_LU611P + Gap_group_R$R_LU612P + Gap_group_R$R_LU613P + Gap_group_R$R_LU62P
Gap_group_R$R_Barren <- Gap_group_R$R_LU70P
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_R$R_Alluvium_fluvial <- Gap_group_R$R_QG12P + Gap_group_R$R_QG19P + Gap_group_R$R_QG23P
Gap_group_R$R_Attenuated_drift <- Gap_group_R$R_QG14P + Gap_group_R$R_QG22P
Gap_group_R$R_Bedrock <- Gap_group_R$R_QG99P
Gap_group_R$R_Broken_rocky <- Gap_group_R$R_QG11P + Gap_group_R$R_QG14P + Gap_group_R$R_QG22P
# Coarse from SSA does not include Outwash (coarse) A3- R_QG1P & Attenuated drift (coarse) J3- R_QG14P 
# Attenuated drift (coarse) is all 0 for kasky
# Not in the SSA layers: Colluvium (coarse) F3- R_QG32P and Stagnation moraine (coarse) I3- R_QG13P all 0 for kasky
#### Remove R_QG1P if you want identical values to Yong/Brians Group Data
Gap_group_R$R_Coarse <- Gap_group_R$R_QG1P + Gap_group_R$R_QG2P + Gap_group_R$R_QG5P + Gap_group_R$R_QG8P + Gap_group_R$R_QG10P + Gap_group_R$R_QG13P + 
  Gap_group_R$R_QG14P +Gap_group_R$R_QG17P + Gap_group_R$R_QG19P + Gap_group_R$R_QG29P + Gap_group_R$R_QG32P
Gap_group_R$R_Coarse_moraine <- Gap_group_R$R_QG5P + Gap_group_R$R_QG8P + Gap_group_R$R_QG13P
Gap_group_R$R_Colluvium <- Gap_group_R$R_QG11P
Gap_group_R$R_Dune <- Gap_group_R$R_QG29P
Gap_group_R$R_Fine_moraine <- Gap_group_R$R_QG3P + Gap_group_R$R_QG6P + Gap_group_R$R_QG30P
# Fine from SSA does not include Lacustrine clay and silt (fine) E1- R_QG9P
#### Remove R_QG9P if you want identical values to Yong/Brians Group Data
Gap_group_R$R_Fines <- Gap_group_R$R_QG3P +Gap_group_R$R_QG6P + Gap_group_R$R_QG9P + Gap_group_R$R_QG24P + Gap_group_R$R_QG30P
Gap_group_R$R_Icecontact <- Gap_group_R$R_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_R$R_Lacustrine <- Gap_group_R$R_QG9P + Gap_group_R$R_QG10P + Gap_group_R$R_QG31P
Gap_group_R$R_Loess <- Gap_group_R$R_QG24P
# Medium from SSA does not include Attenuated-drift (medium) J2- R_QG22P
# Attenuated drift (medium) is all 0 for kasky
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_R$R_Medium <- Gap_group_R$R_QG4P + Gap_group_R$R_QG7P + Gap_group_R$R_QG16P + Gap_group_R$R_QG20P + Gap_group_R$R_QG22P + Gap_group_R$R_QG23P
Gap_group_R$R_Medium_moraine <- Gap_group_R$R_QG4P + Gap_group_R$R_QG7P + Gap_group_R$R_QG20P
Gap_group_R$R_Outwash <- Gap_group_R$R_QG1P
Gap_group_R$R_Outwash_icecontact <- Gap_group_R$R_QG1P + Gap_group_R$R_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_R$R_Peat_muck <- Gap_group_R$R_QG18P + Gap_group_R$R_QG31P
Gap_group_R$R_Rocky <- Gap_group_R$R_QG11P + Gap_group_R$R_QG14P + Gap_group_R$R_QG22P + Gap_group_R$R_QG99P
# Regroup Bed Rock Depth
Gap_group_R$R_BD50 <- Gap_group_R$R_BD201P
Gap_group_R$R_BD100 <- Gap_group_R$R_BD202P
Gap_group_R$R_BD200 <- Gap_group_R$R_BD203P
Gap_group_R$R_BD400 <- Gap_group_R$R_BD204P
# Regroup Bed Rock Type
Gap_group_R$R_BR_sandstone <- Gap_group_R$R_BR1P
Gap_group_R$R_BR_shale <- Gap_group_R$R_BR2P
Gap_group_R$R_BR_carbonate <- Gap_group_R$R_BR3P
Gap_group_R$R_BR_metamorphic <- Gap_group_R$R_BR41P
Gap_group_R$R_BR_igneous <- Gap_group_R$R_BR42P
Gap_group_R$R_BR_unknown <- Gap_group_R$R_BR5P
Gap_group_R$R_BR_water <- Gap_group_R$R_BR6P

Gap_group_R <- Gap_group_R %>%
  select(PU_Gap_Code, PU_Code, Gap_Code,
         R_TOTAL_SQM, R_PERMX, R_SLOPE, R_DARCYX,
         R_BR_sandstone, R_BR_shale, R_BR_carbonate, R_BR_metamorphic, R_BR_igneous, R_BR_unknown, R_BR_water,
         R_BD50, R_BD100, R_BD200, R_BD400,
         R_Urban, R_Agriculture, R_Grassland, R_Forest_total, R_Forested_upland, R_Forested_wetland, R_Inland_water, R_Open_wet, R_Wetland_total, R_Barren,
         R_Alluvium_fluvial, R_Attenuated_drift, R_Bedrock, R_Broken_rocky, R_Coarse, R_Coarse_moraine, R_Colluvium, R_Dune, R_Fine_moraine, R_Fines,
         R_Icecontact, R_Lacustrine, R_Loess, R_Medium, R_Medium_moraine, R_Outwash, R_Outwash_icecontact, R_Peat_muck, R_Rocky)


write.csv(Gap_group_R, file = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_RIPARIAN.csv", row.names = F)

