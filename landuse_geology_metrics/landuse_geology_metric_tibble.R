library(tidyverse)

Gap <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_WATERSHED_minus_ny.xlsx", 
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
                                                   "numeric","numeric","numeric", "numeric"
                                                   
                          ))

Gap_group <- Gap %>% 
  filter(PU_CODE == 'kasky')

colnames(Gap_group)[colnames(Gap_group)=="PU_CODE"] <- "PU_Code"
colnames(Gap_group)[colnames(Gap_group)=="GAP_CODE"] <- "Gap_Code"
colnames(Gap_group)[colnames(Gap_group)=="PU_GAP"] <- "PU_Gap_Code"

Gap_group$W_Urban <- Gap_group$W_LU11P + Gap_group$W_LU12P + Gap_group$W_LU13P + Gap_group$W_LU14P
Gap_group$W_Agriculture <- Gap_group$W_LU21P + Gap_group$W_LU22P + Gap_group$W_LU23P
Gap_group$W_Grassland <- Gap_group$W_LU30P
#Forested Total = Forested_wetland + Forested_upland (Forest + All wooded wetland including shrubland)
Gap_group$W_Forest_total <- Gap_group$W_LU41P + Gap_group$W_LU42P + Gap_group$W_LU43P + Gap_group$W_LU61P + Gap_group$W_LU610P + Gap_group$W_LU611P + Gap_group$W_LU612P + Gap_group$W_LU613P
Gap_group$W_Forested_upland <- Gap_group$W_LU41P + Gap_group$W_LU42P + Gap_group$W_LU43P
# Forested_wetland is all wooded wetland including W_LU610P (Wooded Wetland, shrubland)
Gap_group$W_Forested_wetland <- Gap_group$W_LU61P + Gap_group$W_LU610P + Gap_group$W_LU611P + Gap_group$W_LU612P + Gap_group$W_LU613P
Gap_group$W_Inland_water <- Gap_group$W_LU50P
# Open wet in SSA is calculated  as inland water/open water + non-wooded wetland (Wetland-emergent herbaceous)
# In GAP we use  Open Water + Wetland, non-wooded 
Gap_group$W_Open_wet <- Gap_group$W_LU50P + Gap_group$W_LU62P
Gap_group$W_Wetland_total <- Gap_group$W_LU61P + Gap_group$W_LU610P + Gap_group$W_LU611P + Gap_group$W_LU612P + Gap_group$W_LU613P + Gap_group$W_LU62P
Gap_group$W_Barren <- Gap_group$W_LU70P
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group$W_Alluvium_fluvial <- Gap_group$W_QG12P + Gap_group$W_QG19P + Gap_group$W_QG23P
Gap_group$W_Attenuated_drift <- Gap_group$W_QG14P + Gap_group$W_QG22P
Gap_group$W_Bedrock <- Gap_group$W_QG99P
Gap_group$W_Broken_rocky <- Gap_group$W_QG11P + Gap_group$W_QG14P + Gap_group$W_QG22P
# Coarse from SSA does not include Outwash (coarse) A3- W_QG1P & Attenuated drift (coarse) J3- W_QG14P 
# Attenuated drift (coarse) is all 0 for kasky
# Not in the SSA layers: Colluvium (coarse) F3- W_QG32P and Stagnation moraine (coarse) I3- W_QG13P all 0 for kasky
#### Remove W_QG1P if you want identical values to Yong/Brians Group Data
Gap_group$W_Coarse <- Gap_group$W_QG1P + Gap_group$W_QG2P + Gap_group$W_QG5P + Gap_group$W_QG8P + Gap_group$W_QG10P + Gap_group$W_QG13P + 
  Gap_group$W_QG14P +Gap_group$W_QG17P + Gap_group$W_QG19P + Gap_group$W_QG29P + Gap_group$W_QG32P
Gap_group$W_Coarse_moraine <- Gap_group$W_QG5P + Gap_group$W_QG8P + Gap_group$W_QG13P
Gap_group$W_Colluvium <- Gap_group$W_QG11P
Gap_group$W_Dune <- Gap_group$W_QG29P
Gap_group$W_Fine_moraine <- Gap_group$W_QG3P + Gap_group$W_QG6P + Gap_group$W_QG30P
# Fine from SSA does not include Lacustrine clay and silt (fine) E1- W_QG9P
#### Remove W_QG9P if you want identical values to Yong/Brians Group Data
Gap_group$W_Fines <- Gap_group$W_QG3P +Gap_group$W_QG6P + Gap_group$W_QG9P + Gap_group$W_QG24P + Gap_group$W_QG30P
Gap_group$W_Icecontact <- Gap_group$W_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group$W_Lacustrine <- Gap_group$W_QG9P + Gap_group$W_QG10P + Gap_group$W_QG31P
Gap_group$W_Loess <- Gap_group$W_QG24P
# Medium from SSA does not include Attenuated-drift (medium) J2- W_QG22P
# Attenuated drift (medium) is all 0 for kasky
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group$W_Medium <- Gap_group$W_QG4P + Gap_group$W_QG7P + Gap_group$W_QG16P + Gap_group$W_QG20P + Gap_group$W_QG22P + Gap_group$W_QG23P
Gap_group$W_Medium_moraine <- Gap_group$W_QG4P + Gap_group$W_QG7P + Gap_group$W_QG20P
Gap_group$W_Outwash <- Gap_group$W_QG1P
Gap_group$W_Outwash_icecontact <- Gap_group$W_QG1P + Gap_group$W_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group$W_Peat_muck <- Gap_group$W_QG18P + Gap_group$W_QG31P
Gap_group$W_Rocky <- Gap_group$W_QG11P + Gap_group$W_QG14P + Gap_group$W_QG22P + Gap_group$W_QG99P
# Regroup Bed Rock Depth
Gap_group$W_BD50 <- Gap_group$W_BD201P
Gap_group$W_BD100 <- Gap_group$W_BD202P
Gap_group$W_BD200 <- Gap_group$W_BD203P
Gap_group$W_BD400 <- Gap_group$W_BD204P
# Regroup Bed Rock Type
Gap_group$W_BR_sandstone <- Gap_group$W_BR1P
Gap_group$W_BR_shale <- Gap_group$W_BR2P
Gap_group$W_BR_carbonate <- Gap_group$W_BR3P
Gap_group$W_BR_metamorphic <- Gap_group$W_BR41P
Gap_group$W_BR_igneous <- Gap_group$W_BR42P
Gap_group$W_BR_unknown <- Gap_group$W_BR5P
Gap_group$W_BR_water <- Gap_group$W_BR6P


Gap_group_W <- Gap_group %>%
  select(PU_Gap_Code, PU_Code, Gap_Code,
         W_TOTAL_SQM, W_PERMX, W_PREC, W_GDD, W_MAATX, W_JULMEX, W_JULMNX,  W_JULMXX, W_SLOPE, W_DARCYX,
         W_BR_sandstone, W_BR_shale, W_BR_carbonate, W_BR_metamorphic, W_BR_igneous, W_BR_unknown, W_BR_water,
         W_BD50, W_BD100, W_BD200, W_BD400,
         W_Urban, W_Agriculture, W_Grassland, W_Forest_total, W_Forested_upland, W_Forested_wetland, W_Inland_water, W_Open_wet, W_Wetland_total, W_Barren,
         W_Alluvium_fluvial, W_Attenuated_drift, W_Bedrock, W_Broken_rocky, W_Coarse, W_Coarse_moraine, W_Colluvium, W_Dune, W_Fine_moraine, W_Fines,
         W_Icecontact, W_Lacustrine, W_Loess, W_Medium, W_Medium_moraine, W_Outwash, W_Outwash_icecontact, W_Peat_muck, W_Rocky)

