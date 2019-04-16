library(tidyverse)

Gap <- readxl::read_excel("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_RIPARIAN_TOTAL_minus_ny.xlsx", 
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

Gap_group_RT <- Gap %>% 
  filter(PU_CODE == 'kasky')

colnames(Gap_group_RT)[colnames(Gap_group_RT)=="PU_CODE"] <- "PU_Code"
colnames(Gap_group_RT)[colnames(Gap_group_RT)=="GAP_CODE"] <- "Gap_Code"
colnames(Gap_group_RT)[colnames(Gap_group_RT)=="PU_GAP"] <- "PU_Gap_Code"

Gap_group_RT$RT_Urban <- Gap_group_RT$RT_LU11P + Gap_group_RT$RT_LU12P + Gap_group_RT$RT_LU13P + Gap_group_RT$RT_LU14P
Gap_group_RT$RT_Agriculture <- Gap_group_RT$RT_LU21P + Gap_group_RT$RT_LU22P + Gap_group_RT$RT_LU23P
Gap_group_RT$RT_Grassland <- Gap_group_RT$RT_LU30P
#Forested Total = Forested_wetland + Forested_upland (Forest + All wooded wetland including shrubland)
Gap_group_RT$RT_Forest_total <- Gap_group_RT$RT_LU41P + Gap_group_RT$RT_LU42P + Gap_group_RT$RT_LU43P + Gap_group_RT$RT_LU61P + Gap_group_RT$RT_LU610P + Gap_group_RT$RT_LU611P + Gap_group_RT$RT_LU612P + Gap_group_RT$RT_LU613P
Gap_group_RT$RT_Forested_upland <- Gap_group_RT$RT_LU41P + Gap_group_RT$RT_LU42P + Gap_group_RT$RT_LU43P
# Forested_wetland is all wooded wetland including RT_LU610P (Wooded Wetland, shrubland)
Gap_group_RT$RT_Forested_wetland <- Gap_group_RT$RT_LU61P + Gap_group_RT$RT_LU610P + Gap_group_RT$RT_LU611P + Gap_group_RT$RT_LU612P + Gap_group_RT$RT_LU613P
Gap_group_RT$RT_Inland_water <- Gap_group_RT$RT_LU50P
# Open wet in SSA is calculated  as inland water/open water + non-wooded wetland (Wetland-emergent herbaceous)
# In GAP we use  Open Water + Wetland, non-wooded 
Gap_group_RT$RT_Open_wet <- Gap_group_RT$RT_LU50P + Gap_group_RT$RT_LU62P
Gap_group_RT$RT_Wetland_total <- Gap_group_RT$RT_LU61P + Gap_group_RT$RT_LU610P + Gap_group_RT$RT_LU611P + Gap_group_RT$RT_LU612P + Gap_group_RT$RT_LU613P + Gap_group_RT$RT_LU62P
Gap_group_RT$RT_Barren <- Gap_group_RT$RT_LU70P
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_RT$RT_Alluvium_fluvial <- Gap_group_RT$RT_QG12P + Gap_group_RT$RT_QG19P + Gap_group_RT$RT_QG23P
Gap_group_RT$RT_Attenuated_drift <- Gap_group_RT$RT_QG14P + Gap_group_RT$RT_QG22P
Gap_group_RT$RT_Bedrock <- Gap_group_RT$RT_QG99P
Gap_group_RT$RT_Broken_rocky <- Gap_group_RT$RT_QG11P + Gap_group_RT$RT_QG14P + Gap_group_RT$RT_QG22P
# Coarse from SSA does not include Outwash (coarse) A3- RT_QG1P & Attenuated drift (coarse) J3- RT_QG14P 
# Attenuated drift (coarse) is all 0 for kasky
# Not in the SSA layers: Colluvium (coarse) F3- RT_QG32P and Stagnation moraine (coarse) I3- RT_QG13P all 0 for kasky
#### Remove RT_QG1P if you want identical values to Yong/Brians Group Data
Gap_group_RT$RT_Coarse <- Gap_group_RT$RT_QG1P + Gap_group_RT$RT_QG2P + Gap_group_RT$RT_QG5P + Gap_group_RT$RT_QG8P + Gap_group_RT$RT_QG10P + Gap_group_RT$RT_QG13P + 
  Gap_group_RT$RT_QG14P +Gap_group_RT$RT_QG17P + Gap_group_RT$RT_QG19P + Gap_group_RT$RT_QG29P + Gap_group_RT$RT_QG32P
Gap_group_RT$RT_Coarse_moraine <- Gap_group_RT$RT_QG5P + Gap_group_RT$RT_QG8P + Gap_group_RT$RT_QG13P
Gap_group_RT$RT_Colluvium <- Gap_group_RT$RT_QG11P
Gap_group_RT$RT_Dune <- Gap_group_RT$RT_QG29P
Gap_group_RT$RT_Fine_moraine <- Gap_group_RT$RT_QG3P + Gap_group_RT$RT_QG6P + Gap_group_RT$RT_QG30P
# Fine from SSA does not include Lacustrine clay and silt (fine) E1- RT_QG9P
#### Remove RT_QG9P if you want identical values to Yong/Brians Group Data
Gap_group_RT$RT_Fines <- Gap_group_RT$RT_QG3P +Gap_group_RT$RT_QG6P + Gap_group_RT$RT_QG9P + Gap_group_RT$RT_QG24P + Gap_group_RT$RT_QG30P
Gap_group_RT$RT_Icecontact <- Gap_group_RT$RT_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_RT$RT_Lacustrine <- Gap_group_RT$RT_QG9P + Gap_group_RT$RT_QG10P + Gap_group_RT$RT_QG31P
Gap_group_RT$RT_Loess <- Gap_group_RT$RT_QG24P
# Medium from SSA does not include Attenuated-drift (medium) J2- RT_QG22P
# Attenuated drift (medium) is all 0 for kasky
# Not in the SSA layers: G2 Alluvium (medium)- QG23P, all 0 for kasky
Gap_group_RT$RT_Medium <- Gap_group_RT$RT_QG4P + Gap_group_RT$RT_QG7P + Gap_group_RT$RT_QG16P + Gap_group_RT$RT_QG20P + Gap_group_RT$RT_QG22P + Gap_group_RT$RT_QG23P
Gap_group_RT$RT_Medium_moraine <- Gap_group_RT$RT_QG4P + Gap_group_RT$RT_QG7P + Gap_group_RT$RT_QG20P
Gap_group_RT$RT_Outwash <- Gap_group_RT$RT_QG1P
Gap_group_RT$RT_Outwash_icecontact <- Gap_group_RT$RT_QG1P + Gap_group_RT$RT_QG2P
# Not in the SSA layers: E4 Lacustrine (peat and muck)- QG31P, all 0 for kasky
Gap_group_RT$RT_Peat_muck <- Gap_group_RT$RT_QG18P + Gap_group_RT$RT_QG31P
Gap_group_RT$RT_Rocky <- Gap_group_RT$RT_QG11P + Gap_group_RT$RT_QG14P + Gap_group_RT$RT_QG22P + Gap_group_RT$RT_QG99P
# Regroup Bed Rock Depth
Gap_group_RT$RT_BD50 <- Gap_group_RT$RT_BD201P
Gap_group_RT$RT_BD100 <- Gap_group_RT$RT_BD202P
Gap_group_RT$RT_BD200 <- Gap_group_RT$RT_BD203P
Gap_group_RT$RT_BD400 <- Gap_group_RT$RT_BD204P
# Regroup Bed Rock Type
Gap_group_RT$RT_BR_sandstone <- Gap_group_RT$RT_BR1P
Gap_group_RT$RT_BR_shale <- Gap_group_RT$RT_BR2P
Gap_group_RT$RT_BR_carbonate <- Gap_group_RT$RT_BR3P
Gap_group_RT$RT_BR_metamorphic <- Gap_group_RT$RT_BR41P
Gap_group_RT$RT_BR_igneous <- Gap_group_RT$RT_BR42P
Gap_group_RT$RT_BR_unknown <- Gap_group_RT$RT_BR5P
Gap_group_RT$RT_BR_water <- Gap_group_RT$RT_BR6P

Gap_group_RT <- Gap_group_RT %>%
  select(PU_Gap_Code, PU_Code, Gap_Code,
         RT_TOTAL_SQME, RT_PERMX, RT_SLOPE, RT_DARCYX,
         RT_BR_sandstone, RT_BR_shale, RT_BR_carbonate, RT_BR_metamorphic, RT_BR_igneous, RT_BR_unknown, RT_BR_water,
         RT_BD50, RT_BD100, RT_BD200, RT_BD400,
         RT_Urban, RT_Agriculture, RT_Grassland, RT_Forest_total, RT_Forested_upland, RT_Forested_wetland, RT_Inland_water, RT_Open_wet, RT_Wetland_total, RT_Barren,
         RT_Alluvium_fluvial, RT_Attenuated_drift, RT_Bedrock, RT_Broken_rocky, RT_Coarse, RT_Coarse_moraine, RT_Colluvium, RT_Dune, RT_Fine_moraine, RT_Fines,
         RT_Icecontact, RT_Lacustrine, RT_Loess, RT_Medium, RT_Medium_moraine, RT_Outwash, RT_Outwash_icecontact, RT_Peat_muck, RT_Rocky,
         RT_LSCS1P, RT_LSCS2P, RT_LSCS3P, RT_LSCS4P, RT_LSCS5P)
