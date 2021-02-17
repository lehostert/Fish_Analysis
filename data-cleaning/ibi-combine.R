### Bring together all of the IBIs for the stream sites 
library(tidyverse)

network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")

# Read in IBI from 2013-2017
ibi17 <- readxl::read_excel(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Data/Data_IN/IBI"))
# Read in IBI from 2018
# Read in IBI from 2019
# Read in IBI from 2020


# Combine into one data set
# Add to fish metrics/table
# Save