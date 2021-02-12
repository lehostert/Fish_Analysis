## The purpose fo this script to to take tidy data set of fish species counts and compute various fish metrics on a per site basis
## and ultimately produce a tibble of all of the per site fish metrics.
## First functions for the various metrics are created 
## Second Fish data is loaded in a tidy dataframe (such as from a Database) 
## Fish data should be in a style where site, date, rep info has been combined to a unique name in a single column called Site_ID
## Third the "site_metric_tibble" base is created with basic indicies from 'vegan' package. 
## Finally, additional metrics are computed and added to the "site_metric_tibble"

library(tidyverse)
library(vegan)
library(docstring)


#### Set up access to INHS-Bison Network Drive ####
network_prefix <- if_else(as.character(Sys.info()["sysname"]) == "Windows", "//INHS-Bison", "/Volumes")

# TODO Change instances of "Site_ID" "Fish_Species_Code" and "Fish_Species_Count" to string to lower to make more generic

#### Create Functions ####

add_traits_to_data <- function(species_count_data) {
  
  #' Create dataframe in which species specific traits are joined with data collection data. 
  #'
  #' 
  #' @param species_count_data A dataframe with at least 3 collumns unique idenifying name for sample (Site_ID), 
  #' 3-letter short name for fish species (Fish_Species_Code), count of the number of that specific species sampled (Fish_Species_Count)
  #' 
  #' This adds species specific trait informaition compiled from the following sources:
  #' VT Fish Traits
  #' Emmanuel Frimpong, and Paul L. Angermeier, 200811, Fish Traits Database: USGS,  https://doi.org/10.5066/F7WD3ZH8.
  #' Accessed from USGS sciencebase through https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0 on 2019-02-11
  #' 
  #' USGS Tolerance Data
  #' M.R. Meador and D.M. Carlisle.  2007.  Quantifying tolerance indicator values for common fish species of the United States. Ecological Indicators, 7:329-338.
  #' Available from USGS Ecological National Synthesis (ENS) Project "Fish Traits & Tolerance Data"  https://water.usgs.gov/nawqa/ecology/data.html 
  #' Specifically <https://water.usgs.gov/nawqa/ecology/pubs/FishToleranceIndicatorValuesTables.xls> (accessed 2019-02-21)
  #' 
  #' Additional Tolerance Class Data was communicated directly from Dr. M.R. Meador and filled in missing values for the following fish:
  #' BGB, BUD, CAP, DUD, FRM, MUD, ORD, RDS, SES, SHD, SLD, SLM, SRD, SUM, SVS, WHS, YLB
  #' 
  #' Calculated binary Values from VT Traits
  #' HERBIVORE = "1" if ALGPHYTO or MACVASCU or DETRITUS = 1 ,  "0" if none of them = 1
  #' OMNIVORE = "1" if more than one of HERBIVORE INVLVFSH FSHCRCRB BLOOD EGGS OTHER = 1,"0" if only one =1
  #' LITHOPHILIC = "1" if GRAVEL or COBBLE or BOULDER = 1, "0" if none of them = 1
  #' BENTHIC_INSECTIVORE = "1" if BENTHIC and INVLVFSH = 1 , "0" if one or both = 0
  #' 
  #' MIN and MAXTEMP values were added for WHS and CAP as -8.9 and 28.9 for the 30 year ave min (Jan) and ave max (July) from NOAA records for Champaign, IL
  
  
  il_fish_traits <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Illinois_fish_traits_complete.csv"), na = "", stringsAsFactors = F)
  il_fish_traits$Native_Intolerant <- ifelse(il_fish_traits$Nonnative == '0' & il_fish_traits$Tolerance_Class == 'INTOLERANT', 1, 0)
  
  fish_table <- species_count_data %>% 
    select(c(Site_ID, Fish_Species_Code, Fish_Species_Count))%>%
    left_join(il_fish_traits, by = 'Fish_Species_Code')
}

create_site_metric_tibble <- function(counts_and_traits) {
  
  #' Create base for the site metric tibble using functions from package 'vegan'
  #' 
  #' Creates a dataframe with calculated features number of individuals, species richness, shannon diversity index,
  #' and eveness for each site ids in the input tibble
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  #' 
  
  # Use quosure to be able to evaluate desired trait and remove the quotes the tidyverse is going to automatically put around it.
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function>  OR
  # See Hadley's vingette <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html> for more details on why enquo is necessary.
  # trait <- dplyr::enquo(trait) 
  
  sparse_fish_data <- counts_and_traits %>%
    select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
    spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
  # vegan::diversity requires row names for evaluation. 
  #This will make Site_ID the row name then remove the collmun Site_ID after row names are assigned
  row.names(sparse_fish_data) <- sparse_fish_data$Site_ID
  sparse_fish_data <- sparse_fish_data %>% select(-c(Site_ID))
  
  # Begin metric computation from sparse dataset
  INDIVIDUALS <- rowSums(sparse_fish_data)
  RICHNESS <- rowSums(sparse_fish_data != 0) 
  DIVERSITY <- vegan::diversity(sparse_fish_data, index = "shannon")
  EVENNESS <- vegan::diversity(sparse_fish_data, index = "shannon")/log(vegan::specnumber(sparse_fish_data))
  
  # Create dataframe from computed metrics and convert row names back to a collumn in the dataframe.
  # Consider making this a tibble directly instead of a dataframe. 
  data.frame(INDIVIDUALS,RICHNESS,DIVERSITY,EVENNESS) %>%
    tibble::rownames_to_column(var = "Site_ID")
  
}

num_taxa_by_trait <- function(counts_and_traits, desired_trait, value) {
  
  #' Get number of unique taxa per specified trait per site (*NTAX)
  #' 
  #' Create a named vector of the counts of the trait in question for
  #' the site ids in the input tibble
  #' 
  #' Used to create fields for the final Site Metric tibble. The return value
  #' is a named vector to match the data structure of the other Site Metric fields.
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  #' 
  #' @param desired_trait The animal trait in question to filter.
  #' 
  #' @param value The value of the desired trait
  
  # Use quosure to be able to evaluate desired trait and remove the quotes the tidyverse is going to automatically put around it.
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function>  OR
  # See Hadley's vingette <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html> for more details on why enquo is necessary.
  # In dplyr (and in tidyeval in general) you use !! to say that you want to unquote an input so that it’s evaluated, not quoted.
  desired_trait <- dplyr::enquo(desired_trait) 
  
  # Filter out the family of interest
  n_taxa_by_trait <- counts_and_traits %>% 
    dplyr::group_by(Site_ID, !!desired_trait) %>% 
    dplyr::summarise(count = n()) %>%
    filter(!!desired_trait == value)
  
  # Group counts by site ID (this will be a template to overwrite)
  site_id_tibble <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>% 
    dplyr::summarise(sp_count = n())
  
  # Then mutate the counts to be those of n_taxa_by_family to
  # get a named vector of site IDs with the filtered family count

  site_id_taxa_by_trait <- site_id_tibble %>% 
    dplyr::left_join(n_taxa_by_trait, by = "Site_ID") %>% 
    tidyr::replace_na(list(count = 0)) %>% 
    dplyr::select(x = count, nm = Site_ID) %>% 
    purrr::pmap(set_names) %>% 
    unlist
  
  return(site_id_taxa_by_trait)
}

num_ind_by_trait <- function(counts_and_traits, desired_trait, value) {
  
  #' Get number of individuals per specified family per site (*NIND)
  #' 
  #' Create a named vector of the counts of the family in question for
  #' the site ids in the input tibble
  #' 
  #' Used to create fields for the final Site Metric tibble. The return value
  #' is a named vector to match the data structure of the other Site Metric fields.
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  #' 
  #' @param desired_trait The animal trait in question to filter. Must be binary coded. 
  #' 
  #' @param value The value of the desired trait
  
  # Use quosure to be able to evaluate desired trait and remove the quotes the tidyverse is going to automatically put around it.
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function>  OR
  # See Hadley's vingette <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html> for more details on why enquo is necessary.
  desired_trait <- dplyr::enquo(desired_trait)
  
  # Summarise by number of indiviudals and filter out the family of interest
  n_ind_by_trait<- counts_and_traits %>% 
    dplyr::group_by(Site_ID, !!desired_trait) %>% 
    dplyr::summarise(sum(Fish_Species_Count)) %>%
    filter(!!desired_trait == value)
  
  #Rename column to avoid errors in the mutate function below
  colnames(n_ind_by_trait)[colnames(n_ind_by_trait)=="sum(Fish_Species_Count)"] <- "count"
  
  # Group counts by site ID (this will be a template to overwrite)
  site_id_tibble <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>% 
    dplyr::summarise(sp_count = n())
  
  # Then mutate the counts to be those of n_taxa_by_family to
  # get a named vector of site IDs with the filtered family count
  site_id_ind_by_trait <- site_id_tibble %>%
    dplyr::left_join(n_ind_by_trait, by = "Site_ID") %>% 
    tidyr::replace_na(list(count = 0))%>%
    dplyr::select(x = count, nm = Site_ID) %>%
    purrr::pmap(set_names) %>% 
    unlist
  
  
  return(site_id_ind_by_trait)
}

average_by_trait <- function(counts_and_traits, trait) {
  
  #' Get average level of a cerain trait on a per Site_ID basis (*TAX)
  #' For example average Suspended Sediment Tolerance for taxa at a site.  
  #' 
  #' Used to create fields for the final Site Metric Tibble. The return value
  #' is a named vector to match the data structure of the other site metric fields.
  #' 
  #' Missing trait values (NAs) are excluded from computation.
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  #' 
  #' @param trait The animal trait in question to filter (eg.Total Phosphorus Tolerance).
  
  # Use quosure to be able to evaluate desired trait and remove the quotes the tidyverse is going to automatically put around it.
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function>  OR
  # See Hadley's vingette <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html> for more details on why enquo is necessary.
  trait <- dplyr::enquo(trait) 
  
  # Group by Site_ID, then commute the mean DO for each site
  # Then mutate the DO mean to get a named vector of site IDs with the mean.
  site_id_average <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>% 
    dplyr::summarise(mean = mean(!!trait, na.rm = TRUE)) %>%
    dplyr::select(x = mean, nm = Site_ID) %>%
    purrr::pmap(set_names) %>% 
    unlist
  
  return(site_id_average)
}

weighted_average_by_trait <- function(counts_and_traits, trait) {
  
  #' Get average level of a cerain trait weighted by number of individuals per taxa at a given Site_ID (*IND)
  #' For example average Suspended Sediment Tolerance for total individuals at a site.  
  #' 
  #' Used to create fields for the final Site Metric Tibble. The return value
  #' is a named vector to match the data structure of the other site metric fields.
  #' 
  #' Missing trait values (NAs) are excluded from computation. 
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  #' 
  #' @param trait The animal trait in question to filter (eg.Total Phosphorus Tolerance)

  
  # Use quosure to be able to evaluate desired trait and remove the quotes the tidyverse is going to automatically put around it.
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function>  OR
  # See Hadley's vingette <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html> for more details on why enquo is necessary.
  trait <- dplyr::enquo(trait) 
  
  # Group by Site_ID, then commute the mean DO for each site
  # Then mutate the DO mean to get a named vector of site IDs with the mean.
  site_id_w_average <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>% 
    dplyr::summarise(wmean = weighted.mean(!!trait, Fish_Species_Count, na.rm = TRUE)) %>%
    dplyr::select(x = wmean, nm = Site_ID) %>%
    purrr::pmap(set_names) %>% 
    unlist
  
  return(site_id_w_average)
}

fecundity_by_total_length <- function(counts_and_traits) {
  
  #' Get average species fecundity adjusted for total length
  #' 
  #' Create a named vector of the average fecundity corrected by total length
  #' for the site ids in the input tibble
  #' 
  #' Used to create fields for the final Site Metric Tibble. The return value
  #' is a named vector to match the data structure of the other site metric fields.
  #' 
  #' @param counts_and_traits A tibble of count data and animal traits
  
  # Calculate the fecundity adjusted by total length
  # Group by Site_ID, then compute the mean adjusted fecundity for each site
  # Then select just the mean and site info to produce a named vector of site IDs with the mean.
  site_id_fecund <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>%
    dplyr::mutate(fecund_tl = FECUNDITY/MAXTL)%>%
    dplyr::summarise(mean = mean(fecund_tl)) %>%
    dplyr::select(x = mean, nm = Site_ID) %>%
    purrr::pmap(set_names) %>%
    unlist
  
  
  return(site_id_fecund)
}

#### Load fish count data ####
#TODO Add checker here that looks for the following 3 fields: "Site_ID" "Fish_Species_Code" and "Fish_Species_Count"

## For CREP data
# fish_data <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data_CREP_2013-2019.csv"), na = "", stringsAsFactors = F)

## For IDNR Basin data
fish_data <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data_Drake_1991-2007.csv"), na = "", stringsAsFactors = F)


## For IDNR Basin Data ##
# fish_data_path <- file.choose()
# fish_data <- readr::read_csv(fish_data_path, na = "")

#### Create unique Site_ID per sample if this has not already been created ####
## For CREP and IDNR Basin data
# fish_data$Event_Date <- as.Date(fish_data$Event_Date, "%m/%d/%Y")
fish_data$Event_Date <- as.Date(fish_data$Event_Date)

fish_data$Site_ID <-paste(str_replace_all(fish_data$Reach_Name, "[:blank:]", ""), str_replace_all(fish_data$Event_Date,"-",""), sep = "_")

#### Add fish traits ####
# Before moving on the fish count data must have the following 3 fields: "Site_ID" "Fish_Species_Code" and "Fish_Species_Count"
# names(fish_data)
# 
# id_table <- fish_data %>% 
#   select(Site_ID, pugap_code, Reach_Name, Event_Date) %>% 
#   unique()
# write_csv(id_table, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_CREP_2013-2019.csv"))
# write_csv(id_table, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/id_table_Drake_1991-2007.csv"))

# Add traits to fish count data
fish_table <- add_traits_to_data(fish_data)

# Remove Hybrid or Unidentified Species from this analysis
fish_table <- fish_table %>%
  filter (Hybrid == 0, Unidentified_Species == 0)

#### Create Metric Tibble ####
# Create Tibble with basic diversity indices from 'vegan'.
# This tibble will be the base for storing all additional computed site metrics. 
site_metric_tibble <- create_site_metric_tibble(fish_table)
site_metric_tibble$EVENNESS[is.nan(site_metric_tibble$EVENNESS)]<-0

# Compute and add additional site metrics to site_metric_tibble
# Taxonomic traits
site_metric_tibble$CATONTAX <- num_taxa_by_trait(fish_table, Family, 'Catostomidae')
site_metric_tibble$CATOPTAX <- round(site_metric_tibble$CATONTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$CATONIND <- num_ind_by_trait(fish_table, Family, 'Catostomidae')
site_metric_tibble$CATOPIND <- round(site_metric_tibble$CATONIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$CENTNTAX <- num_taxa_by_trait(fish_table, Family, 'Centrarchidae')
site_metric_tibble$CENTPTAX <- round(site_metric_tibble$CENTNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$CENTNIND <- num_ind_by_trait(fish_table, Family, 'Centrarchidae')
site_metric_tibble$CENTPIND <- round(site_metric_tibble$CENTNIND/site_metric_tibble$INDIVIDUALS, digits = 3)  

site_metric_tibble$CYPRNTAX <- num_taxa_by_trait(fish_table, Family, 'Cyprinidae')
site_metric_tibble$CYPRPTAX <- round(site_metric_tibble$CYPRNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$CYPRNIND <- num_ind_by_trait(fish_table, Family, 'Cyprinidae')
site_metric_tibble$CYPRPIND <- round(site_metric_tibble$CYPRNIND/site_metric_tibble$INDIVIDUALS, digits = 3)  

site_metric_tibble$LEUCNTAX <- num_taxa_by_trait(fish_table, Family, 'Leucisidae')
site_metric_tibble$LEUCPTAX <- round(site_metric_tibble$LEUCNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$LEUCNIND <- num_ind_by_trait(fish_table, Family, 'Leucisidae')
site_metric_tibble$LEUCPIND <- round(site_metric_tibble$LEUCNIND/site_metric_tibble$INDIVIDUALS, digits = 3)  

site_metric_tibble$XENONTAX <- num_taxa_by_trait(fish_table, Family, 'Xenocyprididae')
site_metric_tibble$XENOPTAX <- round(site_metric_tibble$XENONTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$XENONIND <- num_ind_by_trait(fish_table, Family, 'Xenocyprididae')
site_metric_tibble$XENOPIND <- round(site_metric_tibble$XENONIND/site_metric_tibble$INDIVIDUALS, digits = 3)  

site_metric_tibble$CLXNTAX <- site_metric_tibble$CYPRNTAX + site_metric_tibble$LEUCNTAX + site_metric_tibble$XENONTAX
site_metric_tibble$CLXPTAX <- round(site_metric_tibble$CLXNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$CLXNIND <- site_metric_tibble$CYPRNIND + site_metric_tibble$LEUCNIND + site_metric_tibble$XENONIND
site_metric_tibble$CLXPIND <- round(site_metric_tibble$CLXNIND/site_metric_tibble$INDIVIDUALS, digits = 3)  

site_metric_tibble$ICTANTAX <- num_taxa_by_trait(fish_table, Family, 'Ictaluridae')
site_metric_tibble$ICTAPTAX <- round(site_metric_tibble$ICTANTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$ICTANIND <- num_ind_by_trait(fish_table, Family, 'Ictaluridae')
site_metric_tibble$ICTAPIND <- round(site_metric_tibble$ICTANIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PERCNTAX <- num_taxa_by_trait(fish_table, Family, 'Percidae')
site_metric_tibble$PERCPTAX <- round(site_metric_tibble$PERCNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PERCNIND <- num_ind_by_trait(fish_table, Family, 'Percidae')
site_metric_tibble$PERCPIND <- round(site_metric_tibble$PERCNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$ALIENNTAX <- num_taxa_by_trait(fish_table, Nonnative, '1')
site_metric_tibble$ALIENPTAX <- round(site_metric_tibble$ALIENNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$ALIENNIND <- num_ind_by_trait(fish_table, Nonnative, '1')
site_metric_tibble$ALIENPIND <- round(site_metric_tibble$ALIENNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$NATIVNTAX <- num_taxa_by_trait(fish_table, Nonnative, '0')
site_metric_tibble$NATIVPTAX <- round(site_metric_tibble$NATIVNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$NATIVNIND <- num_ind_by_trait(fish_table, Nonnative, '0')
site_metric_tibble$NATIVPIND <- round(site_metric_tibble$NATIVNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$NATINTNTAX <- num_taxa_by_trait(fish_table, Native_Intolerant, '1')
site_metric_tibble$NATINTPTAX <- round(site_metric_tibble$NATINTNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$NATINTNIND <- num_ind_by_trait(fish_table, Native_Intolerant, '1')
site_metric_tibble$NATINTPIND <- round(site_metric_tibble$NATINTNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

## Tolerance traits
site_metric_tibble$TOLRNTAX <- num_taxa_by_trait(fish_table, Tolerance_Class, 'TOLERANT')
site_metric_tibble$TOLRPTAX <- round(site_metric_tibble$TOLRNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$TOLRNIND <- num_ind_by_trait(fish_table, Tolerance_Class, 'TOLERANT')
site_metric_tibble$TOLRPIND <- round(site_metric_tibble$TOLRNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$INTOLNTAX <- num_taxa_by_trait(fish_table, Tolerance_Class, 'INTOLERANT')
site_metric_tibble$INTOLPTAX <- round(site_metric_tibble$INTOLNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$INTOLNIND <- num_ind_by_trait(fish_table, Tolerance_Class, 'INTOLERANT')
site_metric_tibble$INTOLPIND <- round(site_metric_tibble$INTOLNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$MODTOLNTAX <- num_taxa_by_trait(fish_table, Tolerance_Class, 'MODERATE')
site_metric_tibble$MODTOLPTAX <- round(site_metric_tibble$MODTOLNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$MODTOLNIND <- num_ind_by_trait(fish_table, Tolerance_Class, 'MODERATE')
site_metric_tibble$MODTOLPIND <- round(site_metric_tibble$MODTOLNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$DOTOLTAX <- average_by_trait(fish_table, Dissolved_Oxygen)
site_metric_tibble$DOTOLIND <- weighted_average_by_trait(fish_table, Dissolved_Oxygen)

site_metric_tibble$NO2TOLTAX <- average_by_trait(fish_table, Nitrate_Nitrite)
site_metric_tibble$NO2TOLIND <- weighted_average_by_trait(fish_table, Nitrate_Nitrite)

site_metric_tibble$TPHOSTOLTAX <- average_by_trait(fish_table, Total_Phosphorus)
site_metric_tibble$TPHOSTOLIND <- weighted_average_by_trait(fish_table, Total_Phosphorus)

site_metric_tibble$SUSSEDTOLTAX <- average_by_trait(fish_table, Suspended_Sediment)
site_metric_tibble$SUSSEDTOLIND <- weighted_average_by_trait(fish_table, Suspended_Sediment)

site_metric_tibble$TEMPMAXTOLTAX <- average_by_trait(fish_table, MAXTEMP)
site_metric_tibble$TEMPMAXTOLIND <- weighted_average_by_trait(fish_table, MAXTEMP)

site_metric_tibble$TEMPMINTOLTAX <- average_by_trait(fish_table, MINTEMP)
site_metric_tibble$TEMPMINTOLIND <- weighted_average_by_trait(fish_table, MINTEMP)

# Life history traits

site_metric_tibble$SPAWNDUR <- average_by_trait(fish_table, SEASON)
site_metric_tibble$FECUNDITY_TL <- fecundity_by_total_length(fish_table)
site_metric_tibble$LONGEVITY <- average_by_trait(fish_table, LONGEVITY)
site_metric_tibble$MATUAGE <- average_by_trait(fish_table, MATUAGE)

site_metric_tibble$PCNGOSNTAX <- (num_taxa_by_trait(fish_table, A_1_1, '1') + 
                                    num_taxa_by_trait(fish_table, A_1_2, '1') + 
                                    num_taxa_by_trait(fish_table, A_1_3A, '1') +
                                    num_taxa_by_trait(fish_table, A_1_3B, '1') +
                                    num_taxa_by_trait(fish_table, A_1_3C, '1') +
                                    num_taxa_by_trait(fish_table, A_1_4, '1') +
                                    num_taxa_by_trait(fish_table, A_1_5, '1') +
                                    num_taxa_by_trait(fish_table, A_1_6, '1') 
                                  )
site_metric_tibble$PCNGOSPTAX <- round(site_metric_tibble$PCNGOSNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PCNGOSNIND <- (num_ind_by_trait(fish_table, A_1_1, '1') + 
                                    num_ind_by_trait(fish_table, A_1_2, '1') + 
                                    num_ind_by_trait(fish_table, A_1_3A, '1') +
                                    num_ind_by_trait(fish_table, A_1_3B, '1') +
                                    num_ind_by_trait(fish_table, A_1_3C, '1') +
                                    num_ind_by_trait(fish_table, A_1_4, '1') +
                                    num_ind_by_trait(fish_table, A_1_5, '1') +
                                    num_ind_by_trait(fish_table, A_1_6, '1') 
                                  )
site_metric_tibble$PCNGOSPIND <- round(site_metric_tibble$PCNGOSNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PCNGBHNTAX <- (num_taxa_by_trait(fish_table, A_2_3A, '1') + 
                                    num_taxa_by_trait(fish_table, A_2_3B, '1') + 
                                    num_taxa_by_trait(fish_table, A_2_3C, '1') +
                                    num_taxa_by_trait(fish_table, A_2_4A, '1') +
                                    num_taxa_by_trait(fish_table, A_2_4C, '1')
                                  )
site_metric_tibble$PCNGBHPTAX <- round(site_metric_tibble$PCNGBHNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PCNGBHNIND <- (num_ind_by_trait(fish_table, A_2_3A, '1') + 
                                    num_ind_by_trait(fish_table, A_2_3B, '1') + 
                                    num_ind_by_trait(fish_table, A_2_3C, '1') +
                                    num_ind_by_trait(fish_table, A_2_4A, '1') +
                                    num_ind_by_trait(fish_table, A_2_4C, '1')
                                  )
site_metric_tibble$PCNGBHPIND <- round(site_metric_tibble$PCNGBHNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PCGSUBNTAX <- (num_taxa_by_trait(fish_table, B_1_3A, '1') + num_taxa_by_trait(fish_table, B_1_4, '1'))
site_metric_tibble$PCGSUBPTAX <- round(site_metric_tibble$PCGSUBNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PCGSUBNIND <- (num_ind_by_trait(fish_table, B_1_3A, '1') + num_ind_by_trait(fish_table, B_1_4, '1'))
site_metric_tibble$PCGSUBPIND <- round(site_metric_tibble$PCGSUBNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PCNESTNTAX <- (num_taxa_by_trait(fish_table, B_2_2, '1') + 
                                    num_taxa_by_trait(fish_table, B_2_3A, '1') + 
                                    num_taxa_by_trait(fish_table, B_2_3B, '1') +
                                    num_taxa_by_trait(fish_table, B_2_4, '1') +
                                    num_taxa_by_trait(fish_table, B_2_5, '1') +
                                    num_taxa_by_trait(fish_table, B_2_6, '1') + 
                                    num_taxa_by_trait(fish_table, B_2_7A, '1') +
                                    num_taxa_by_trait(fish_table, B_2_7B, '1') +
                                    num_taxa_by_trait(fish_table, B_2_7C, '1')
)
site_metric_tibble$PCNESTPTAX <- round(site_metric_tibble$PCNESTNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PCNESTNIND <- (num_ind_by_trait(fish_table, B_2_2, '1') + 
                                    num_ind_by_trait(fish_table, B_2_3A, '1') + 
                                    num_ind_by_trait(fish_table, B_2_3B, '1') +
                                    num_ind_by_trait(fish_table, B_2_4, '1') +
                                    num_ind_by_trait(fish_table, B_2_5, '1') +
                                    num_ind_by_trait(fish_table, B_2_6, '1') + 
                                    num_ind_by_trait(fish_table, B_2_7A, '1') +
                                    num_ind_by_trait(fish_table, B_2_7B, '1') +
                                    num_ind_by_trait(fish_table, B_2_7C, '1')
)
site_metric_tibble$PCNESTPIND <- round(site_metric_tibble$PCNESTNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PCBEARNTAX <- num_taxa_by_trait(fish_table, C1_3_4_C24, '1')
site_metric_tibble$PCBEARPTAX <- round(site_metric_tibble$PCBEARNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PCBEARNIND <- num_ind_by_trait(fish_table, C1_3_4_C24, '1')
site_metric_tibble$PCBEARPIND <- round(site_metric_tibble$PCBEARNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$FSNTAX <- num_taxa_by_trait(fish_table, PREFLOT, '1')
site_metric_tibble$FSPTAX <- round(site_metric_tibble$FSNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$FSNIND <- num_ind_by_trait(fish_table, PREFLOT, '1')
site_metric_tibble$FSPIND <- round(site_metric_tibble$FSNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$LITHNTAX <- num_taxa_by_trait(fish_table, LITHOPHILIC, '1')
site_metric_tibble$LITHPTAX <- round(site_metric_tibble$LITHNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$LITHNIND <- num_ind_by_trait(fish_table, LITHOPHILIC, '1')
site_metric_tibble$LITHPIND <- round(site_metric_tibble$LITHNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$CARNNTAX <- num_taxa_by_trait(fish_table, FSHCRCRB, '1')
site_metric_tibble$CARNPTAX <- round(site_metric_tibble$CARNNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$CARNNIND <- num_ind_by_trait(fish_table, FSHCRCRB, '1')
site_metric_tibble$CARNPIND <- round(site_metric_tibble$CARNNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$INVNTAX <- num_taxa_by_trait(fish_table, INVLVFSH, '1')
site_metric_tibble$INVPTAX <- round(site_metric_tibble$INVNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$INVNIND <- num_ind_by_trait(fish_table, INVLVFSH, '1')
site_metric_tibble$INVPIND <- round(site_metric_tibble$INVNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$HERBNTAX <- num_taxa_by_trait(fish_table, HERBIVORE, '1')
site_metric_tibble$HERBPTAX <- round(site_metric_tibble$HERBNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$HERBNIND <- num_ind_by_trait(fish_table, HERBIVORE, '1')
site_metric_tibble$HERBPIND <- round(site_metric_tibble$HERBNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$OMNINTAX <- num_taxa_by_trait(fish_table, OMNIVORE, '1')
site_metric_tibble$OMNIPTAX <- round(site_metric_tibble$OMNINTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$OMNININD <- num_ind_by_trait(fish_table, OMNIVORE, '1')
site_metric_tibble$OMNIPIND <- round(site_metric_tibble$OMNININD/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$ALGNTAX <- num_taxa_by_trait(fish_table, ALGPHYTO, '1')
site_metric_tibble$ALGPTAX <- round(site_metric_tibble$ALGNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$ALGNIND <- num_ind_by_trait(fish_table, ALGPHYTO, '1')
site_metric_tibble$ALGPIND <- round(site_metric_tibble$ALGNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$PLANTNTAX <- num_taxa_by_trait(fish_table, MACVASCU, '1')
site_metric_tibble$PLANTPTAX <- round(site_metric_tibble$PLANTNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$PLANTNIND <- num_ind_by_trait(fish_table, MACVASCU, '1')
site_metric_tibble$PLANTPIND <- round(site_metric_tibble$PLANTNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$DETNTAX <- num_taxa_by_trait(fish_table, DETRITUS, '1')
site_metric_tibble$DETPTAX <- round(site_metric_tibble$DETNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$DETNIND <- num_ind_by_trait(fish_table, DETRITUS, '1')
site_metric_tibble$DETPIND <- round(site_metric_tibble$DETNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$BENTINVNTAX <- num_taxa_by_trait(fish_table, BENTHIC_INSECTIVORE, '1')
site_metric_tibble$BENTINVPTAX <- round(site_metric_tibble$BENTINVNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$BENTINVNIND <- num_ind_by_trait(fish_table, BENTHIC_INSECTIVORE, '1')
site_metric_tibble$BENTINVPIND <- round(site_metric_tibble$BENTINVNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$COSUBNTAX <- (num_taxa_by_trait(fish_table, A_1_3A, '1') + 
                                 num_taxa_by_trait(fish_table, A_1_3B, '1') + 
                                 num_taxa_by_trait(fish_table, A_2_3A, '1') +
                                 num_taxa_by_trait(fish_table, A_2_3B, '1') 
) 
site_metric_tibble$COSUBPTAX <- round(site_metric_tibble$COSUBNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$COSUBNIND <- (num_ind_by_trait(fish_table, A_1_3A, '1') + 
                                 num_ind_by_trait(fish_table, A_1_3B, '1') + 
                                 num_ind_by_trait(fish_table, A_2_3A, '1') +
                                 num_ind_by_trait(fish_table, A_2_3B, '1')
)
site_metric_tibble$COSUBPIND <- round(site_metric_tibble$COSUBNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble <- select(site_metric_tibble, -ends_with("NIND"))

# write.csv(site_metric_tibble, file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_CREP_2013-2019_rename.csv"), na = "0", row.names = F)
# write.csv(site_metric_tibble, file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_Drake_1991-2007_rename.csv"), na = "0", row.names = F)
write.csv(site_metric_tibble, file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/Fish_Metrics_CREP_2020.csv"), na = "0", row.names = F)