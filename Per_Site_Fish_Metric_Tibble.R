## The purpose fo this script to to take tidy data set and compute various fish metrics on a per site basis
## and ultimately produce a tibble of all of the per site fish metrics.
## First functions for the various metrics are created 
## Second Fish data is loaded in two forms. 1- Sparse "Diversity" style dataframe 2- tidy dataframe from a Database
## Finally, metrics are computed and added to the "site_metric_tibble"

library(tidyverse)
library(vegan)
library(docstring)

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
  
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function> for more details
  # Create quosure to use 
  desired_trait <- dplyr::enquo(desired_trait) 
  
  # Filter out the family of interest
  n_taxa_by_trait <- counts_and_traits %>% 
    dplyr::group_by(Site_ID, !!desired_trait) %>% 
    dplyr::summarise(count = n()) %>%
    filter(!!desired_trait == value)
  
  # Group counts by site ID (this will be a template to overwrite)
  site_id_tibble <- counts_and_traits %>% 
    dplyr::group_by(Site_ID) %>% 
    dplyr::summarise(count = n())
  
  # Then mutate the counts to be those of n_taxa_by_family to
  # get a named vector of site IDs with the filtered family count
  #### LEH: When I run this outside of the function it seems that this does not retain the site ID information. 
  # Can we discuss whether or not this matter given the intended use of this function 
  site_id_taxa_by_trait <- site_id_tibble %>%
    dplyr::mutate(count = ifelse(Site_ID %in% n_taxa_by_trait$Site_ID, 
                                 n_taxa_by_trait$count,
                                 0)) %>%
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
  
  # See <https://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function> for more details
  # Create quosure to use 
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
    dplyr::summarise(count = n())
  
  # Then mutate the counts to be those of n_taxa_by_family to
  # get a named vector of site IDs with the filtered family count
  site_id_ind_by_trait <- site_id_tibble %>%
    dplyr::mutate(count = ifelse(Site_ID %in% n_ind_by_trait$Site_ID, 
                                 n_ind_by_trait$count,
                                 0)) %>%
    dplyr::select(x = count, nm = Site_ID) %>%
    purrr::pmap(set_names) %>% 
    unlist
  
  
  return(site_id_ind_by_trait)
}

J_evenness  <- function(x) {
  #' Creates additional metric called 'Eveness' based on diversity and species number from vegan package
  return(vegan::diversity(x)/log(specnumber(x)))
}

#  Load data
il_fish_traits <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Example/Example_Fish_Traits.csv", na ="")
fish_data <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Example/Example_Fish_Data.csv", row.names = 1, na = "")
fish_table <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Example/Example_Fish_Data_With_Traits.csv", na = "")

# Additional categories need to be calculated in trait data (This affects both il_fish_traits and fish_table)
# Can be removed for final calculations provided il_fish_traits has been updated
fish_table$HERBIVORE <- ifelse(fish_table$ALGPHYTO == '1',
                               1,
                               ifelse(fish_table$MACVASCU == '1',
                                      1,
                                      ifelse(fish_table$DETRITUS == '1',
                                             1,
                                             0
                                      )
                               )
)

il_fish_traits$HERBIVORE <- ifelse(il_fish_traits$ALGPHYTO == '1',
                                   1,
                                   ifelse(il_fish_traits$MACVASCU == '1',
                                          1,
                                          ifelse(il_fish_traits$DETRITUS == '1',
                                                 1,
                                                 0
                                          )
                                   )
)

fish_table$OMNIVORE <- ifelse((fish_table$HERBIVORE + fish_table$INVLVFSH + fish_table$FSHCRCRB + fish_table$BLOOD + fish_table$EGGS + fish_table$OTHER) >1,
                              1,
                              0
)

# Get Site Metric fields
INDIVIDUALS <- rowSums(fish_data)
RICHNESS <- rowSums(fish_data != 0) 
DIVERSITY <- vegan::diversity(fish_data, index = "shannon")
EVENNESS <- J_evenness(fish_data)

site_metric_tibble <- data.frame(INDIVIDUALS,RICHNESS,DIVERSITY,EVENNESS) %>%
  tibble::rownames_to_column(var = "Site_ID")

# Before we use the function at the top let's first learn about it by having RStudio display its docstring
# Remove this later
?num_taxa_by_trait
?num_ind_by_trait

# Add Desired Metrics to the base table called site_metric_tibble
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

site_metric_tibble$ALIENNTAX <- num_taxa_by_trait(fish_table, Nonnative, '1')
site_metric_tibble$ALIENPTAX <- round(site_metric_tibble$ALIENNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$ALIENNIND <- num_ind_by_trait(fish_table, Nonnative, '1')
site_metric_tibble$ALIENPIND <- round(site_metric_tibble$ALIENNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$TOLRNTAX <- num_taxa_by_trait(fish_table, Tolerance_Level, 'tolerant')
site_metric_tibble$TOLRPTAX <- round(site_metric_tibble$TOLRNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$TOLRNIND <- num_ind_by_trait(fish_table, Tolerance_Level, 'tolerant')
site_metric_tibble$TOLRPIND <- round(site_metric_tibble$TOLRNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$TOLRNTAX <- num_taxa_by_trait(fish_table, Tolerance_Level, 'tolerant')
site_metric_tibble$TOLRPTAX <- round(site_metric_tibble$TOLRNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$TOLRNIND <- num_ind_by_trait(fish_table, Tolerance_Level, 'tolerant')
site_metric_tibble$TOLRPIND <- round(site_metric_tibble$TOLRNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$SENSNTAX <- num_taxa_by_trait(fish_table, Tolerance_Level, 'sensitive')
site_metric_tibble$SENSPTAX <- round(site_metric_tibble$SENSNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$SENSNIND <- num_ind_by_trait(fish_table, Tolerance_Level, 'sensitive')
site_metric_tibble$SENSPIND <- round(site_metric_tibble$SENSNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

site_metric_tibble$INTOLNTAX <- num_taxa_by_trait(fish_table, Tolerance_Level, 'intermediate')
site_metric_tibble$INTOLPTAX <- round(site_metric_tibble$INTOLNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$INTOLNIND <- num_ind_by_trait(fish_table, Tolerance_Level, 'intermediate')
site_metric_tibble$INTOLPIND <- round(site_metric_tibble$INTOLNIND/site_metric_tibble$INDIVIDUALS, digits = 3)

## Missing DTOLTAX through MATUAGE. Different functions must be created for these types of metrics

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

site_metric_tibble$LITHNTAX <- (num_taxa_by_trait(fish_table, GRAVEL, '1') + 
                                    num_taxa_by_trait(fish_table, COBBLE, '1') + 
                                    num_taxa_by_trait(fish_table, BOULDER, '1')
                                )
site_metric_tibble$LITHPTAX <- round(site_metric_tibble$LITHNTAX/site_metric_tibble$RICHNESS, digits = 3)
site_metric_tibble$LITHNIND <- (num_ind_by_trait(fish_table, GRAVEL, '1') + 
                                    num_ind_by_trait(fish_table, COBBLE, '1') + 
                                    num_ind_by_trait(fish_table, BOULDER, '1')
                                )
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