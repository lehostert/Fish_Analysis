df <- fish_table
desired_trait <- 'Family'
value <- 'Catostomidae'
desired_trait <- dplyr::enquo(desired_trait) 

# Filter out the family of interest
n_taxa_by_trait <- df %>% 
  dplyr::group_by(Site_ID, Family) %>% 
  dplyr::summarise(count = n()) %>%
  filter(Family == 'Catostomidae')

# Group counts by site ID (this will be a template to overwrite)
site_id_tibble <- df %>% 
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