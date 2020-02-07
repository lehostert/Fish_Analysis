df <- fish_table
# desired_trait <- 'Family'
# value <- 'Catostomidae'
# desired_trait <- dplyr::enquo(desired_trait) 

# Filter out the family of interest
n_taxa_by_trait <- df %>% 
  dplyr::group_by(Site_ID, Family) %>% 
  dplyr::summarise(count = n()) %>%
  filter(Family == 'Catostomidae')

# # Get summary of family richness
# sum_n_taxa_by_trait <- df %>% 
#   dplyr::group_by(Site_ID, Family) %>% 
#   dplyr::summarise(count = n())


# Group counts by site ID (this will be a template to overwrite)
# In this form it gives the total number of entries per sites which should be the same as # of species but it does not check for unique()  
site_id_tibble <- df %>% 
  dplyr::group_by(Site_ID) %>% 
  dplyr::summarise(sp_count = n())

# Then mutate the counts to be those of n_taxa_by_family to
# get a named vector of site IDs with the filtered family count

### NEW
site_id_tibble$count <- n_taxa_by_trait$count[match(n_taxa_by_trait$Site_ID, site_id_tibble$Site_ID)]

site_id_taxa_by_trait <- site_id_tibble %>%
  replace_na(list(count = 0)) %>%
  dplyr::select(x = count, nm = Site_ID)%>%
  purrr::pmap(set_names) %>% 
  unlist
return(site_id_taxa_by_trait)

## TODO count int site_id_tibble becomes count.count then to replace NA it does not work. Fix this!

### Original
site_id_taxa_by_trait  <- site_id_tibble %>%
  dplyr::mutate(count = ifelse(Site_ID %in% n_taxa_by_trait$Site_ID, 
                               n_taxa_by_trait$count,
                               0)) %>%
  dplyr::select(x = count, nm = Site_ID) %>% 
  purrr::pmap(set_names) %>% 
  unlist
return(site_id_taxa_by_trait)



site_id_taxa_by_trait <- site_id_tibble %>% 
  left_join(n_taxa_by_trait, by = "Site_ID") %>% 
  replace_na(list(count = 0)) %>% 
  dplyr::select(x = count, nm = Site_ID) %>% 
  purrr::pmap(set_names) %>% 
  unlist

  



