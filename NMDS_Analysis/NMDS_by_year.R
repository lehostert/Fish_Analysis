# Script for Analysis of CREP Fish Count Data NMDS by YEAR

# Begin by running Per_Site_Fish_Metric_Tibble.R Script this will create needed data and load applicable packages.

## MDS
# Now create sparse data set for MDS analysis. Create row.names from Site_ID 

sparse_fish_data <- fish_data %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
row.names(sparse_fish_data) <- sparse_fish_data$Site_ID
sparse_fish_data <- sparse_fish_data %>% select(-c(Site_ID))

#######################################
## Log transform the data so that any outlying values are standarized.
sparse_fish.log <- log(sparse_fish_data+1)
# Global NMDS using metaMDS
sparse_fish.MDS <- metaMDS(sparse_fish.log)
sparse_fish.MDS
plot(sparse_fish.MDS, type="t", main = "Fish MDS Bray-Curtis (stress = 0.219)")
## Start from previous best solution
sparse_fish.MDS.best <- metaMDS(sparse_fish.log, previous.best = sparse_fish.MDS)
sparse_fish.MDS.best
plot(sparse_fish.MDS.best, type="t", main = "Fish MDS Bray-Curtis Best (stress = 0.219)")

# sparse_fish.MDS.euc <- metaMDS(sparse_fish.log, distance = "euclidean")
# sparse_fish.MDS.euc 
# plot(sparse_fish.MDS.euc, type="t", main = "Fish MDS Euclidean (stress= 0.164)")

#### The black clouds are clusters are sites that are all in the same streams
## The species names in the center of the plot are speces that are similarly found 
## in all of the streams, more balanced. The species on the edges are close to sites that are
## close to them 

fish.BC <- vegdist(sparse_fish.log, method = "bray")
fish.cluster <- hclust(fish.BC, method = "complete", members = NULL)
plot(fish.cluster, type="t", main = "Fish Cluster BC Complete")


#####################################
# Separate data by year
# 2018
fish_2018 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2018') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2018) <- fish_2018$Site_ID
fish_2018 <- fish_2018 %>% select(-c(Site_ID))

fish_2018.log <- log(fish_2018+1)

fish_2018.MDS <- metaMDS(fish_2018.log)
fish_2018.MDS
plot(fish_2018.MDS, type="t", main = "2018 Fish MDS Bray-Curtis (stress = 0.200)")

fish_2018.BC <- vegdist(fish_2018.log, method = "bray")
fish_2018.cluster <- hclust(fish_2018.BC, method = "complete", members = NULL)
plot(fish_2018.cluster, type="t", main = "2018 Fish Cluster BC Complete")

#2017
fish_2017 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2017') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2017) <- fish_2017$Site_ID
fish_2017 <- fish_2017 %>% select(-c(Site_ID))

fish_2017.log <- log(fish_2017+1)

fish_2017.MDS <- metaMDS(fish_2017.log)
fish_2017.MDS
plot(fish_2017.MDS, type="t", main = "2017 Fish MDS Bray-Curtis (stress = 0.231)") 

fish_2017.BC <- vegdist(fish_2017.log, method = "bray")
fish_2017.cluster <- hclust(fish_2017.BC, method = "complete", members = NULL)
plot(fish_2017.cluster, type="t", main = "2017 Fish Cluster BC Complete")

#2016
fish_2016 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2016') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2016) <- fish_2016$Site_ID
fish_2016 <- fish_2016 %>% select(-c(Site_ID))

fish_2016.log <- log(fish_2016+1)

fish_2016.MDS <- metaMDS(fish_2016.log)
fish_2016.MDS
plot(fish_2016.MDS, type="t", main = "2016 Fish MDS Bray-Curtis (stress = 0.186)") 

fish_2016.BC <- vegdist(fish_2016.log, method = "bray")
fish_2016.cluster <- hclust(fish_2016.BC, method = "complete", members = NULL)
plot(fish_2016.cluster, type="t", main = "2016 Fish Cluster BC Complete")

#2015
fish_2015 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2015') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2015) <- fish_2015$Site_ID
fish_2015 <- fish_2015 %>% select(-c(Site_ID))

fish_2015.log <- log(fish_2015+1)

fish_2015.MDS <- metaMDS(fish_2015.log)
fish_2015.MDS
plot(fish_2015.MDS, type="t", main = "2015 Fish MDS Bray-Curtis (stress = 0.159)") 

fish_2015.BC <- vegdist(fish_2015.log, method = "bray")
fish_2015.cluster <- hclust(fish_2015.BC, method = "complete", members = NULL)
plot(fish_2015.cluster, type="t", main = "2015 Fish Cluster BC Complete")

#2014
fish_2014 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2014') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2014) <- fish_2014$Site_ID
fish_2014 <- fish_2014 %>% select(-c(Site_ID))

fish_2014.log <- log(fish_2014+1)

fish_2014.MDS <- metaMDS(fish_2014.log)
fish_2014.MDS
plot(fish_2014.MDS, type="t", main = "2014 Fish MDS Bray-Curtis (stress = 0.137)") 

fish_2014.BC <- vegdist(fish_2014.log, method = "bray")
fish_2014.cluster <- hclust(fish_2014.BC, method = "complete", members = NULL)
plot(fish_2014.cluster, type="t", main = "2014 Fish Cluster BC Complete")

#2013
fish_2013 <- fish_data %>%
  dplyr::filter(lubridate::year(Event_Date) == '2013') %>%
  select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0) 

row.names(fish_2013) <- fish_2013$Site_ID
fish_2013 <- fish_2013 %>% select(-c(Site_ID))

fish_2013.log <- log(fish_2013+1)

fish_2013.MDS <- metaMDS(fish_2013.log)
fish_2013.MDS
plot(fish_2013.MDS, type="t", main = "2013 Fish MDS Bray-Curtis (stress = 0.185)") 

fish_2013.BC <- vegdist(fish_2013.log, method = "bray")
fish_2013.cluster <- hclust(fish_2013.BC, method = "complete", members = NULL)
plot(fish_2013.cluster, type="t", main = "2013 Fish Cluster BC Complete")

## Save Your Plots
  file_out <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_MDS_counts/MDS_counts_by_year.pdf"
  pdf(file_out)
  plot(sparse_fish.MDS.best, type="t", main = "Fish MDS Bray-Curtis Best (stress = 0.219)")
  plot(fish.cluster, main = "Fish Cluster BC Complete")
  plot(fish_2018.MDS, type="t", main = "2018 Fish MDS Bray-Curtis (stress = 0.200)")
  plot(fish_2018.cluster, main = "2018 Fish Cluster BC Complete")
  plot(fish_2017.MDS, type="t", main = "2017 Fish MDS Bray-Curtis (stress = 0.231)")
  plot(fish_2017.cluster, main = "2017 Fish Cluster BC Complete")
  plot(fish_2016.MDS, type="t", main = "2016 Fish MDS Bray-Curtis (stress = 0.186)") 
  plot(fish_2016.cluster, main = "2016 Fish Cluster BC Complete")
  plot(fish_2015.MDS, type="t", main = "2015 Fish MDS Bray-Curtis (stress = 0.159)") 
  plot(fish_2015.cluster, main = "2015 Fish Cluster BC Complete")
  plot(fish_2014.MDS, type="t", main = "2014 Fish MDS Bray-Curtis (stress = 0.137)") 
  plot(fish_2014.cluster, main = "2014 Fish Cluster BC Complete")
  plot(fish_2013.MDS, type="t", main = "2013 Fish MDS Bray-Curtis (stress = 0.185)") 
  plot(fish_2013.cluster, main = "2013 Fish Cluster BC Complete")
  dev.off()