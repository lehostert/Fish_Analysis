# Script for Analysis of CREP Fish Count Data NMDS by YEAR

# Begin by running Per_Site_Fish_Metric_Tibble.R Script this will create needed data and load applicable packages.

## MDS
# Now create row name in Site_Metric_Tibble data set for MDS analysis. Create row.names from Site_ID 

fish_metrics <- site_metric_tibble
row.names(fish_metrics) <- fish_metrics$Site_ID
fish_metrics <- fish_metrics %>% select(-c(Site_ID))

# BEWARE Replaces all NA and NaN values in dataframe
# Purpose is to replace two Evenness values of 'NaN' in the fish_metrics data frame with '0' byt beware it will replace all. 
fish_metrics[is.na(fish_metrics)] <- 0

# Removes rows with incomplete cases
# fish_metrics <- na.omit(fish_metrics)

#######################################
## Log transform the data so that any outlying values are standarized.
# fish_metrics.log <- log(fish_metrics+1)

# Global NMDS using metaMDS
fish_metrics.MDS <- metaMDS(fish_metrics)
fish_metrics.MDS
plot(fish_metrics.MDS, type="t", main = "Fish MDS Bray-Curtis (stress = 0.152)")

## Start from previous best solution
fish_metrics.MDS.best <- metaMDS(fish_metrics, previous.best = fish_metrics.MDS)
fish_metrics.MDS.best
plot(fish_metrics.MDS.best, type="t", main = "Fish MDS Bray-Curtis Best (stress = 0.152)")

# fish_metrics.MDS.euc <- metaMDS(fish_metrics.log, distance = "euclidean")
# fish_metrics.MDS.euc 
# plot(fish_metrics.MDS.euc, type="t", main = "Fish MDS Euclidean (stress= 0.164)")

#### The black clouds are clusters are sites that are all in the same streams
## The species names in the center of the plot are speces that are similarly found 
## in all of the streams, more balanced. The species on the edges are close to sites that are
## close to them 

fish.BC <- vegdist(fish_metrics, method = "bray")
fish.cluster <- hclust(fish.BC, method = "complete", members = NULL)
plot(fish.cluster, type="t", main = "Fish Cluster BC Complete")


#####################################
# Separate data by year
# 2018
fish_2018 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2018')) 

row.names(fish_2018) <- fish_2018$Site_ID
fish_2018 <- fish_2018 %>% select(-c(Site_ID)) 
# %>% na.omit()

fish_2018.MDS <- metaMDS(fish_2018)
fish_2018.MDS
plot(fish_2018.MDS, type="t", main = "2018 Fish MDS Bray-Curtis (stress = 0.11)")

fish_2018.BC <- vegdist(fish_2018, method = "bray")
fish_2018.cluster <- hclust(fish_2018.BC, method = "complete", members = NULL)
plot(fish_2018.cluster, main = "2018 Fish Cluster BC Complete")

#2017
fish_2017 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2017')) 

row.names(fish_2017) <- fish_2017$Site_ID
fish_2017 <- fish_2017 %>% select(-c(Site_ID))

fish_2017.MDS <- metaMDS(fish_2017)
fish_2017.MDS
plot(fish_2017.MDS, type="t", main = "2017 Fish MDS Bray-Curtis (stress = 0.146)") 

fish_2017.BC <- vegdist(fish_2017, method = "bray")
fish_2017.cluster <- hclust(fish_2017.BC, method = "complete", members = NULL)
plot(fish_2017.cluster, type="t", main = "2017 Fish Cluster BC Complete")

#2016
fish_2016 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2016')) 

row.names(fish_2016) <- fish_2016$Site_ID
fish_2016 <- fish_2016 %>% select(-c(Site_ID))

fish_2016.MDS <- metaMDS(fish_2016)
fish_2016.MDS
plot(fish_2016.MDS, type="t", main = "2016 Fish MDS Bray-Curtis (stress = 0.172)") 

fish_2016.BC <- vegdist(fish_2016, method = "bray")
fish_2016.cluster <- hclust(fish_2016.BC, method = "complete", members = NULL)
plot(fish_2016.cluster, type="t", main = "2016 Fish Cluster BC Complete")

#2015
fish_2015 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2015')) 

row.names(fish_2015) <- fish_2015$Site_ID
fish_2015 <- fish_2015 %>% select(-c(Site_ID))

fish_2015.MDS <- metaMDS(fish_2015)
fish_2015.MDS
plot(fish_2015.MDS, type="t", main = "2015 Fish MDS Bray-Curtis (stress = 0.143)") 

fish_2015.BC <- vegdist(fish_2015, method = "bray")
fish_2015.cluster <- hclust(fish_2015.BC, method = "complete", members = NULL)
plot(fish_2015.cluster, type="t", main = "2015 Fish Cluster BC Complete")

#2014
fish_2014 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2014')) 

row.names(fish_2014) <- fish_2014$Site_ID
fish_2014 <- fish_2014 %>% select(-c(Site_ID))

fish_2014.MDS <- metaMDS(fish_2014)
fish_2014.MDS
plot(fish_2014.MDS, type="t", main = "2014 Fish MDS Bray-Curtis (stress = 0.201)") 

fish_2014.BC <- vegdist(fish_2014, method = "bray")
fish_2014.cluster <- hclust(fish_2014.BC, method = "complete", members = NULL)
plot(fish_2014.cluster, type="t", main = "2014 Fish Cluster BC Complete")

#2013
fish_2013 <- site_metric_tibble %>%
  filter(str_detect(Site_ID, '_2013')) 

row.names(fish_2013) <- fish_2013$Site_ID
fish_2013 <- fish_2013 %>% select(-c(Site_ID))

fish_2013.MDS <- metaMDS(fish_2013)
fish_2013.MDS
plot(fish_2013.MDS, type="t", main = "2013 Fish MDS Bray-Curtis (stress = 0.119)") 

fish_2013.BC <- vegdist(fish_2013, method = "bray")
fish_2013.cluster <- hclust(fish_2013.BC, method = "complete", members = NULL)
plot(fish_2013.cluster, type="t", main = "2013 Fish Cluster BC Complete")

## Save Your Plots
file_out <- "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_MDS_metrics/MDS_metrics_by_year.pdf"
pdf(file_out)
plot(fish_metrics.MDS.best, type="t", main = "Fish MDS Bray-Curtis Best (stress = 0.173)")
plot(fish.cluster, main = "Fish Cluster BC Complete")
plot(fish_2018.MDS, type="t", main = "2018 Fish MDS Bray-Curtis (stress = 0.200)")
plot(fish_2018.cluster, main = "2018 Fish Cluster BC Complete")
plot(fish_2017.MDS, type="t", main = "2017 Fish MDS Bray-Curtis (stress = 0.156)") 
plot(fish_2017.cluster, main = "2017 Fish Cluster BC Complete")
plot(fish_2016.MDS, type="t", main = "2016 Fish MDS Bray-Curtis (stress = 0.172)") 
plot(fish_2016.cluster, main = "2016 Fish Cluster BC Complete")
plot(fish_2015.MDS, type="t", main = "2015 Fish MDS Bray-Curtis (stress = 0.143)") 
plot(fish_2015.cluster, main = "2015 Fish Cluster BC Complete")
plot(fish_2014.MDS, type="t", main = "2014 Fish MDS Bray-Curtis (stress = 0.201)") 
plot(fish_2014.cluster, main = "2014 Fish Cluster BC Complete")
plot(fish_2013.MDS, type="t", main = "2013 Fish MDS Bray-Curtis (stress = 0.119)") 
plot(fish_2013.cluster, main = "2013 Fish Cluster BC Complete")
dev.off()