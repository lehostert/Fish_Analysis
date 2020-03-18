# Fish_Analysis

## Purpose
## Scripts

### Data Preparation and Computation
#### Fish_Trait_Matrix_Creation.R
Script to combine data from
- VT Fish traits Frimpong 2008
- USGS Tolerance Data
- Meador and Carlisle 2007

It creates the IL fish traits file that covers the species specific trait information of all fish species in IL. The output file known as illinois_fish_traits.csv is still missing tolerance class values for  the following species BGB, BUD, CAP, DUD, FRM, MUD, ORD, RDS, RSF, SES, SHD, SLD, SLM, SRD, SUM, SVS, WHS, YLB. The tolerance values are necessary for the next step of the process so these values were filled in by hand with values from data directly sent to Dr. Yong Cao from Dr. M.R. Meador.

The final complete dataset is known as illinois_fish_traits_complete.csv

#### Per_Site_Fish_Metric_Tibble.R
This script reads in fish data and produces a dataframe with 110 computed fish metrics. The script relies on the output from Fish_Trait_Matrix_Creation.R (illinois_fish_traits_complete.csv) to be able to calculate the metrics. 

#### landuse_geology_metric_tibble.R
This is a generic scrip that is the base code for simplifying landuse and geology metrics that come from the 2010 Aquatic GAP Analysis at all available scales (C- channel, R- local riparian, RT-riparian total, W- local watershed, WT- total watershed). The following scripts are scale specifici variations of the this generic script. 
 -C_landuse_geology_metric_tibble.R
 -R_landuse_geology_metric_tibble.R
 -RT_landuse_geology_metric_tibble.R
 -W_landuse_geology_metric_tibble.R
 -WT_landuse_geology_metric_tibble.R

#### landuse_metric_binder.R
This script reads in landuse and geology data from mulitple Scales (C- channel, R- local riparian, RT-riparian total, W- local watershed, WT- total watershed) along with watershed level area data (CRP/CREP % and highly erodable lands) and produces a dataframe with 67 landscape and geology level variables for each PU in the Kaskaskia River Basin. The resulting dataframe are known as landuse geology metrics. 

#### fish-landscape-metric-matrix.R
 This script combines the fish trait metric tibble with the landuse geology metrics for use in a random forest analysis
 
### Analysis

#### rf-analysis-all-fish-landscape.R
Script runs a random forest iteratively with all fish variables as response variables and all landuse/geology variables as the predictor variables, using mtry 1-8. The purpose of this is to determine the lowest mtry for which the r squared value is maximized. Based on a <1% change for the next mtry.

#### rf-analysis-fish-landscape-best-mtry.R
This script takes in the fish metrics list used in the all-fish-landscape rf analysis and the best mtry as determined by the outcome of the saem script. It also 
looks at the outcome of the RF with the best mtry to rank all of the landscape level variables by how frequnetly they are in the top 10 predictor variables (as ranked by percent increase in MSE)

#### rf-analysis-summary-stats.R
Gets some summary stats from the RFs. 

## Data

1. CREP fish community samples from 2013-2019 in the Kaskaskia River Basin
2. ~~ IDNR Basin Surveys from 1992-2007 Sourced from JStein Database ~~
2. IDNR Kaskaskia Community Basin Surveys from 1992-2007 Sourced from YCao. Used in LDrake MS thesis. 
3. Aquatic Gap Analysis 2010 Landuse and Geology Data (Watershed Unit Areas are also based on PU Gap Analysis)
4. Highly Erodible Lands GIS data
5. CREP & CRP GIS data