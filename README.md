# Fish_Analysis

## Purpose
## Scripts
###Fish_Trait_Matrix_Creation.R
Script to combine date from
- VT Fish traits Frimpong 2008
- USGS Tolerance Data
Meador and Carlisle 2007

It creates the IL fish traits file that covers the species specific trait information of all fish species in IL. The output file known as illinois_fish_traits.csv is still missing tolerance class values for  the following species BGB, BUD, CAP, DUD, FRM, MUD, ORD, RDS, SES, SHD, SLD, SLM, SRD, SUM, SVS, WHS, YLB. The tolerance values are necessary for the next step of the process so these values were filled in by hand with values from data directly sent to Dr. Yong Cao from Dr. M.R. Meador.

The final complete dataset is known as illinois_fish_traits_complete.csv

###Per_Site_Fish_Metric_Tibble.R
This script reads in
## Data
