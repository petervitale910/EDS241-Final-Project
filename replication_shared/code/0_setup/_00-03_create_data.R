#############################################################################################
# Plastic bag bans and fees reduce harmful bag litter on shorelines
# Anna Papp (ap3907@columbia.edu) and Kimberly Oremus 
# Script for creating data 
# last modified: 01/23/25
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## directory, update as needed
directory <- "/Users/"
setwd(paste0(directory, "plasticbag/replication/"))

## check - this should be the "replication" folder
getwd()

# Step 0: Clean-Up Data Processing -----------------------------------------------------------

# a
source("code/0_setup/00_cleanup/00a_cleanup_data_county.R",local = knitr::knit_global())

# b
source("code/0_setup/00_cleanup/00b_cleanup_data_zip.R",local = knitr::knit_global())

# c
source("code/0_setup/00_cleanup/00c_cleanup_data_cell.R",local = knitr::knit_global())

# d
source("code/0_setup/00_cleanup/00d_cleanup_data_clean.R",local = knitr::knit_global())

# e
source("code/0_setup/00_cleanup/00e_cleanup_distance.R",local = knitr::knit_global())

# f
source("code/0_setup/00_cleanup/00f_cleanup_final.R",local = knitr::knit_global())

# Step 1: Policy Data Processing ------------------------------------------------------------- 

# a
source("code/0_setup/01_policy/01a_policy_county_zip.R",local = knitr::knit_global())

# b
source("code/0_setup/01_policy/01b_zip_characteristics.R",local = knitr::knit_global())

# c
source("code/0_setup/01_policy/01c_neighbor_zip.R",local = knitr::knit_global())

# d
source("code/0_setup/01_policy/01d_neighbor_zip_policy.R",local = knitr::knit_global())

# Step 2: Merge Data ------------------------------------------------------------------------- 

# a
source("code/0_setup/02_merge/02a_merge.R",local = knitr::knit_global())

# Step 3: Entanglement Data ------------------------------------------------------------------

# a
source("code/0_setup/03_entanglement/03_cleanup_entanglement.R",local = knitr::knit_global())

