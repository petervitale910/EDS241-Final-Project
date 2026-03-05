#############################################################################################
# Distance to coast, rivers, and lakes 
# last modified: 09/08/24 (update with NHD measurements)
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr)

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")

# Load distance to rivers and coasts  ------------------------------------------------------

## Note: these distances are calculated in Google Earth Engine 
## https://code.earthengine.google.com/559bb1ca7244c3ad508f8c918d4eba50

coast <- read.csv("data/other/coast_distance.csv") %>% dplyr::select(id, distanceCoast = distance) %>% mutate(id = as.character(id))
rivers <- read.csv("data/other/river_distance.csv") %>% dplyr::select(id, distanceRiver = distance) %>% mutate(id = as.character(id))
lakes <- read.csv("data/other/lake_distance.csv") %>% dplyr::select(id, distanceLake = distance) %>% mutate(id = as.character(id))

## Note: watershed calculated in Google Earth Engine 
## https://code.earthengine.google.com/f8b133c09418f606c6a3c63aacd9805b
watershed <- read.csv("data/other/cleanups_in_watersheds.csv") %>% dplyr::select(id, inDrainage = inDataset) %>% mutate(id = as.character(id))

# Merge   -----------------------------------------------------------------------------------

# merge with distance information
cleanup <- left_join(cleanup, coast)
cleanup <- left_join(cleanup, rivers)
cleanup <- left_join(cleanup, lakes)
cleanup <- left_join(cleanup, watershed)

# find closest waterbody 
cleanup <- cleanup %>% mutate(distanceLake = ifelse(is.na(distanceLake), 100000, distanceLake), 
                              distanceRiver = ifelse(is.na(distanceRiver),10000, distanceRiver), 
                              inDrainage = ifelse(is.na(inDrainage), 0, inDrainage))
cleanup <- cleanup %>% mutate(closestWater = ifelse(distanceCoast <= distanceRiver & distanceCoast <= distanceLake, 0, 
                                                    ifelse(distanceRiver < distanceCoast & distanceRiver <= distanceLake, 1, 
                                                        ifelse(distanceLake < distanceCoast & distanceLake < distanceRiver, 2, NA))))

# create indicators for ocean, lake, and river cleanups 
cleanup <- cleanup %>% mutate(coastInd = ifelse(closestWater == 0 & distanceCoast < 1000, 1, 0), 
                              riverInd = ifelse(closestWater == 1 & distanceRiver < 1000, 1, 0), 
                              lakeInd = ifelse(closestWater == 2 & distanceLake < 1000, 1, 0))
cleanup <- cleanup %>% mutate(coastInd = ifelse(coastInd == 0 & riverInd == 0 & lakeInd == 0 & zip == 99660, 1, coastInd))
cleanup <- cleanup %>% mutate(otherInd = ifelse(coastInd == 0 & riverInd == 0 & lakeInd == 0, 1, 0))
cleanup <- cleanup %>% mutate(within5km = ifelse((coastInd == 1 | riverInd == 1) & inDrainage == 1 & distanceCoast < 5000, 1, 0))
cleanup <- cleanup %>% mutate(within10km = ifelse((coastInd == 1 | riverInd == 1) & inDrainage == 1 & distanceCoast < 10000, 1, 0))
cleanup <- cleanup %>% mutate(within50km = ifelse((coastInd == 1 | riverInd == 1) & inDrainage == 1 & distanceCoast < 50000, 1, 0))
cleanup <- cleanup %>% mutate(within100km = ifelse((coastInd == 1 | riverInd == 1) & inDrainage == 1 & distanceCoast < 100000, 1, 0))
cleanup <- cleanup %>% mutate(riverWithin10km = ifelse((riverInd == 1) & inDrainage == 1 & distanceCoast < 10000, 1, 0))
cleanup <- cleanup %>% mutate(lakeWithin10km = ifelse((lakeInd == 1) & inDrainage == 1 & distanceCoast < 10000, 1, 0))

# save 
save(cleanup, file="data/processed/00_data_cleanup.rda")

# count various types (for descriptions in text)
sum(cleanup$coastInd)/nrow(cleanup)
sum(cleanup$riverInd)/nrow(cleanup)
sum(cleanup$lakeInd)/nrow(cleanup)
sum(cleanup$otherInd)/nrow(cleanup)
sum(cleanup$within5km)/nrow(cleanup)
sum(cleanup$within10km)/nrow(cleanup)
sum(cleanup$within50km)/nrow(cleanup)
sum(cleanup$within100km)/nrow(cleanup)
sum(cleanup$riverWithin10km)/nrow(cleanup)
sum(cleanup$lakeWithin10km)/nrow(cleanup)
