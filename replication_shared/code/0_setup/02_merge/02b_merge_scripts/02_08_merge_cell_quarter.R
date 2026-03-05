#############################################################################################
# Cell-level code, quarter level final data combination 
# last modified: 07/19/23
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr, lubridate)

# Load cleanups and policies  ---------------------------------------------------------------

# zip policy 
load("data/processed/01_zip_policy_characteristics.rda")

# cleanups by zip x month 
# want to run for: all, coast, river, lake, other 
load('data/processed/00_data_intermediate/00_data_cell_quarter.rda')

# spillover zip codes 
load('data/processed/01_zip_neighbors_policy.rda')

# Merge --------------------------------------------------------------------------------------

data <- left_join(data, merged %>% dplyr::select(zip, state, medianIncome, incQuartile, incQuintile, firstEffect, completeBagBan, partialBagBan, bagLimit, plasticBagCharge, paperBagCharge, paperBagBan, repealed, repealedDate, 
                                                               lastPolicyGeo, firstPolicyGeo, lastPolicyType, firstPolicyType, lastPolicyID , firstPolicyID, popTotal))
data <- data %>% mutate(zip = as.integer(zip))

# Check and fix multiple matches -------------------------------------------------------------

# observations with multiple zip matches for each cell
dataRep <- data %>% mutate(ind = 1) %>% group_by(groupID01, year, zip) %>% summarise(sum = sum(ind)) %>% ungroup()
dataRep <- dataRep %>% group_by(groupID01) %>% mutate(minZip = min(zip), maxZip = max(zip))
dataRepMultiple <- dataRep %>% filter(minZip != maxZip)
dataRepSingle <- dataRep %>% filter(minZip == maxZip)
dataSingle <- left_join(dataRepSingle, data)
dataMultiple <- left_join(dataRepMultiple, data) 
rm(dataRepMultiple, dataRepSingle)

# fix multiple matches, if there are multiple zip codes within a cell, take the first treated one 
dataMultiple <- dataMultiple %>% arrange(groupID01, firstEffect)
dataMultiple <- dataMultiple %>% group_by(groupID01) %>% mutate(zip = first(zip), county = first(county), firstEffect = first(firstEffect),
                                                                completeBagBan = first(completeBagBan), partialBagBan = first(partialBagBan), 
                                                                bagLimit = first(bagLimit), plasticBagCharge = first(plasticBagCharge), 
                                                                paperBagCharge = first(paperBagCharge), paperBagBan = first(paperBagBan), 
                                                                repealed = first(repealed), repealedDate = first(repealedDate), 
                                                                lastPolicyType=first(lastPolicyType), lastPolicyID = first(lastPolicyID), 
                                                                firstPolicyType=first(firstPolicyType), firstPolicyID = first(firstPolicyID), 
                                                                firstPolicyGeo=first(firstPolicyGeo), lastPolicyGeo = first(lastPolicyGeo)) %>% ungroup()
data <- rbind(dataMultiple, dataSingle) %>% dplyr::select(-c(minZip, maxZip))
data <- data %>% arrange(groupID01, year, quarter)
rm(dataSingle, dataMultiple)

# Process ------------------------------------------------------------------------------------

# neighbors 
data <- left_join(data, neighborZipFinal)
data <- data %>% mutate(neighborInd = ifelse(is.na(neighborInd), 0, neighborInd), 
                        neighborRepealedInd = ifelse(is.na(neighborRepealedInd), 0, neighborRepealedInd))

# quarter date 
data <- data %>% mutate(quarterStartDate = ifelse(quarter == 0, as.Date(paste0(year-1, "-12-01"), format="%Y-%m-%d"), 
                                                  ifelse(quarter == 1, as.Date(paste0(year, "-03-01"), format="%Y-%m-%d"),
                                                         ifelse(quarter == 2, as.Date(paste0(year, "-06-01"), format="%Y-%m-%d"),
                                                                as.Date(paste0(year, "-09-01"), format="%Y-%m-%d")))))
data <- data %>% mutate(quarterStartDate = as.Date(quarterStartDate, format="%Y-%m-%d", origin="1970-01-01"))

# policy type
data <- data %>% mutate(type = ifelse(completeBagBan == 1, "complete", 
                                      ifelse(partialBagBan == 1, "partial", 
                                             ifelse(plasticBagCharge == 1, "charge", NA))))
data <- data %>% mutate(type = ifelse(is.na(type), "control", type))
data <- data %>% mutate(type = ifelse(is.na(repealed) | repealed == 0, type,  "repealed"))
data <- data %>% mutate(type = ifelse(neighborInd == 1, "controlSpillover1", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 1, "controlRepealSpillover1", type))
data <- data %>% mutate(type = ifelse(neighborInd == 2, "controlSpillover2", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 2, "controlRepealSpillover2", type))
data <- data %>% mutate(type = ifelse(neighborInd == 3, "controlSpillover3", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 3, "controlRepealSpillover3", type))

# all pooled policies 
data <- data %>% mutate(treatedAny = ifelse(quarterStartDate > firstEffect, 1, 0), 
                        treatedComplete = ifelse(quarterStartDate > firstEffect & completeBagBan == 1, 1, 0),
                        treatedPartial = ifelse(quarterStartDate > firstEffect & partialBagBan == 1, 1, 0), 
                        treatedCharge = ifelse(quarterStartDate > firstEffect & plasticBagCharge == 1, 1, 0),
                        treatedSpillover1 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 1, 1, 0), 
                        treatedSpillover2 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 2, 1, 0), 
                        treatedSpillover3 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 3, 1, 0))

# recode NAs to 0
data <- data %>%
  mutate(across(
    starts_with("treated"),
    ~ifelse(is.na(.), 0, .)
  ))

# keep only 2016 - 2022, and areas treated beginning in 2017 
data <- data %>% filter(year >= 2016 & year <= 2023) %>% filter(year(firstEffect) >= 2017 | is.na(firstEffect))
data <- data %>% filter(!is.na(percPlasticBag)) 

# drop any CA (before 2016) or treated NY 
data <- data %>% filter(state != "CA") %>% filter(!(state == "NY" & type == "control"))

# Save --------------------------------------------------------------------------------------

save(data, file="data/processed/appendix/02_data_merged/02_merged_cell_quarter.rda")
