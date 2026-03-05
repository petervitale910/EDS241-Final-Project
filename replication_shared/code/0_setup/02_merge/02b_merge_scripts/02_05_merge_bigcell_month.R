#############################################################################################
# Bigcell-month level final data combination 
# last modified: 08/13/24
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
load('data/processed/00_data_intermediate/00_data_bigcell_month.rda')

# spillover zip codes 
load('data/processed/01_zip_neighbors_policy.rda')

# Merge --------------------------------------------------------------------------------------

data <- left_join(data, merged %>% dplyr::select(zip, state, medianIncome, incQuartile, incQuintile, firstEffect, completeBagBan, partialBagBan, bagLimit, plasticBagCharge, paperBagCharge, paperBagBan, repealed, repealedDate, 
                                                                lastPolicyGeo, firstPolicyGeo, lastPolicyType, firstPolicyType, lastPolicyID , firstPolicyID, popTotal))
data <- data %>% mutate(zip = as.integer(zip))

# Check and fix multiple matches -------------------------------------------------------------

# observations with multiple zip matches for each cell
dataRep <- data %>% mutate(ind = 1) %>% group_by(groupID1, year, zip) %>% summarise(sum = sum(ind)) %>% ungroup()
dataRep <- dataRep %>% group_by(groupID1) %>% mutate(minZip = min(zip), maxZip = max(zip))
dataRepMultiple <- dataRep %>% filter(minZip != maxZip)
dataRepSingle <- dataRep %>% filter(minZip == maxZip)
dataSingle <- left_join(dataRepSingle, data)
dataMultiple <- left_join(dataRepMultiple, data) 
rm(dataRepMultiple, dataRepSingle)

# fix multiple matches, if there are multiple zip codes within a cell, take the first treated one 
dataMultiple <- dataMultiple %>% arrange(groupID1, firstEffect)
dataMultiple <- dataMultiple %>% group_by(groupID1) %>% mutate(zip = first(zip), county = first(county), firstEffect = first(firstEffect),
                                                               completeBagBan = first(completeBagBan), partialBagBan = first(partialBagBan), 
                                                               bagLimit = first(bagLimit), plasticBagCharge = first(plasticBagCharge), 
                                                               paperBagCharge = first(paperBagCharge), paperBagBan = first(paperBagBan), 
                                                               repealed = first(repealed), repealedDate = first(repealedDate), 
                                                               lastPolicyType=first(lastPolicyType), lastPolicyID = first(lastPolicyID), 
                                                               firstPolicyType=first(firstPolicyType), firstPolicyID = first(firstPolicyID), 
                                                               firstPolicyGeo=first(firstPolicyGeo), lastPolicyGeo = first(lastPolicyGeo)) %>% ungroup()
data <- rbind(dataMultiple, dataSingle) %>% dplyr::select(-c(minZip, maxZip))
data <- data %>% arrange(groupID1, year, month)
rm(dataSingle, dataMultiple)

# Process ------------------------------------------------------------------------------------

# neighbors 
data <- left_join(data, neighborZipFinal)
data <- data %>% mutate(neighborInd = ifelse(is.na(neighborInd), 0, neighborInd), 
                        neighborRepealedInd = ifelse(is.na(neighborRepealedInd), 0, neighborRepealedInd))

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
data <- data %>% mutate(treatedAny = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect, 1, 0), 
                        treatedComplete = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & completeBagBan == 1, 1, 0),
                        treatedPartial = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & partialBagBan == 1, 1, 0), 
                        treatedCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & plasticBagCharge == 1, 1, 0),
                        treatedSpillover1 =  ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > neighborFirstEffect & neighborInd == 1, 1, 0), 
                        treatedSpillover2 =  ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > neighborFirstEffect & neighborInd == 2, 1, 0), 
                        treatedSpillover3 =  ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > neighborFirstEffect & neighborInd == 3, 1, 0))

# recode NAs to 0
data <- data %>%
  mutate(across(
    starts_with("treated"),
    ~ifelse(is.na(.), 0, .)
  ))

# keep only 2016 - 2023, and areas treated beginning in 2017 
data <- data %>% filter(year >= 2016 & year <= 2023) %>% filter(year(firstEffect) >= 2017 | is.na(firstEffect))
data <- data %>% filter(!is.na(percPlasticBag)) 

# drop any CA (before 2016) or treated NY 
data <- data %>% filter(state != "CA") %>% filter(!(state == "NY" & type == "control"))

# Save --------------------------------------------------------------------------------------

save(data, file="data/processed/appendix/02_data_merged/02_merged_bigcell_month.rda")
