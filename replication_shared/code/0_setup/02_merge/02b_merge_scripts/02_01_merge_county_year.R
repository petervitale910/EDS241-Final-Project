#############################################################################################
# County-year level final data combination 
# last modified: 09/19/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr, lubridate)

# Load cleanups and policies  ---------------------------------------------------------------

# zip policy 
load("data/processed/01_data_intermediate/01_county_policy.rda")

# cleanups by zip x month 
# want to run for: all, coast, river, lake, other 
load('data/processed/00_data_intermediate/00_data_county_year.rda')

# Merge --------------------------------------------------------------------------------------

data <- left_join(data, county %>% dplyr::select(county, state, firstEffect, completeBagBan, partialBagBan, bagLimit, plasticBagCharge, paperBagCharge, paperBagBan, repealed, repealedDate))

# Check and fix multiple matches -------------------------------------------------------------

data <- data %>% group_by(county) %>% mutate(firstEffect = first(firstEffect),
                                             completeBagBan = first(completeBagBan), partialBagBan = first(partialBagBan), 
                                             bagLimit = first(bagLimit), plasticBagCharge = first(plasticBagCharge), 
                                             paperBagCharge = first(paperBagCharge), paperBagBan = first(paperBagBan), 
                                             repealed = first(repealed), repealedDate = first(repealedDate)) %>% ungroup()

# Process ------------------------------------------------------------------------------------

# policy type
data <- data %>% mutate(type = ifelse(completeBagBan == 1, "complete", 
                                      ifelse(partialBagBan == 1, "partial", 
                                             ifelse(plasticBagCharge == 1, "charge", NA))))
data <- data %>% mutate(type = ifelse(is.na(type), "control", type))
data <- data %>% mutate(type = ifelse(is.na(repealed) | repealed == 0, type,  "repealed"))

# all pooled policies 
data <- data %>% mutate(treatedAny = ifelse(as.Date(paste0(year, "-01-01"), format="%Y-%m-%d") > firstEffect, 1, 0), 
                        treatedComplete = ifelse(as.Date(paste0(year, "-01-01"), format="%Y-%m-%d") > firstEffect & completeBagBan == 1, 1, 0),
                        treatedPartial = ifelse(as.Date(paste0(year, "-01-01"), format="%Y-%m-%d") > firstEffect & partialBagBan == 1, 1, 0), 
                        treatedCharge = ifelse(as.Date(paste0(year, "-01-01"), format="%Y-%m-%d") > firstEffect & plasticBagCharge == 1, 1, 0))

# recode 
data <- data %>%
  mutate(across(
    starts_with("treated"),
    ~ifelse(is.na(.), 0, .)
  ))

#keep only 2016 - 2022, and areas treated beginning in 2017 
data <- data %>% filter(year >= 2016 & year <= 2023) %>% filter(year(firstEffect) >= 2017 | is.na(firstEffect))
data <- data %>% filter(!is.na(percPlasticBag)) 

# drop any CA (before 2016) or treated NY 
data <- data %>% filter(state != "CA") %>% filter(!(state == "NY" & type == "control"))

# Save --------------------------------------------------------------------------------------

save(data, file="data/processed/appendix/02_data_merged/02_merged_county_year.rda")





