#############################################################################################
# Process policies and link to counties and zips
# last modified: 08/14/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr, readxl, lubridate, sf)

# Load zip code and county data ---------------------------------------------------------------

# list of US counties 
county <- read.csv("data/other/uscounties.csv")

# list of us zip codes 
zipList <- read.csv("data/other/uszipcodes.csv")
zipList <- zipList %>% filter(decommissioned == 0)
zipList <- zipList %>% filter(state != "PR" & state != "VI")
zipList <- zipList %>% dplyr::select(zip, type, city = primary_city, state, county, lat = latitude, lon = longitude, popIRS = irs_estimated_population)

# zip code shapefile (Tiger 2019)
zipSf <- st_read("data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") 
zipSf <- data.frame(zipSf) %>% dplyr::select(zip = ZCTA5CE10)
zipSf <- zipSf %>% mutate(zip = as.integer(zip))

# merge two zip files, based on shapefile 
zip <- left_join(zipSf, zipList)
rm(zipList, zipSf)

# Load policy data  --------------------------------------------------------------------------

# Town-level data 
towns <- read_excel("data/policies/policies.xlsx", sheet = "policies_towns")
towns <- towns %>% rename(city = jurisdiction)
towns <- towns %>% group_by(state, city) %>% mutate(policyID = paste0("town_", cur_group_id())) %>% ungroup()
towns <- towns %>% filter(year(effect) < 2024)

townCount <- towns[order(towns$effect),]
townCount <- townCount %>% group_by(state, city) %>% summarise(complete_bag_ban = last(complete_bag_ban), 
                                                               partial_bag_ban = last(partial_bag_ban), 
                                                               plastic_bag_charge = last(plastic_bag_charge), 
                                                               paper_bag_charge = last(paper_bag_charge))
nrow(townCount %>% filter(complete_bag_ban == 1) %>% distinct(city, state))
nrow(townCount %>% filter(partial_bag_ban == 1) %>% distinct(city, state))
nrow(townCount %>% filter(plastic_bag_charge == 1) %>% distinct(city, state))
nrow(townCount %>% distinct(city, state))

townsMerged <- left_join(towns, zip)
townsMerged <- townsMerged %>% arrange(state, city)
townsMerged <- townsMerged %>% dplyr::select(state, zip, city, county, effect, description, complete_bag_ban, partial_bag_ban, bag_limit, plastic_bag_charge, paper_bag_charge, reusable_bag_charge, paper_bag_ban, repealed, repealed_date, lat, lon, popIRS, policyID)
townsMerged <- townsMerged %>% mutate(policyType = "town")

# County-level data 
counties <- read_excel("data/policies/policies.xlsx", sheet = "policies_counties")
counties <- counties %>% filter(state != "") %>% rename(county = jurisdiction)
counties <- counties %>% group_by(state, county) %>% mutate(policyID = paste0("county_", cur_group_id())) %>% ungroup()
counties <- counties %>% filter(year(effect) < 2024)

countiesCount <- counties[order(counties$effect),]
countiesCount <- countiesCount %>% group_by(state, county) %>% summarise(complete_bag_ban = last(complete_bag_ban), 
                                                               partial_bag_ban = last(partial_bag_ban), 
                                                               plastic_bag_charge = last(plastic_bag_charge), 
                                                               paper_bag_charge = last(paper_bag_charge))
nrow(countiesCount %>% filter(complete_bag_ban == 1) %>% distinct(county))
nrow(countiesCount %>% filter(partial_bag_ban == 1) %>% distinct(county))
nrow(countiesCount %>% filter(plastic_bag_charge == 1) %>% distinct(county))
nrow(countiesCount %>% distinct(county))

countiesMerged <- left_join(counties, zip)
countiesMerged <- countiesMerged %>% dplyr::select(state, zip, city, county, effect, description, complete_bag_ban, partial_bag_ban, bag_limit, plastic_bag_charge, paper_bag_charge, reusable_bag_charge, paper_bag_ban, repealed, repealed_date, lat, lon, popIRS, policyID)
countiesMerged <- countiesMerged %>% mutate(policyType = "county")

# State-level data 
states <- read_excel("data/policies/policies.xlsx", sheet = "policies_states")
states <- states %>% dplyr::select(-c(jurisdiction))
states <- states %>% group_by(state) %>% mutate(policyID = paste0("state_", cur_group_id())) %>% ungroup()
states <- states %>% filter(year(effect) < 2024)

statesCount <- states[order(states$effect),]
statesCount <- statesCount %>% group_by(state) %>% summarise(complete_bag_ban = last(complete_bag_ban), 
                                                                         partial_bag_ban = last(partial_bag_ban), 
                                                                         plastic_bag_charge = last(plastic_bag_charge), 
                                                                         paper_bag_charge = last(paper_bag_charge))
nrow(statesCount %>% filter(complete_bag_ban == 1) %>% distinct(state))
nrow(statesCount %>% filter(partial_bag_ban == 1) %>% distinct(state))
nrow(statesCount %>% filter(plastic_bag_charge == 1) %>% distinct(state))
nrow(statesCount %>% distinct(state))

statesMerged <- left_join(states, zip)
statesMerged <- statesMerged %>% dplyr::select(state, zip, city, county, effect, description, complete_bag_ban, partial_bag_ban, bag_limit, plastic_bag_charge, paper_bag_charge, reusable_bag_charge = reusable_bag_tax, paper_bag_ban, repealed, repealed_date, lat, lon, popIRS, policyID)
statesMerged <- statesMerged %>% mutate(policyType = "state")

# Combine policy data  ------------------------------------------------------------------------

zipPolicies <- rbind(townsMerged, countiesMerged, statesMerged)
zipPolicies <- zipPolicies %>% mutate(effect = as.Date(effect, format="%m/%d/%y"), repealed_date = as.Date(repealed_date, format="%m/%d/%y"))
zipPolicies <- zipPolicies %>% arrange(zip, effect)
rm(townsMerged, countiesMerged, statesMerged)

# each zip code x policy combination 
zipPoliciesYear <- zipPolicies %>% filter(repealed != 1) %>% distinct(policyID, zip, effect, policyType, popIRS, complete_bag_ban, partial_bag_ban, plastic_bag_charge, policyType)
save(zipPoliciesYear, file="data/processed/01_zip_policy_distinct.rda")

zipPoliciesCollapse <- zipPolicies %>% group_by(state, zip, city, county) %>% summarise(firstEffect = first(effect), 
                                                                                        firstPolicyGeo = first(policyType),
                                                                                        firstPolicyID = first(policyID),
                                                                                        lastPolicyGeo = last(policyType),
                                                                                        lastPolicyID = last(policyID),
                                                                                        description = last(description), 
                                                                                        completeBagBan = last(complete_bag_ban), 
                                                                                        partialBagBan = last(partial_bag_ban), 
                                                                                        plasticBagCharge = last(plastic_bag_charge), 
                                                                                        firstCompleteBagBan = first(complete_bag_ban), 
                                                                                        firstPartialBagBan = first(partial_bag_ban), 
                                                                                        firstPlasticBagCharge = first(plastic_bag_charge), 
                                                                                        bagLimit = last(bag_limit), 
                                                                                        paperBagCharge = last(paper_bag_charge), 
                                                                                        paperBagBan = last(paper_bag_ban), 
                                                                                        repealed = first(repealed), 
                                                                                        repealedDate = first(repealed_date), 
                                                                                        lat = mean(lat), 
                                                                                        lon = mean(lon),
                                                                                        popIRS = mean(popIRS))
zipPoliciesCollapse <- zipPoliciesCollapse %>% mutate(firstPolicyType = ifelse(firstCompleteBagBan == 1, "complete_bag_ban", 
                                                                             ifelse(firstPartialBagBan == 1, "partial_bag_ban", 
                                                                                    ifelse(firstPlasticBagCharge == 1, "plastic_bag_charge", NA))),
                                                      lastPolicyType = ifelse(completeBagBan == 1, "complete_bag_ban", 
                                                                             ifelse(partialBagBan == 1, "partial_bag_ban", 
                                                                                    ifelse(plasticBagCharge == 1, "plastic_bag_charge", NA))))
zipPoliciesCollapse <- zipPoliciesCollapse %>% dplyr::select(-c(firstCompleteBagBan, firstPartialBagBan, firstPlasticBagCharge))
zipPoliciesCollapse <- zipPoliciesCollapse %>% dplyr::select(state, zip, city, county, firstEffect, firstPolicyGeo, lastPolicyGeo, firstPolicyType, lastPolicyType, firstPolicyID, lastPolicyID, 
                                                             description, 
                                                             completeBagBan, partialBagBan, plasticBagCharge, bagLimit, paperBagCharge, paperBagBan, repealed, repealedDate, 
                                                             lat, lon, popIRS)
zipPoliciesCollapse <- zipPoliciesCollapse %>% arrange(state, zip)
zipPoliciesCollapse <- zipPoliciesCollapse %>% ungroup()

zipOverTime <- zipPoliciesCollapse

countyPolicies <- zipPolicies %>% arrange(county, effect)
countyPolicies <- countyPolicies %>% group_by(state, county, effect, description) %>% summarise(completeBagBan = first(complete_bag_ban), 
                                                                                                partialBagBan = first(partial_bag_ban), 
                                                                                                bagLimit = first(bag_limit), 
                                                                                                plasticBagCharge = first(plastic_bag_charge), 
                                                                                                paperBagCharge = first(paper_bag_charge), 
                                                                                                paperBagBan = first(paper_bag_ban), 
                                                                                                repealed = first(repealed), 
                                                                                                repealedDate = first(repealed_date))  %>% mutate(ind = 1)
countiesPoliciesCollapse <- countyPolicies %>% group_by(state, county) %>% summarise(count = sum(ind),
                                                                                    firstEffect = first(effect), 
                                                                                    completeBagBan = last(completeBagBan), 
                                                                                    partialBagBan = last(partialBagBan), 
                                                                                    bagLimit = last(bagLimit), 
                                                                                    plasticBagCharge = last(plasticBagCharge), 
                                                                                    paperBagCharge = last(paperBagCharge), 
                                                                                    paperBagBan = last(paperBagBan), 
                                                                                    repealed = first(repealed), 
                                                                                    repealedDate = first(repealedDate))
countiesPoliciesCollapse <- countiesPoliciesCollapse %>% ungroup()
countiesPoliciesCollapse <- countiesPoliciesCollapse %>% arrange(state, county)
countiesPoliciesCollapse <- left_join(countiesPoliciesCollapse, county %>% dplyr::select(state = state_id, county = county_full, fips = county_fips, pop = population))
countiesPoliciesCollapse <- countiesPoliciesCollapse %>% filter(!is.na(fips) & !is.na(county))


rm(counties, county, countyPolicies, states, towns, zip)
zipAll <- zipPolicies
rm(zipPolicies)

zip <- zipPoliciesCollapse
county <- countiesPoliciesCollapse
rm(zipPoliciesCollapse, countiesPoliciesCollapse)

# save files 
save(zip, file="data/processed/01_zip_policy.rda")
save(county, file="data/processed/01_data_intermediate/01_county_policy.rda")

