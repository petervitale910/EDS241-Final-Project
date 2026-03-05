#############################################################################################
# Linking clean up data to lat/lon cells 
# last modified: 07/31/23
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr)

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_intermediate/00_data_cleanup_county_zip.rda")

# Create cells  ----------------------------------------------------------------------------

cleanup <- cleanup %>% mutate(latFloor001 = floor(lat * 1000) / 1000, 
                              lonFloor001 = floor(lon * 1000) / 1000, 
                              latFloor01 = floor(lat * 100) / 100, 
                              lonFloor01 = floor(lon * 100) / 100, 
                              latFloor1 = floor(lat * 10) / 10, 
                              lonFloor1 = floor(lon * 10)/10)

cleanup <- cleanup %>% group_by(latFloor001, lonFloor001) %>% mutate(groupID001 = cur_group_id()) %>% ungroup()
cleanup <- cleanup %>% group_by(latFloor01, lonFloor01) %>% mutate(groupID01 = cur_group_id()) %>% ungroup()
cleanup <- cleanup %>% group_by(latFloor1, lonFloor1) %>% mutate(groupID1 = cur_group_id()) %>% ungroup()
cleanup <- cleanup %>% group_by(zip) %>% mutate(groupIDZip = cur_group_id()) %>% ungroup()
cleanup <- cleanup %>% group_by(fips) %>% mutate(groupIDCounty = cur_group_id()) %>% ungroup()

cleanup <- cleanup %>% dplyr::select(groupIDCounty, groupIDZip, groupID1, groupID01, groupID001, id, zip, county, state, fips, lat, lon, date, year, month, iccd, adults, children, people, 
                                     pounds, miles, bags, totalItems, 
                                     plasticGroceryBag, plasticOtherBag, plasticBevBottle, plasticBottleOther, plasticBottleCap, plasticCupPlates, plasticFoodCont, plasticLids, plasticStraws, plasticUtensils, plastic6packHolders, plasticPiece, plasticOtherWaste, plasticFoamOtherPackaging, plasticFoamPiece, 
                                     foamCupsPlates, foamFoodCont, foamDockPieces, foamPackaging, foamPiece,
                                     glassBevBottle, glassPiece, metalCanbev, pouchBeverage, metalBottleCap, paperCupsPlates,  paperBags, 
                                     foodWrappers, appliances, constructionMat, tires, ewaste, clothes, footwear, balloons, toys, fireworks, strappingBands, 
                                     fishingBuoysPotsTraps, fishingNet, fishingLine, rope, fishingGear, 
                                     linesRopes, tobaccoCigbutt, tobaccoCigartips, tobaccoCigLighters, tobaccoEcig, tobaccoWrap, tobaccoOtherPackaging, tobaccoOtherProducts, 
                                     personalHygiene, condoms, tampons, cottonBud, diaper, syringe, ppe, nonplasticOtherWaste, otherPackaging,otherTrash)

# save data 
save(cleanup, file="data/processed/00_data_intermediate/00_data_cleanup_county_zip_cell.rda")

