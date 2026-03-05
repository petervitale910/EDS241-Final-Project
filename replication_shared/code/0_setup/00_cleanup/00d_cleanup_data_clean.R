#############################################################################################
# Clean clean-up data 
# last modified: 09/07/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr)

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_intermediate/00_data_cleanup_county_zip_cell.rda")

# Clean data -------------------------------------------------------------------------------

# keep 2016 onwards
cleanup <- cleanup %>% filter(year >= 2016)

# delete duplicate observations
cleanup$duplicate <- duplicated(cleanup[, !(names(cleanup) %in% "id")])
cleanup <- cleanup %>% filter(duplicate == FALSE) %>% dplyr::select(-c("duplicate"))
count1 <- nrow(cleanup)

# delete observations with all NAs for items, 0 total items, but NOT zero bags / pounds 
cleanup <- cleanup %>% mutate(na = ifelse(is.na(plasticGroceryBag) & is.na(plasticOtherBag) & is.na(plasticBevBottle) & is.na(plasticBottleOther) & is.na(plasticBottleCap) & is.na(plasticCupPlates) & is.na(plasticFoodCont) & is.na(plasticLids) & is.na(plasticStraws) & is.na(plasticUtensils) & is.na(plastic6packHolders) & is.na(plasticPiece) & is.na(plasticOtherWaste) & is.na(plasticFoamOtherPackaging) & is.na(plasticFoamPiece) 
                                     & is.na(foamCupsPlates) & is.na(foamFoodCont) & is.na(foamDockPieces) & is.na(foamPackaging) & is.na(foamPiece) 
                                     & is.na(glassBevBottle) & is.na(glassPiece) & is.na(metalCanbev) & is.na(pouchBeverage) & is.na(metalBottleCap) & is.na(paperCupsPlates) & is.na( paperBags) 
                                     & is.na(foodWrappers) & is.na(appliances) & is.na(constructionMat) & is.na(tires) & is.na(ewaste) & is.na(clothes) & is.na(footwear) & is.na(balloons) & is.na(toys) & is.na(fireworks) & is.na(strappingBands) 
                                     & is.na(fishingBuoysPotsTraps) & is.na(fishingNet) & is.na(fishingLine) & is.na(rope) & is.na(fishingGear) 
                                     & is.na(linesRopes) & is.na(tobaccoCigbutt) & is.na(tobaccoCigartips) & is.na(tobaccoCigLighters) & is.na(tobaccoEcig) & is.na(tobaccoWrap) & is.na(tobaccoOtherPackaging) & is.na(tobaccoOtherProducts) 
                                     & is.na(personalHygiene) & is.na(condoms) & is.na(tampons) & is.na(cottonBud) & is.na(diaper) & is.na(syringe) & is.na(ppe) & is.na(nonplasticOtherWaste) & is.na(otherPackaging) & is.na(otherTrash),
                                     1, 0))
cleanup <- cleanup %>% filter(!(na == 1 & !is.na(bags)))
cleanup <- cleanup %>% filter(!(na == 1 & pounds > 0.02))
count2 <- nrow(cleanup)

cleanup <- cleanup %>% filter(!(na == 1 & is.na(bags) & pounds == 0 & miles == 0))
cleanup <- cleanup %>% filter(!(na == 1 & is.na(bags) & pounds == 0.01 & miles == 0))
count3 <- nrow(cleanup)

# differences (for appendix)
count1-count2
count2-count3 

# for the rest, assume 0 items 
cleanup <- cleanup %>% mutate(totalItems = ifelse(is.na(totalItems), 0, totalItems))

# check that the number of items is equal to sum 
cleanup$itemSum <-rowSums(cleanup %>% dplyr::select(plasticGroceryBag, plasticOtherBag, plasticBevBottle, plasticBottleOther, plasticBottleCap, plasticCupPlates, plasticFoodCont, plasticLids, plasticStraws, plasticUtensils, plastic6packHolders, plasticPiece, plasticOtherWaste, plasticFoamOtherPackaging, plasticFoamPiece, 
                                                   foamCupsPlates, foamFoodCont, foamDockPieces, foamPackaging, foamPiece,
                                                   glassBevBottle, glassPiece, metalCanbev, pouchBeverage, metalBottleCap, paperCupsPlates,  paperBags, 
                                                   foodWrappers, appliances, constructionMat, tires, ewaste, clothes, footwear, balloons, toys, fireworks, strappingBands, 
                                                   fishingBuoysPotsTraps, fishingNet, fishingLine, rope, fishingGear, 
                                                   linesRopes, tobaccoCigbutt, tobaccoCigartips, tobaccoCigLighters, tobaccoEcig, tobaccoWrap, tobaccoOtherPackaging, tobaccoOtherProducts, 
                                                   personalHygiene, condoms, tampons, cottonBud, diaper, syringe, ppe, nonplasticOtherWaste, otherPackaging,otherTrash), na.rm=TRUE)
  
# check whether there are any unequal ones 
nrow(cleanup %>% filter(totalItems != itemSum))

# round miles, calculate miles per person
cleanup <- cleanup %>% mutate(miles = round(miles, 2))
cleanup <- cleanup %>% mutate(milesPP = miles / people)

# now we can calculate the per person and other measures 
cleanup <- cleanup %>% mutate(plasticGroceryBag = ifelse(is.na(plasticGroceryBag), 0, plasticGroceryBag), 
                              plasticOtherBag = ifelse(is.na(plasticOtherBag), 0, plasticOtherBag), 
                              plasticBevBottle = ifelse(is.na(plasticBevBottle), 0, plasticBevBottle),
                              plasticBottleCap = ifelse(is.na(plasticBottleCap), 0, plasticBottleCap),
                              plasticStraws = ifelse(is.na(plasticStraws), 0, plasticStraws),
                              plasticFoodCont = ifelse(is.na(plasticFoodCont), 0, plasticFoodCont),
                              paperBags = ifelse(is.na(paperBags), 0, paperBags), 
                              tobaccoCigbutt = ifelse(is.na(tobaccoCigbutt), 0, tobaccoCigbutt))

cleanup <- cleanup %>% mutate(itemsPP = totalItems / people, 
                              itemsPPPM = ifelse(miles > 0, totalItems / people / miles, NA), 
                              plasticBag = plasticGroceryBag + plasticOtherBag, 
                              percGroceryBag = ifelse(!is.na(totalItems), plasticGroceryBag / totalItems, NA), 
                              percOtherBag = ifelse(!is.na(totalItems), plasticOtherBag / totalItems, NA), 
                              percPlasticBag = ifelse(!is.na(totalItems), (plasticGroceryBag + plasticOtherBag) / totalItems, NA),
                              percPlasticBottle = ifelse(!is.na(totalItems), plasticBevBottle / totalItems, NA),
                              percPaperBag = ifelse(!is.na(totalItems), paperBags / totalItems, NA),
                              percPlasticBottleCap = ifelse(!is.na(totalItems), plasticBottleCap / totalItems, NA),
                              percPlasticStraws = ifelse(!is.na(totalItems), plasticStraws / totalItems, NA),
                              percPlasticFoodCont = ifelse(!is.na(totalItems), plasticFoodCont / totalItems, NA),
                              plasticBagPP = plasticBag / people, 
                              plasticBagPPPM = ifelse(miles > 0, plasticBag / people / miles, NA),
                              plasticBagPM = ifelse(miles > 0, plasticBag / miles, NA),
                              plasticBottlePP = plasticBevBottle / people, 
                              plasticBottlePPPM = ifelse(miles > 0, plasticBevBottle / people / miles, NA),
                              paperBagPP = paperBags / people, 
                              paperBagPPPM = ifelse(miles > 0, paperBags / people / miles, NA), 
                              percCigbutt =  ifelse(!is.na(totalItems), tobaccoCigbutt / totalItems, NA)) 

# drop outliers 
cleanup <- cleanup %>% filter(milesPP < quantile(cleanup$milesPP, c(0.99), na.rm=TRUE)[1] &  
                              itemsPP < quantile(cleanup$itemsPP, c(0.99), na.rm=TRUE)[1] & 
                              (is.na(itemsPPPM) | itemsPPPM < quantile(cleanup$itemsPPPM, c(0.99), na.rm=TRUE)[1]))

# count (for appendix)
count4 <- nrow(cleanup)
count3-count4

# keep data 
cleanup <- cleanup %>% dplyr::select(groupIDCounty, groupIDZip, groupID1, groupID01, groupID001, 
                                     id, zip, county, state, fips, lat, lon, date, year, month, iccd, people, adults, children, miles, 
                                     totalItems, itemsPP, itemsPPPM, 
                                     plasticBag, plasticGroceryBag, plasticOtherBag, plasticBevBottle, paperBags, 
                                     percPlasticBag, percGroceryBag, percOtherBag,  
                                     percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, percPaperBag,
                                     plasticBagPP, plasticBagPPPM, plasticBagPM, 
                                     plasticBottlePP, plasticBottlePPPM, 
                                     paperBagPP, paperBagPPPM, 
                                     tobaccoCigbutt, percCigbutt)

# cleanup robustness 
cleanup <- cleanup %>% mutate(smallInd = ifelse(people < 5,1, 0), 
                              bigInd = ifelse(people > 50 ,1, 0), 
                              childInd = ifelse(children == 0 | is.na(children), 0, 1), 
                              children = ifelse(is.na(children), 0, children))

# save 
save(cleanup, file="data/processed/00_data_cleanup.rda")

# save table for later use 
cleanup <- cleanup %>% dplyr::select(id, lat, lon)
write.csv(cleanup, file="data/processed/00_data_intermediate/00_data_cleanup_locations.csv")

                              