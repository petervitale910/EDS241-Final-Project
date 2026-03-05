#############################################################################################
# Final data files 
# last modified: 08/06/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr)

# Functions --------------------------------------------------------------------------------

aggregate_cleanup_data <- function(data, group_cols, filter_condition = NULL) {
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))
  }
  
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(c(people, adults, children, miles, totalItems, itemsPP, itemsPPPM,
                       plasticBag, plasticGroceryBag, plasticOtherBag,
                       percPlasticBag, percGroceryBag, percOtherBag, 
                       percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont,
                       percPaperBag, 
                       plasticBagPP, plasticBagPPPM, plasticBagPM, 
                       plasticBottlePP, plasticBottlePPPM, paperBagPP, paperBagPPPM, tobaccoCigbutt, percCigbutt),
                     ~mean(.x, na.rm = TRUE)),
              zip = first(zip),
              county = first(county), 
              state = first(state),
              cleanupCount = n(), 
              cleanupIds = paste(id, collapse = " ")) %>%
    ungroup() %>%
    mutate(across(c(people, adults, children, miles, totalItems, itemsPP, itemsPPPM,
                    plasticBag, plasticGroceryBag, plasticOtherBag,
                    percPlasticBag, percGroceryBag, percOtherBag, 
                    percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont,
                    percPaperBag, 
                    plasticBagPP, plasticBagPPPM, plasticBagPM, 
                    plasticBottlePP, plasticBottlePPPM, paperBagPP, paperBagPPPM, tobaccoCigbutt, percCigbutt),
                  ~ifelse(is.nan(.x), NA, .x)))
}

save_data <- function(data, filename) {
  save(data, file = paste0("data/processed/00_data_intermediate/", filename, ".rda"))
}

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")

# Process by different levels --------------------------------------------------------------

# generate quarter info 
cleanup <- cleanup %>% mutate(quarter = ifelse(month %in% c(12, 1, 2), 0, 
                                               ifelse(month %in% c(3, 4, 5), 1, 
                                                      ifelse(month %in% c(6, 7, 8), 2, 3))))

cleanup <- cleanup %>% dplyr::select(groupIDCounty, groupIDZip, groupID1, groupID01, groupID001, id, zip, county, state, fips, lat, lon,  date, year, month, quarter,
                                     distanceCoast, distanceRiver, distanceLake, coastInd, riverInd, lakeInd, otherInd, inDrainage, within10km, riverWithin10km, lakeWithin10km, iccd,
                                     people, adults, children, miles, totalItems, itemsPP, itemsPPPM, 
                                     plasticBag, plasticGroceryBag, plasticOtherBag,
                                     plasticBevBottle,paperBags, 
                                     percPlasticBag, percGroceryBag, percOtherBag, 
                                     percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont,
                                     percPaperBag, 
                                     plasticBagPP, plasticBagPPPM, plasticBagPM, 
                                     plasticBottlePP, plasticBottlePPPM, paperBagPP, paperBagPPPM, 
                                     tobaccoCigbutt, percCigbutt, 
                                     smallInd, bigInd, childInd)

# Aggregations  ----------------------------------------------------------------------------

# County aggregations
cleanupCountyYear <- aggregate_cleanup_data(cleanup, c("county", "year"))

# Zip code aggregations
cleanupZipMonth <- aggregate_cleanup_data(cleanup, c("zip", "year", "month"))
cleanupZipQuarter <- aggregate_cleanup_data(cleanup, c("zip", "year", "quarter"))
cleanupZipYear <- aggregate_cleanup_data(cleanup, c("zip", "year"))

# Big cell aggregations
cleanupBigCellMonth <- aggregate_cleanup_data(cleanup, c("groupID1", "year", "month"))
cleanupBigCellQuarter <- aggregate_cleanup_data(cleanup, c("groupID1", "year", "quarter"))
cleanupBigCellYear <- aggregate_cleanup_data(cleanup, c("groupID1", "year"))
cleanupBigCellYearCoast <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "coastInd == 1")
cleanupBigCellYearRiver <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "riverInd == 1")
cleanupBigCellYearRiver10km <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "riverInd == 1 & riverWithin10km == 1")
cleanupBigCellYearLake <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "lakeInd == 1")
cleanupBigCellYearLake10km <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "lakeInd == 1 & lakeWithin10km == 1")
cleanupBigCellYearOther <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "otherInd == 1")
cleanupBigCellYearDrainage <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "inDrainage == 1")
cleanupBigCellYearDrainageNoLake <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "inDrainage == 1 & lakeInd == 0")
cleanupBigCellYear10km <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "within10km == 1 | riverWithin10km == 1 | lakeWithin10km == 1")
cleanupBigCellYearICCD <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "iccd == 1")
cleanupBigCellYearNoKids <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "childInd == 0")
cleanupBigCellYearSmall <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "smallInd == 0")
cleanupBigCellYearBig <- aggregate_cleanup_data(cleanup, c("groupID1", "year"), "bigInd == 0")

# Regular cell aggregations
cleanupCellQuarter <- aggregate_cleanup_data(cleanup, c("groupID01", "year", "quarter"))
cleanupCellYear <- aggregate_cleanup_data(cleanup, c("groupID01", "year"))

# Save -------------------------------------------------------------------------------

# Save county files
save_data(cleanupCountyYear, "00_data_county_year")

# Save zip files
save_data(cleanupZipYear, "00_data_zip_year")

# Save big cell files
save_data(cleanupBigCellMonth, "00_data_bigcell_month")
save_data(cleanupBigCellQuarter, "00_data_bigcell_quarter")
save_data(cleanupBigCellYear, "00_data_bigcell_year")
save_data(cleanupBigCellYearCoast, "00_data_bigcell_year_coast")
save_data(cleanupBigCellYearRiver, "00_data_bigcell_year_river")
save_data(cleanupBigCellYearRiver10km, "00_data_bigcell_year_river_10km")
save_data(cleanupBigCellYearLake, "00_data_bigcell_year_lake")
save_data(cleanupBigCellYearLake10km, "00_data_bigcell_year_lake_10km")
save_data(cleanupBigCellYearOther, "00_data_bigcell_year_other")
save_data(cleanupBigCellYearDrainage, "00_data_bigcell_year_drainage")
save_data(cleanupBigCellYearDrainageNoLake, "00_data_bigcell_year_drainage_nolake")
save_data(cleanupBigCellYear10km, "00_data_bigcell_year_10km")
save_data(cleanupBigCellYearICCD, "00_data_bigcell_year_iccd")
save_data(cleanupBigCellYearNoKids, "00_data_bigcell_year_nokids")
save_data(cleanupBigCellYearSmall, "00_data_bigcell_year_small")
save_data(cleanupBigCellYearBig, "00_data_bigcell_year_big")

# Save cell files
save_data(cleanupCellQuarter, "00_data_cell_quarter")
save_data(cleanupCellYear, "00_data_cell_year")

  
  
