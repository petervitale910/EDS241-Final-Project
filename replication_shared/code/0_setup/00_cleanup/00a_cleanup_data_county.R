#############################################################################################
# Linking clean up data to counties 
# last modified: 05/07/24 (update with 2023 data)
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, readr, tidyr, broom, stringr, lubridate, raster, sf)

# spherical geometry 
sf_use_s2(FALSE)

# Load data -----------------------------------------------------------------------------------

# beach clean-up data 
data2000 <- read_csv("data/tides/2000-2009.csv")
data2010 <- read_csv("data/tides/2010-2014.csv")
data <- rbind(data2000, data2010)
for(year in 2015:2023){
  datayear <- read_csv(paste0("data/tides/", year, ".csv"))
  data <- rbind(data, datayear)
}
rm(data2000, data2010, datayear)

# county-level shapefile
county <- read_sf(dsn = "data/shapefiles/county/cb_2018_us_county_500k.shp")
county <- county %>% st_transform(4326)
county_all <- county
county <- county %>% dplyr::select(fips = GEOID)

# state fips crosswalk 
state <- read_csv("data/other/us-state-ansi-fips.csv")
state <- data.frame(state) %>% dplyr::select(state = stusps, state_fips = st)

# get county, state -> fips crosswalk 
countyNames <- data.frame(county_all) %>% dplyr::select(state_fips = STATEFP, fips = GEOID, countyName = NAME, countyArea = ALAND, countyWater = AWATER)
countyNames <- countyNames %>% mutate(county = ifelse(state_fips == "22", paste0(countyName, " Parrish"), paste0(countyName, " County"))) %>% dplyr::select(county, state_fips, fips, countyArea, countyWater)
countyNames <- left_join(countyNames, state) 
countyNames <- countyNames %>% dplyr::select(county, state, fips)

# fips to county name 
fips <- read_csv("data/other/us-county-ansi-fips.csv")

# Initial Clean-up -----------------------------------------------------------------------------

# clean column names 
data <- data.frame(data) %>% dplyr::select(id = Cleanup.ID, zone = Zone, state = State, country = Country, coord = GPS, 
                                           type = Cleanup.Type, env = Environment, date = Cleanup.Date, 
                                           group = Group.Name, adults = Adults, children = Children, people = People, 
                                           pounds = Pounds, miles = Miles, totalItems = Total.Items.Collected,
                                           bags = X..of.bags, 
                                           
                                           # plastic objects 
                                           plasticGroceryBag = Grocery.bags..plastic., plasticOtherBag = Other.bags..plastic., 
                                           plasticBevBottle = Beverage.bottles..plastic., plasticBottleOther = Other.plastic.bottles..oil..bleach..etc.., plasticBottleCap = Bottle.caps..plastic., plasticCupPlates = Cups..Plates..plastic.,
                                           plasticFoodCont = Food.containers..plastic., plasticLids = Lids..plastic., plasticStraws = Straws.stirrers..plastic., plasticUtensils = Utensils..plastic.,
                                           plastic6packHolders = X6.Pack.holders, plasticPiece = Plastic.pieces, plasticOtherWaste = Other.plastic.waste,  
                                           plasticFoamOtherPackaging = Other.plastic.foam.packaging, plasticFoamPiece = Plastic.foam.pieces,
                                           
                                           # foam objects 
                                           foamCupsPlates = Cups..Plates..foam., foamFoodCont = Food.containers..foam., foamDockPieces = Foam.dock.pieces, 
                                           foamPackaging = Foam.packaging,  foamPiece = Foam.pieces, 
                                           
                                           # glass objects 
                                           glassBevBottle = Beverage.bottles..glass.,  glassPiece = Glass.pieces,
                                           
                                           # metal objects 
                                           metalCanbev = Beverage.cans, pouchBeverage = Beverage.sachets.pouches, metalBottleCap = Bottle.caps..metal.,  
                                           
                                           # paper objects 
                                           paperCupsPlates = Cups..Plates..paper.,  paperBags = Paper.bags, 
                                           
                                           # food wrappers 
                                           foodWrappers = Food.wrappers..candy..chips..etc.., 
                                            
                                           # appliances, household, construction objects 
                                           appliances = Appliances..refrigerators..washers..etc.., constructionMat = Construction.materials,  tires = Tires,  
                                           ewaste = Electronic.waste..phones..batteries., clothes = Clothing, footwear = Footwear..shoes.slippers., 
                                           balloons = Balloons,  toys = Toys, fireworks = Fireworks, strappingBands = Strapping.bands,  

                                           # fishing related objects 
                                           fishingBuoysPotsTraps = Fishing.buoys..pots...traps, fishingNet = Fishing.net...pieces, 
                                           fishingLine = Fishing.line..1.yard.meter...1.piece., rope = Rope..1.yard.meter...1.piece., fishingGear = Fishing.gear..Clean.Swell., 
                                           linesRopes = Lines..nets..traps..ropes..etc., 
                                           
                                           # tobacco related objects 
                                           tobaccoCigbutt = Cigarette.butts, tobaccoCigartips = Cigar.tips, tobaccoCigLighters = Cigarette.lighters,
                                           tobaccoEcig = E.cigarettes, tobacco_wrap  = Tobacco.packaging.wrap,  tobaccoOtherPackaging = Other.tobacco..packaging..lighter..etc.., 
                                           tobaccoOtherProducts = Tobacco.products..lighters..cigar.tips..wrap.,
                                          
                                           # personal hygiene related objects 
                                           personalHygiene = Personal.hygiene..Clean.Swell., condoms = Condoms, tampons = Tampons...applicators,
                                           cottonBud = Cotton.bud.sticks..swabs., diaper = Diapers, 
                                           
                                           # syringe 
                                           syringe = Syringes,  
                                           
                                           # ppe 
                                           ppe = Gloves...masks..PPE., 
                                           
                                           # other 
                                           nonplasticOtherWaste = Other.waste..metal..paper..etc..,
                                           otherPackaging = Other.packaging..Clean.Swell., otherTrash = Other.trash..Clean.Swell.) 

# filter country, drop year totals 
data <- data %>% filter(!is.na(country))
data <- data %>% filter(!is.na(coord))

# keep only land clean ups 
data <- data %>% filter(type == "land")
data <- data %>% dplyr::select(-c("country", "type", "env", "group"))

# date 
data <- data %>% mutate(date = as.Date(date, format="%m/%d/%y"))
data <- data %>% mutate(year = year(date), month = month(date))
data <- arrange(data, date)

# lat and longitude 
data <- data %>% separate(coord,c("lat", "lon"), ",")
data <- data %>% mutate(lat = as.numeric(lat), 
                        lon = as.numeric(lon))

# missing lat / lon or missing people, miles 
data <- data %>% filter(!is.na(lat) & !is.na(lon) & !is.na(date) & !is.na(people) & !is.na(miles))
data <- data %>% relocate(id, zone, state, lat, lon, date, year, month)

# county and state 
data <- data %>% mutate(zoneOrig = zone, stateOrig = state)
data <- data %>% filter(stateOrig != "Ontario, Canada" & stateOrig != "United States")
data <- data %>% separate(zone, c("county", "state", "country"), ",")
data <- data %>% relocate(id, zoneOrig, stateOrig, county, state, country)
data <- data %>% mutate(state = ifelse(state == " VA 24112" | state == " VA 20111" | state == " VA 24273" | 
                                         state == " VA 23847" | state == " VA 23834" | state == " VA 22401" | 
                                         state == " VA 24153" | state == " VA 22980"| state == " Arlington", " VA", state), 
                        state = ifelse(state == " AK 99840", " AK", state), 
                        state = ifelse(state == " CA 93430", " CA", state), 
                        state = ifelse(state == " NY 11754", " NY", state), 
                        state = ifelse(state == " FL 33715", " FL", state), 
                        state = ifelse(state == " HI 96753", " HI", state), 
                        state = ifelse(state == " San Francisco", " CA", state), 
                        state = ifelse(state == " New York" | state == " The Bronx" | state == " Staten Island" | state == " Brooklyn" | state == " Queens", " NY", state), 
                        state = ifelse(state == " Philadelphia", " PA", state), 
                        state = ifelse(state == " Denver", " CO", state), 
                        state = ifelse(state == " Columbus", " GA", state), 
                        state = ifelse(state == " Lexington", " KY", state), 
                        state = ifelse(state == " New Orleans", " LA", state), 
                        state = ifelse(state == " D.C.", " DC", state), 
                        state = ifelse(state == " Sitka", "AK", state), 
                        state = ifelse(state == " Bronx", "NY", state), 
                        state = ifelse(state == " Anchorage", "AK", state))
data$state <- trimws(data$state, which=c("both"))
data <- data %>% dplyr::select(-c("zoneOrig", "stateOrig", "country"))

# Get Counties -----------------------------------------------------------------------------

dataCounty <- data[(data$county %like% " County" | data$county %like% " Parish"),]
dataCounty <- dataCounty %>% filter(county != "St. Louis County" & county != "Baltimore County" & county != "Fairfax County" & county != "Richmond County" & county != "Roanoke County" & county != "Franklin County")
dataCounty <- left_join(dataCounty, countyNames)
dataCounty <- dataCounty %>% relocate(id, county, state, fips, lat, lon, date, year, month)

dataCityCounty <- data %>% filter(county == "St. Louis County" | county == "Baltimore County" | county == "Fairfax County" | county == "Richmond County" | county == "Roanoke County" | county == "Franklin County")
dataCityCountySf <- st_as_sf(dataCityCounty, coords = c("lon", "lat"), crs=4326)
dataCityCountySf  <- st_intersection(dataCityCountySf, county)
dataCityCounty <- data.frame(dataCityCountySf) %>% dplyr::select(-c("geometry"))
dataCityCounty <- left_join(dataCityCounty, data)
dataCityCounty <- dataCityCounty %>% relocate(id, county, state, fips, lat, lon, date, year, month)
rm(dataCityCountySf)

dataMissing <- data[!(data$county %like% " County" | data$county %like% " Parish"),]
dataMissingSf <- st_as_sf(dataMissing, coords = c("lon", "lat"), crs=4326)
dataMissingSf  <- st_intersection(dataMissingSf, county)
dataMissing <- data.frame(dataMissingSf) %>% dplyr::select(-c("geometry"))
dataMissing <- left_join(dataMissing, data)
dataMissing <- dataMissing %>% relocate(id, county, state, fips, lat, lon, date, year, month)

cleanup <- rbind(dataCounty, dataCityCounty, dataMissing)
rm(dataCityCounty, dataCounty, dataMissing, dataMissingSf)
cleanup <- arrange(cleanup, date)

# Fix Missing County and State ------------------------------------------------------------- 

cleanupMissing <- cleanup %>% filter(is.na(county)) %>% mutate(fips = as.numeric(fips))
cleanupMissing <- left_join(cleanupMissing %>% dplyr::select(-c(county)), fips)
cleanupMissing <- cleanupMissing %>% dplyr::select(-c(state))
cleanupMissing <- cleanupMissing %>% mutate(state_fips = floor(fips/1000))
cleanupMissing <- left_join(cleanupMissing, state %>% mutate(state_fips = as.numeric(state_fips)))
cleanupMissing <- cleanupMissing %>% dplyr::select(id, county, state, fips, lat, lon, date, year, month, adults, children, people, pounds, miles, totalItems, bags, plasticGroceryBag, plasticOtherBag, plasticBevBottle, plasticBottleOther, plasticBottleCap, plasticCupPlates, plasticFoodCont, plasticLids, plasticStraws, plasticUtensils, plastic6packHolders, plasticPiece, plasticOtherWaste, 
                                                   plasticFoamOtherPackaging, plasticFoamPiece, foamCupsPlates, foamFoodCont, foamDockPieces, foamPackaging, foamPiece, glassBevBottle, glassPiece, metalCanbev, pouchBeverage, metalBottleCap, paperCupsPlates, paperBags, foodWrappers, appliances, constructionMat, tires, ewaste, clothes, footwear, balloons, toys, fireworks, strappingBands, fishingBuoysPotsTraps, fishingNet, fishingLine, 
                                                   rope, fishingGear, linesRopes, tobaccoCigbutt, tobaccoCigartips, tobaccoCigLighters, tobaccoEcig, tobacco_wrap, tobaccoOtherPackaging, tobaccoOtherProducts, personalHygiene, condoms, tampons, cottonBud, diaper, syringe, ppe, nonplasticOtherWaste, otherPackaging, otherTrash)
cleanupMissing <- cleanupMissing %>% mutate(county = ifelse(fips == 12086, "Miami-Dade County", county))

cleanupBefore <- cleanup %>% filter(!is.na(county))
cleanup <- rbind(cleanupBefore, cleanupMissing)
cleanup <- cleanup %>% rename(tobaccoWrap = tobacco_wrap)
rm(cleanupMissing, cleanupBefore, county, county_all, countyNames, data, fips, state)

# International Clean-Up Day ---------------------------------------------------------------

cleanup <- cleanup %>% mutate(iccd = ifelse((date == as.Date("2015-09-19", format="%Y-%m-%d") | date == as.Date("2016-09-17", format="%Y-%m-%d") | date == as.Date("2017-09-16", format="%Y-%m-%d") | date == as.Date("2018-09-15", format="%Y-%m-%d") 
                                             | date == as.Date("2019-09-21", format="%Y-%m-%d") | date == as.Date("2020-09-19", format="%Y-%m-%d") | date == as.Date("2021-09-18", format="%Y-%m-%d") | date == as.Date("2022-09-17", format="%Y-%m-%d") | date == as.Date("2023-09-16", format="%Y-%m-%d")), 1, 0))

# Fix some fips codes 
fips <- read.csv("data/other/statefips.csv")
cleanup <- left_join(cleanup, fips)

cleanup <- cleanup  %>% mutate(fipsCheck = as.numeric(substr(fips, 1, 2)))
problem <- cleanup %>% filter(statefips != fipsCheck)

cleanup <- cleanup %>% mutate(fips = ifelse(fips == "2122" & state == "AK", "02122", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "6037" & state == "CA", "06037", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "6073" & state == "CA", "06073", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "6053" & state == "CA", "06053", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "6061" & state == "CA", "06061", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "11001" & state == "VA", "51510", fips))
cleanup <- cleanup %>% mutate(fips = ifelse(fips == "24033" & state == "VA", "51059", fips))

# Fix Wrong State --------------------------------------------------------------------------
cleanup <- cleanup %>% filter(!(state == "FL" & lat > 40))
cleanup <- cleanup %>% filter(!(state == "NC" & lat > 44))
cleanup <- cleanup %>% filter(!(state == "OH" & lat > 46))
cleanup <- cleanup %>% filter(!(state == "OH" & lon > -75))
cleanup <- cleanup %>% filter(!(state == "MD" & lon < -78 & lat < 39.5))
cleanup <- cleanup %>% filter(!(state == "SC" & lat > 36))
cleanup <- cleanup %>% filter(!(state == "NY" & lat < 30))
cleanup <- cleanup %>% filter(!(state == "NY" & lon < -82))
cleanup <- cleanup %>% filter(!(state == "NJ" & lat > 42))
cleanup <- cleanup %>% filter(!(state == "CO" & lat > 44))
cleanup <- cleanup %>% filter(!(state == "MO" & lat > 46))
cleanup <- cleanup %>% filter(!(state == "IA" & lon > -75))
cleanup <- cleanup %>% filter(!(state == "NV" & lon < -122))

# Final County Cleanup ---------------------------------------------------------------------

save(cleanup, file="data/processed/00_data_intermediate/00_data_cleanup_county.rda")
