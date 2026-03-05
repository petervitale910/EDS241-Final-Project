#############################################################################################
# Entanglement data cleaning 
# last modified: 01/16/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr, data.table)

# Clean entanglement data -------------------------------------------------------------------

# entangled animals
data <- data.frame()
for(year in 2016:2023){
  datayear <- read_csv(paste0("data/tides/entanglement/entangled-animals-united-states_", year, ".csv"))
  data <- rbind(data, datayear)
}
rm(datayear)

# stats (dead animals / non-missing entanglement debris)
nrow(data %>% filter(`Animal Status` == "dead"))/nrow(data)
nrow(data %>% filter(!is.na(`Entanglement Debris`)))/nrow(data)
data <- data.frame(data)

# tag possible plastic bags (no entanglement debris specified plus plastic bags)
data <- data %>% mutate(possible_plastic_bag = ifelse(grepl("plastic bag", Entanglement.Debris, ignore.case = TRUE)|grepl("bag", Entanglement.Debris, ignore.case = TRUE)|is.na(Entanglement.Debris), 1, 0))

# coordinates 
data <- separate(data, GPS, into = c("latitude", "longitude"), sep = ", ")
data <- data %>% mutate(latitude = as.numeric(latitude), 
                        longitude = as.numeric(longitude))
data <- data %>% mutate(year = as.numeric(str_sub(Cleanup.date, -4)))

# keep lat/lon/year/indicator
data <- data %>% dplyr::select(latitude, longitude, year, possible_plastic_bag) 
data <- data %>% mutate(latFloor1 = floor(latitude * 10) / 10, 
                        lonFloor1 = floor(longitude * 10) / 10)
nrow(data %>% filter(possible_plastic_bag == 1))

# sum by grid cell, year for both possible plastic bag and other entanglements 
entanglement <- data %>% group_by(latFloor1, lonFloor1, year, possible_plastic_bag) %>% summarise(n = n()) %>% ungroup()
entanglement <- entanglement %>% group_by(latFloor1, lonFloor1, year) %>% summarise(possible_plastic_bag = sum(n[possible_plastic_bag == 1]), total = sum(n)) %>% ungroup()
entanglement <- entanglement %>% rename(lat = latFloor1, lon = lonFloor1)
rm(data)

# bring in cleanup
load("data/processed/00_data_cleanup.rda")
cleanup <- cleanup %>% dplyr::select(groupID1, lat, lon, state)
cleanup <- cleanup %>% mutate(lat = floor(lat * 10) / 10, 
                             lon = floor(lon * 10) / 10)
cleanup <- cleanup %>% distinct(groupID1, lat, lon)

# merge back with entanglement 
entanglement <- left_join(entanglement, cleanup)
entanglement <- entanglement %>% filter(!is.na(groupID1))
entanglement <- entanglement %>% dplyr::select(groupID1, year, possible_plastic_bag, all = total) %>% ungroup()

# load coastal cleanup data and add in entanglement data
load(paste0("data/processed/02_merged_bigcell_year_coast.rda"))
data <- left_join(data, entanglement)
data <- data %>% mutate(entanglement = ifelse(is.na(all), 0, all), 
                        entanglementBag = ifelse(is.na(possible_plastic_bag), 0, possible_plastic_bag),
                        entanglementInd = ifelse(entanglement > 0, 1, 0), 
                        entanglementBagInd = ifelse(entanglementBag > 0, 1, 0), 
                        entanglementPerCleanup = entanglement / cleanupCount, 
                        entanglementBagPerCleanup = entanglementBag / cleanupCount)

# Filter needed 
data <- data %>% filter(state != "CA") %>% filter(!(state == "NY" & type == "control"))

# balanced panel version 
dataBalanced <- data %>% group_by(groupID1) %>% filter(n() == 8) %>% ungroup()

# save 
save(data, file = "data/processed/03_data_entanglement.rda")
save(dataBalanced, file = "data/processed/03_data_entanglement_balanced.rda")


