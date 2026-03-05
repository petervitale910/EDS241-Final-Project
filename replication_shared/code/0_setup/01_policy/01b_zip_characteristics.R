#############################################################################################
# Plastic Bag Ban
# Linking policies to zip code characteristics 
# last modified: 05/07/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr, tidyr, cobalt, lubridate)

# Load Data ----------------------------------------------------------------------------------

load("data/processed/01_zip_policy.rda")

# Prepare Zip Characteristics ----------------------------------------------------------------

# Load ZIP Code Characteristics Data
data <- read.csv("data/other/raw_zip_characteristics.csv", header = TRUE)

# Format ZIP Code Data
data$Geo_ZCTA5 <- sprintf("%05d", as.integer(as.character(data$Geo_ZCTA5)))

# Select and Rename Columns in ZIP Code Data
zipChars <- data %>%
  dplyr::select(
    Geo_ZCTA5, SE_A14006_001, SE_A00002_001, SE_A03001_002, SE_A03001_003,
    SE_A03001_004, SE_A03001_005, SE_A03001_006, SE_A03001_007,
    SE_A12001_001, SE_A12001_002, SE_A12001_003, SE_A12001_004,
    SE_A12001_005, SE_A12001_006, SE_A12001_007, SE_A12001_008
  ) %>%
  rename(
    zip = Geo_ZCTA5,
    medianIncome = SE_A14006_001,
    popTotal = SE_A00002_001,
    popWhite = SE_A03001_002,
    popBlack = SE_A03001_003,
    popAmerindianNative = SE_A03001_004,
    popAsian = SE_A03001_005,
    popPacificNative = SE_A03001_006,
    popOther = SE_A03001_007,
    pop25 = SE_A12001_001,
    pop25Less_HS = SE_A12001_002,
    pop25Over_HS = SE_A12001_003,
    pop25SomeCollege = SE_A12001_004,
    pop25Bachelors = SE_A12001_005,
    pop25Masters = SE_A12001_006,
    pop25ProfSchool = SE_A12001_007,
    pop25Doctorate = SE_A12001_008
  )


# Merge Data ----------------------------------------------------------------------------------

zip$zip <- sprintf("%05d", as.integer(as.character(zip$zip)))
zipChars$zip <- sprintf("%05d", as.integer(as.character(zipChars$zip)))

# adding column in policies_zip dummy that takes 1 since they have a policy 
zip <- zip %>% mutate(policy = 1)

# merge 
merged <- left_join(zipChars, zip)
merged <- merged %>% mutate(policy = ifelse(is.na(policy), 0, policy))
merged <- merged %>% mutate(percNonWhite = 1 - (popWhite/popTotal), 
                            percSomeCollege = pop25SomeCollege/popTotal)

# Drop Zips w/o Pop or Income Data -------------------------------------------------------------
merged <- merged %>% filter(!is.na(medianIncome) & !is.na(popTotal))

# sort based on income 
merged <- merged[order(merged$medianIncome), ]
totalpop <- sum(merged$popTotal)
merged$popTotalCum <- cumsum(merged$popTotal)

merged <- merged %>% mutate(incQuartile = ifelse(popTotalCum <= totalpop/4, 4, 
                                                 ifelse(popTotalCum <= totalpop/2, 3, 
                                                        ifelse(popTotalCum <= totalpop/4*3, 2, 1))))

merged <- merged %>% mutate(incQuintile = ifelse(popTotalCum <= totalpop/5, 5, 
                                               ifelse(popTotalCum <= totalpop/5*2, 4,
                                                      ifelse(popTotalCum <= totalpop/5*3, 3, 
                                                             ifelse(popTotalCum <= totalpop/5*4, 2, 1)))))

# quartiles for income 
merged <- merged %>% relocate(zip, medianIncome, incQuartile, incQuintile)
merged <- merged %>% mutate(yearEffect = year(firstEffect))

# Save Data -----------------------------------------------------------------------------------

save(merged, file="data/processed/01_zip_policy_characteristics.rda")




