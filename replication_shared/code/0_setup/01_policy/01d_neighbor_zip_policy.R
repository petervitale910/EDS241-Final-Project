#############################################################################################
# Plastic Bag Ban
# Neighboring zip 
# last modified: 05/08/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(dplyr)

# Load zip data -----------------------------------------------------------------------------

# zip policy data 
load(file="data/processed/01_zip_policy.rda")

# zip neighbor data 
load(file="data/processed/01_data_intermediate/01_zip_neighbors_list.rda")
neighborPairs <- neighborPairs %>% mutate(zip = as.integer(zip), neighbor = as.integer(neighbor))
neighborPairs <- neighborPairs %>% filter(zip != neighbor)

# Merge and flag neighboring  ---------------------------------------------------------------
neighbor <- left_join(zip %>% filter(is.na(repealed) | repealed == 0), neighborPairs)
neighbor <- neighbor %>% dplyr::select(zip = neighbor, firstEffect) %>% mutate(neighborInd = 1)

neighborRepealed <- left_join(zip %>% filter(repealed == 1), neighborPairs)
neighborRepealed <- neighborRepealed %>% dplyr::select(zip = neighbor, firstEffect) %>% mutate(neighborRepealedInd = 1)

zipUnique <- zip %>% filter(is.na(repealed) | repealed == 0) %>% distinct(zip) %>% mutate(treatedInd = 1)
zipUniqueRepealed <- zip %>% filter(repealed == 1) %>% distinct(zip) %>% mutate(repealedInd = 1)

neighbor <- left_join(neighbor, zipUnique)
neighbor <- neighbor %>% filter(is.na(treatedInd))
neighbor <- neighbor %>% dplyr::select(zip, neighborInd, firstEffect) %>% mutate(neighborRepealedInd = NA)
neighbor <- neighbor %>% group_by(zip) %>% summarise(neighborInd = mean(neighborInd, na.rm=T), neighborRepealedInd = NA, firstEffect = min(firstEffect))

neighborRepealed <- left_join(neighborRepealed, zipUniqueRepealed)
neighborRepealed <- neighborRepealed %>% filter(is.na(repealedInd))
neighborRepealed <- neighborRepealed %>% dplyr::select(zip, neighborRepealedInd) %>% mutate(neighborInd= NA, firstEffect = NA)
neighborRepealed <- neighborRepealed %>% distinct(zip, neighborInd, neighborRepealedInd, firstEffect)

neighborZip <- rbind(neighbor, neighborRepealed)
neighborZip <- neighborZip %>% filter(!is.na(zip))
neighborZip <- neighborZip %>% group_by(zip) %>% summarise(neighborInd = sum(neighborInd, na.rm=TRUE), neighborRepealedInd = sum(neighborRepealedInd, na.rm=TRUE), firstEffect = min(firstEffect, na.rm=TRUE)) %>% ungroup()
neighborZip <- neighborZip %>% rename(neighborFirstEffect = firstEffect)

# Neighbors of neighbors  ----------------------------------------------------------------

# check second "layer" of neighboring zip codes 
neighborLayer2 <- left_join(neighborZip, neighborPairs)
neighborLayer2 <- neighborLayer2 %>% mutate(neighborInd2 = ifelse(neighborInd == 1, 1, 0), 
                                            neighborRepealedInd2 = ifelse(neighborRepealedInd == 1, 1, 0))
neighborLayer2 <- neighborLayer2 %>% dplyr::select(zip = neighbor, neighborInd2, neighborRepealedInd2, neighborFirstEffect)

# now drop those that are neighbors 
neighborLayer2 <- left_join(neighborLayer2, neighbor)
neighborLayer2 <- neighborLayer2 %>% filter(is.na(neighborInd) & is.na(neighborRepealedInd))

# now drop those that are treated 
neighborLayer2 <- left_join(neighborLayer2 %>% dplyr::select(zip, neighborInd2, neighborRepealedInd2, neighborFirstEffect), zip %>% distinct(zip) %>% mutate(treatedInd = 1))
neighborLayer2 <- neighborLayer2 %>% filter(is.na(treatedInd))

# now keep only one per neighbor
neighborLayer2 <- neighborLayer2 %>% group_by(zip) %>% summarise(neighborInd = mean(neighborInd2, na.rm=T), neighborRepealedInd = mean(neighborRepealedInd2, na.rm=T), neighborFirstEffect = min(neighborFirstEffect)) %>% ungroup()
neighborLayer2 <- neighborLayer2 %>% mutate(neighborInd = ifelse(neighborInd > 0, 2, 0), 
                                            neighborRepealedInd = ifelse(neighborRepealedInd > 0, 2, 0))
neighborZipLayer2 <- neighborLayer2

# Neighbors of neighbors of neighbors  -----------------------------------------------------

# check second "layer" of neighboring zip codes 
neighborLayer3 <- left_join(neighborZipLayer2, neighborPairs)
neighborLayer3 <- neighborLayer3 %>% mutate(neighborInd3 = ifelse(neighborInd == 2, 1, 0), 
                                            neighborRepealedInd3 = ifelse(neighborRepealedInd == 2, 1, 0))
neighborLayer3 <- neighborLayer3 %>% dplyr::select(zip = neighbor, neighborInd3, neighborRepealedInd3, neighborFirstEffect)

# now drop those that are neighbors of neighbors
neighborLayer3 <- left_join(neighborLayer3, neighborZipLayer2)
neighborLayer3 <- neighborLayer3 %>% filter(is.na(neighborInd) & is.na(neighborRepealedInd))
neighborLayer3 <- neighborLayer3 %>% dplyr::select(-c(neighborInd, neighborRepealedInd))

# now drop those that are neighbors 
neighborLayer3 <- left_join(neighborLayer3, neighbor)
neighborLayer3 <- neighborLayer3 %>% filter(is.na(neighborInd) & is.na(neighborRepealedInd))
neighborLayer3 <- neighborLayer3 %>% dplyr::select(-c(neighborInd, neighborRepealedInd))

# now drop those that are treated 
neighborLayer3 <- left_join(neighborLayer3 %>% dplyr::select(zip, neighborInd3, neighborRepealedInd3, neighborFirstEffect), zip %>% distinct(zip) %>% mutate(treatedInd = 1))
neighborLayer3 <- neighborLayer3 %>% filter(is.na(treatedInd))

# now keep only one per neighbor
neighborLayer3 <- neighborLayer3 %>% group_by(zip) %>% summarise(neighborInd = mean(neighborInd3, na.rm=T), neighborRepealedInd = mean(neighborRepealedInd3, na.rm=T), neighborFirstEffect = min(neighborFirstEffect)) %>% ungroup()
neighborLayer3 <- neighborLayer3 %>% mutate(neighborInd = ifelse(neighborInd > 0, 3, 0), 
                                            neighborRepealedInd = ifelse(neighborRepealedInd > 0, 3, 0))
neighborZipLayer3 <- neighborLayer3

# Now join all neighbors  --------------------------------------------------------------

neighborZipFinal <- rbind(neighborZip, neighborZipLayer2, neighborZipLayer3)
neighborZipFinal <- neighborZipFinal %>% group_by(zip) %>% summarise(neighborInd = min(neighborInd, na.rm=T), neighborRepealedInd = min(neighborRepealedInd, na.rm=T), neighborFirstEffect = min(neighborFirstEffect)) %>% ungroup()

# save 
save(neighborZipFinal, file="data/processed/01_zip_neighbors_policy.rda")

