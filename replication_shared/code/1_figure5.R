#############################################################################################
# Entanglement TWFE - Figure 5
# last modified: 03/06/25
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## directory, update as needed
directory <- "/Users/"
setwd(paste0(directory, "plasticbag/replication/"))

## check - this should be the "replication" folder
getwd()

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(devtools)
#install_version("DIDmultiplegtDYN", version = "1.0.15")
#install_version("DIDmultiplegt", version = "0.1.0")
pacman::p_load(dplyr, tidyr, tidyverse, broom, ggplot2, PNWColors, fixest, lfe, did, didimputation, DIDmultiplegt, DIDmultiplegtDYN, scales, stringr, ggpubr, gridExtra, grid, patchwork)    

## check versions
packageVersion("DIDmultiplegt")
packageVersion("DIDmultiplegtDYN")

# Figure Setup ------------------------------------------------------------------------------

# text size 
textSize <- 15

# palette gray
paletteGray <- gray.colors(5, start = 0.0, end = 0.65, gamma = 2.2, alpha = NULL, rev = FALSE)

# plot theme 
add_common_theme <- theme_bw() + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(size = textSize-6), 
        axis.text.y=element_text(size = textSize-4), 
        axis.title.x=element_text(size = textSize-2), 
        axis.title.y=element_text(size = textSize-2, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-6), 
        legend.title=element_text(size=textSize-5), 
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.x=element_blank(), 
        axis.ticks.length = unit(0.15, "cm"), 
        plot.title = element_text(hjust = 0.5, face="bold", size = textSize-2), 
        plot.subtitle = element_text(hjust = 0.5)) 

# Entanglement, Unbalanced ---------------------------------------------------------------

load("data/processed/03_data_entanglement.rda")

## Overall --- 
# function 
did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced) {
  
  # Load data
  data <- file_name
  
  # Data preparation based on time_aggregation
  if (time_aggregation == "y") {
    data <- data %>% 
      mutate(treat = treatedAny,
             firstY = year(firstEffect), 
             y = year)
    time_var <- "y"
  } else if (time_aggregation == "ym") {
    data <- data %>% 
      mutate(treat = treatedAny,
             ym = month + (year - 2016)*12,
             firstYM = month(firstEffect) + (year(firstEffect)-2016)*12)
    time_var <- "ym"
  } else if (time_aggregation == "yq") {
    data <- data %>% 
      mutate(treat = treatedAny,
             yq = quarter + (year - 2016)*4,
             firstQ =  ifelse(month(firstEffect) %in% c(12, 1, 2), 0,
                              ifelse(month(firstEffect) %in% c(3, 4, 5), 1,
                                     ifelse(month(firstEffect) %in% c(6, 7, 8), 2,
                                            ifelse(month(firstEffect) %in% c(9, 10, 11), 3, NA)))),
             firstY = ifelse(month(firstEffect) == 12, year(firstEffect) + 1, year(firstEffect))) %>%
      mutate(firstYQ = firstQ + (firstY - 2016)*4)
    time_var <- "yq"
  } else {
    stop("Invalid time_aggregation. Choose 'y', 'ym', or 'yq'.")
  }
  
  # Filter needed 
  dataMain <- data %>% 
    filter(type %in% c("charge", "complete", "partial", "control")) 
  
  # Create variables for estimation
  dataMain <- dataMain %>%
    group_by(!!sym(group_id_var)) %>%
    mutate(treatEver = sum(treat) > 0,
           timeToTreat = ifelse(treatEver, !!sym(time_var) - !!sym(paste0("first", toupper(time_var))), 0),
           firstSA = ifelse(treatEver, !!sym(paste0("first", toupper(time_var))) + 1, 10000),
           treatDCDH = treat == 1 & !!sym(time_var) >= !!sym(paste0("first", toupper(time_var))),
           firstCS = ifelse(is.na(!!sym(paste0("first", toupper(time_var)))), 0, !!sym(paste0("first", toupper(time_var))) + 1)) %>%
    ungroup()
  
  # Filter those with varying start and end dates 
  dataMain <- dataMain %>% group_by(!!sym(group_id_var)) %>% mutate(min = min(firstCS), 
                                                                    max = max(firstCS)) %>% ungroup()
  dataMain <- dataMain %>% mutate(notEq = ifelse(min != max, 1, 0))
  dataMain <- dataMain %>% filter(notEq != 1)
  
  # Select final needed variables
  dataMain <- dataMain %>%
    dplyr::select(type, zip, !!sym(group_id_var), county, year, !!sym(time_var), treat, treatEver, treatDCDH, 
                  !!sym(paste0("first", toupper(time_var))), firstCS, firstSA, timeToTreat, 
                  people, adults, children, totalItems, miles, cleanupCount, itemsPP, itemsPPPM, 
                  plasticBag, plasticGroceryBag, plasticOtherBag, entanglementInd, entanglementBagInd,  percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))
  
  # TWFE model
  formula_twfe <- as.formula(paste("entanglementBagInd  ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate, 
           std.error = std.error) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "entanglementBagInd", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect, dcdh$se_effect)
  results_model <- rbind(results_model, dcdh_results)
  
  # Callaway and Sant'anna 2021
  if(time_aggregation != "ym"){
    atts <- att_gt(data = dataMain, yname = "entanglementBagInd", tname = time_var,  
                   idname = group_id_var, gname = "firstCS", clustervars = "zip",
                   control_group = "notyettreated",
                   bstrap = TRUE, biters = 1000,
                   allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
    agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
    cs_results <- c('Callaway and Santanna (2021)', item_description, 
                    (agg_effects$overall.att ), (agg_effects$overall.se))
    results_model <- rbind(results_model, cs_results)
  }
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("entanglementBagInd ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] ), (sa[2]))
  results_model <- rbind(results_model, sa_results)
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "entanglementBagInd", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
  } 
  borusyak_results <- c('Borusyak et al. (2021)', item_description, 
                        borusyak$estimate, borusyak$std.error)
  results_model <- rbind(results_model, borusyak_results)
  
  # Process results
  results_model <- results_model %>% mutate(estimate = as.numeric(estimate), se = as.numeric(std.error)) %>% dplyr::select(version = item, model, estimate, se) %>%
    mutate(confLow = estimate - 1.96 * se, 
           confHigh = estimate + 1.96 * se,
           confLow90 = estimate - 1.645 * se, 
           confHigh90 = estimate + 1.645 * se, 
           estimate = estimate , 
           se = se , 
           confLow = confLow , 
           confHigh = confHigh ,
           confLow90 = confLow90 , 
           confHigh90 = confHigh90 ) %>%
    mutate(model = factor(model, levels = c("TWFE", 
                                            "Callaway and Santanna (2021)", 
                                            "de Chaisemartin and D'Haultfoeuille (2020)", 
                                            "Sun and Abraham (2021)",
                                            "Borusyak et al. (2021)"
    )))
  
  return(results_model)
}

# results 
results <- did_analysis("groupID1", data, "Entanglement", "y", 0)
results <- results %>% mutate(time = 0)

# keep relevant variables obs/variables
dataMain <- data %>% filter(type %in% c("charge", "complete", "partial", "control")) 
dataMain <- dataMain %>% mutate(treat = treatedAny, firstY = year(firstEffect))
dataMain <- dataMain %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataMain <- dataMain %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataMain <- dataMain %>% arrange(groupID1, year)

# control mean 
control_mean <- dataMain %>% filter(treatEver == 0) %>% summarise(control_mean = mean(entanglementBagInd))
control_mean <- control_mean$control_mean

results <- results  %>% mutate(estimate = estimate/control_mean*100, 
                               se = se/control_mean*100, 
                               confLow = confLow/control_mean*100, 
                               confHigh = confHigh/control_mean*100, 
                               confLow90 = confLow90/control_mean*100, 
                               confHigh90 = confHigh90/control_mean*100)
results_entanglement <- results
rm(results)

# get number of observations 
nobs_main <- nrow(dataMain)
count_main <- sum(dataMain$cleanupCount)

## Dynamic --- 

# keep relevant variables 
dataDynamic <- data %>% filter(type %in% c("charge", "complete", "partial", "control")) 

# variables for estimation 
dataDynamic <- dataDynamic %>%mutate(treat = treatedAny,firstY = year(firstEffect), y = year)
dataDynamic <- dataDynamic %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataDynamic <- dataDynamic %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataDynamic <- dataDynamic %>% mutate(timeToTreat = ifelse(treatEver == 1, year - firstY, 0))
dataDynamic <- dataDynamic %>% mutate(firstYSA = ifelse(treatEver == 0, 10000, firstY+1))
dataDynamic <- dataDynamic %>% mutate(treatDCDH = ifelse(treat == 1 & year >= firstY, 1, 0))
dataDynamic <- dataDynamic %>% mutate(firstYCS = ifelse(is.na(firstY), 0, firstY+1))

# keep final needed variables 
dataDynamic <- dataDynamic %>% dplyr::select(type, zip, groupID1, county, year, treat, treatEver, treatDCDH, firstY, firstYCS, firstYSA, timeToTreat, people, adults, children, totalItems, miles, cleanupCount, cleanupIds, itemsPP, itemsPPPM, entanglement, entanglementBag, entanglementInd, entanglementBagInd, plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, lastPolicyID, firstPolicyID)
dataDynamic <- dataDynamic %>% ungroup()
dataDynamic <- dataDynamic %>% arrange(groupID1, year)

# TWFE 
dynamicTWFE <- feols(entanglementBagInd ~ i(timeToTreat, treatEver, ref = 0) | groupID1 + year,  cluster = ~groupID1, data = dataDynamic)
dynamicTWFE_results <- tidy(dynamicTWFE, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Entanglement")
dynamicTWFE_results$term <- as.numeric(str_extract(dynamicTWFE_results$term, "(?<=timeToTreat::).*?(?=:treatEver)"))
dynamicTWFE_results <- dynamicTWFE_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicTWFE_results$estimate, "type") <- NULL
attr(dynamicTWFE_results$se, "type") <- NULL
dynamicTWFE_results <- dynamicTWFE_results %>% filter(time >= -3 & time <= 6)
dynamicTWFE_results <- rbind(dynamicTWFE_results, c("TWFE", "Entanglement", 0, 0, 0, 0, 0))
dynamicTWFE_results <- dynamicTWFE_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicTWFE_results <- dynamicTWFE_results %>% arrange(time)
rm(dynamicTWFE)

# Callaway and Sant'anna 2021
atts <- att_gt(data = dataDynamic, yname = "entanglementBagInd", tname = "year",  idname = "groupID1", gname = "firstYCS", clustervars = "zip",
               control_group = "notyettreated", 
               bstrap = TRUE, biters = 1000, 
               allow_unbalanced_panel = T, panel = T, base_period = "universal") 
agg_es <- aggte(atts, type = "dynamic")
dynamicCS_results <- tidy(agg_es, conf.int=TRUE) %>% mutate(model = "Callaway and Santanna (2021)", item = "Entanglement")
dynamicCS_results <- dynamicCS_results %>% dplyr::select(model, item, time = event.time, estimate, se = std.error, confLow = point.conf.low, confHigh = point.conf.high)
dynamicCS_results <- dynamicCS_results %>% mutate(time = time + 1)
dynamicCS_results <- dynamicCS_results %>% mutate(estimate = ifelse(time == 0, 0, estimate), se = ifelse(time == 0, 0, se), confLow = ifelse(time == 0, 0, confLow), confHigh = ifelse(time == 0, 0, confHigh))
dynamicCS_results <- dynamicCS_results %>% filter(time >= -3 & time <= 6)
rm(atts, agg_es)

# Chaisemartin and D’Haultfœuille 2020
dynamicDcdh <- did_multiplegt_dyn(df = dataDynamic, outcome = "entanglementBagInd", time = "year", group = "groupID1", treatment = "treatDCDH", cluster = "zip",  effects = 7, placebo = 4)
dynamicDcdh_results_p1 <- data.frame(dynamicDcdh$results$Effects) %>% mutate(time = row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Entanglement") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p1) <- NULL
dynamicDcdh_results_p2 <- data.frame(dynamicDcdh$results$Placebos)%>% mutate(time = -row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Entanglement") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p2) <- NULL
dynamicDcdh_results <- rbind(dynamicDcdh_results_p1, dynamicDcdh_results_p2)
rm(dynamicDcdh, dynamicDcdh_results_p1, dynamicDcdh_results_p2)
dynamicDcdh_results <- rbind(dynamicDcdh_results, c("de Chaisemartin and D'Haultfoeuille (2020)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicDcdh_results <- dynamicDcdh_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicDcdh_results <- dynamicDcdh_results %>% filter(time >= -3 & time <= 6) %>% arrange(time)

# Sun and Abraham 2020 
dynamicSA <- feols(entanglementBagInd ~ sunab(firstYSA, year) | groupID1 + year,  cluster = ~groupID1, data = dataDynamic)
dynamicSA_results <- tidy(dynamicSA, conf.int=TRUE) %>% mutate(model = "Sun and Abraham (2021)", item = "Entanglement")
dynamicSA_results$term <- as.numeric(str_extract(dynamicSA_results$term, "(?<=year::).*"))+1
dynamicSA_results <- dynamicSA_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicSA_results$estimate, "type") <- NULL
attr(dynamicSA_results$se, "type") <- NULL
dynamicSA_results <- dynamicSA_results %>% filter(time >= -3 & time <= 6)
dynamicSA_results <- rbind(dynamicSA_results, c("Sun and Abraham (2021)", "Entanglement", 0, 0, 0, 0, 0))
dynamicSA_results <- dynamicSA_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicSA_results <- dynamicSA_results %>% arrange(time)
rm(dynamicSA)

# combine dynamic results 
dynamic_results <- rbind(dynamicTWFE_results, dynamicCS_results, dynamicDcdh_results, dynamicSA_results)

# control mean 
control_mean <- dataDynamic %>% filter(treatEver == 0) %>% summarise(control_mean = mean(entanglementBagInd))
control_mean <- control_mean$control_mean
dynamic_results <- dynamic_results %>% mutate(estimate = estimate/control_mean*100, se = se/control_mean*100, confLow = confLow/control_mean*100, confHigh = confHigh/control_mean*100)
dynamic_results <- dynamic_results %>% mutate(confLow90 = estimate - 1.645*se, confHigh90 = estimate + 1.645*se)
dynamic_results <- dynamic_results %>% mutate(model = factor(model, levels = c("TWFE", "Callaway and Santanna (2021)", "de Chaisemartin and D'Haultfoeuille (2020)", "Sun and Abraham (2021)")))
dynamic_results <- dynamic_results %>% filter(time < 6)
dynamic_results_entanglement <- dynamic_results 
rm(dynamic_results)

# Entanglement, Balanced -----------------------------------------------------------------

load("data/processed/03_data_entanglement_balanced.rda")

## Overall --- 
# function 
did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced) {
  
  # Load data
  data <- file_name
  
  # Data preparation based on time_aggregation
  if (time_aggregation == "y") {
    data <- data %>% 
      mutate(treat = treatedAny,
             firstY = year(firstEffect), 
             y = year)
    time_var <- "y"
  } else if (time_aggregation == "ym") {
    data <- data %>% 
      mutate(treat = treatedAny,
             ym = month + (year - 2016)*12,
             firstYM = month(firstEffect) + (year(firstEffect)-2016)*12)
    time_var <- "ym"
  } else if (time_aggregation == "yq") {
    data <- data %>% 
      mutate(treat = treatedAny,
             yq = quarter + (year - 2016)*4,
             firstQ =  ifelse(month(firstEffect) %in% c(12, 1, 2), 0,
                              ifelse(month(firstEffect) %in% c(3, 4, 5), 1,
                                     ifelse(month(firstEffect) %in% c(6, 7, 8), 2,
                                            ifelse(month(firstEffect) %in% c(9, 10, 11), 3, NA)))),
             firstY = ifelse(month(firstEffect) == 12, year(firstEffect) + 1, year(firstEffect))) %>%
      mutate(firstYQ = firstQ + (firstY - 2016)*4)
    time_var <- "yq"
  } else {
    stop("Invalid time_aggregation. Choose 'y', 'ym', or 'yq'.")
  }
  
  # Filter needed 
  dataMain <- data %>% 
    filter(type %in% c("charge", "complete", "partial", "control")) 
  
  # Create variables for estimation
  dataMain <- dataMain %>%
    group_by(!!sym(group_id_var)) %>%
    mutate(treatEver = sum(treat) > 0,
           timeToTreat = ifelse(treatEver, !!sym(time_var) - !!sym(paste0("first", toupper(time_var))), 0),
           firstSA = ifelse(treatEver, !!sym(paste0("first", toupper(time_var))) + 1, 10000),
           treatDCDH = treat == 1 & !!sym(time_var) >= !!sym(paste0("first", toupper(time_var))),
           firstCS = ifelse(is.na(!!sym(paste0("first", toupper(time_var)))), 0, !!sym(paste0("first", toupper(time_var))) + 1)) %>%
    ungroup()
  
  # Filter those with varying start and end dates 
  dataMain <- dataMain %>% group_by(!!sym(group_id_var)) %>% mutate(min = min(firstCS), 
                                                                    max = max(firstCS)) %>% ungroup()
  dataMain <- dataMain %>% mutate(notEq = ifelse(min != max, 1, 0))
  dataMain <- dataMain %>% filter(notEq != 1)
  
  # Select final needed variables
  dataMain <- dataMain %>%
    dplyr::select(type, zip, !!sym(group_id_var), county, year, !!sym(time_var), treat, treatEver, treatDCDH, 
                  !!sym(paste0("first", toupper(time_var))), firstCS, firstSA, timeToTreat, 
                  people, adults, children, totalItems, miles, cleanupCount, itemsPP, itemsPPPM, 
                  plasticBag, plasticGroceryBag, plasticOtherBag, entanglementInd, entanglementBagInd,  percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))
  
  # TWFE model
  formula_twfe <- as.formula(paste("entanglementBagInd  ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate, 
           std.error = std.error) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "entanglementBagInd", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect, dcdh$se_effect)
  results_model <- rbind(results_model, dcdh_results)
  
  # Callaway and Sant'anna 2021
  if(time_aggregation != "ym"){
    atts <- att_gt(data = dataMain, yname = "entanglementBagInd", tname = time_var,  
                   idname = group_id_var, gname = "firstCS", clustervars = "zip",
                   control_group = "notyettreated",
                   bstrap = TRUE, biters = 1000,
                   allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
    agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
    cs_results <- c('Callaway and Santanna (2021)', item_description, 
                    (agg_effects$overall.att ), (agg_effects$overall.se))
    results_model <- rbind(results_model, cs_results)
  }
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("entanglementBagInd ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] ), (sa[2]))
  results_model <- rbind(results_model, sa_results)
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "entanglementBagInd", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
  } 
  borusyak_results <- c('Borusyak et al. (2021)', item_description, 
                        borusyak$estimate, borusyak$std.error)
  results_model <- rbind(results_model, borusyak_results)
  
  # Process results
  results_model <- results_model %>% mutate(estimate = as.numeric(estimate), se = as.numeric(std.error)) %>% dplyr::select(version = item, model, estimate, se) %>%
    mutate(confLow = estimate - 1.96 * se, 
           confHigh = estimate + 1.96 * se,
           confLow90 = estimate - 1.645 * se, 
           confHigh90 = estimate + 1.645 * se, 
           estimate = estimate , 
           se = se , 
           confLow = confLow , 
           confHigh = confHigh ,
           confLow90 = confLow90 , 
           confHigh90 = confHigh90 ) %>%
    mutate(model = factor(model, levels = c("TWFE", 
                                            "Callaway and Santanna (2021)", 
                                            "de Chaisemartin and D'Haultfoeuille (2020)", 
                                            "Sun and Abraham (2021)",
                                            "Borusyak et al. (2021)"
    )))
  
  return(results_model)
}

# results 
results <- did_analysis("groupID1", dataBalanced, "Entanglement", "y", 0)
results <- results %>% mutate(time = 0)

# keep relevant variables obs/variables
dataMain <- dataBalanced %>% filter(type %in% c("charge", "complete", "partial", "control")) 
dataMain <- dataMain %>% mutate(treat = treatedAny, firstY = year(firstEffect))
dataMain <- dataMain %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataMain <- dataMain %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataMain <- dataMain %>% arrange(groupID1, year)

# control mean 
control_mean <- dataMain %>% filter(treatEver == 0) %>% summarise(control_mean = mean(entanglementBagInd))
control_mean <- control_mean$control_mean

results <- results  %>% mutate(estimate = estimate/control_mean*100, 
                               se = se/control_mean*100, 
                               confLow = confLow/control_mean*100, 
                               confHigh = confHigh/control_mean*100, 
                               confLow90 = confLow90/control_mean*100, 
                               confHigh90 = confHigh90/control_mean*100)
results_entanglement_balanced <- results
rm(results)

# get number of observations 
nobs_main_balanced <- nrow(dataMain)
count_main_balanced <- sum(dataMain$cleanupCount)

## Dynamic --- 

# keep relevant variables 
dataDynamic <- dataBalanced %>% filter(type %in% c("charge", "complete", "partial", "control")) 

# variables for estimation 
dataDynamic <- dataDynamic %>%mutate(treat = treatedAny,firstY = year(firstEffect), y = year)
dataDynamic <- dataDynamic %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataDynamic <- dataDynamic %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataDynamic <- dataDynamic %>% mutate(timeToTreat = ifelse(treatEver == 1, year - firstY, 0))
dataDynamic <- dataDynamic %>% mutate(firstYSA = ifelse(treatEver == 0, 10000, firstY+1))
dataDynamic <- dataDynamic %>% mutate(treatDCDH = ifelse(treat == 1 & year >= firstY, 1, 0))
dataDynamic <- dataDynamic %>% mutate(firstYCS = ifelse(is.na(firstY), 0, firstY+1))

# keep final needed variables 
dataDynamic <- dataDynamic %>% dplyr::select(type, zip, groupID1, county, year, treat, treatEver, treatDCDH, firstY, firstYCS, firstYSA, timeToTreat, people, adults, children, totalItems, miles, cleanupCount, cleanupIds, itemsPP, itemsPPPM, entanglement, entanglementBag, entanglementInd, entanglementBagInd, plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, lastPolicyID, firstPolicyID)
dataDynamic <- dataDynamic %>% ungroup()
dataDynamic <- dataDynamic %>% arrange(groupID1, year)

# TWFE 
dynamicTWFE <- feols(entanglementBagInd ~ i(timeToTreat, treatEver, ref = 0) | groupID1 + year,  cluster = ~groupID1, data = dataDynamic)
dynamicTWFE_results <- tidy(dynamicTWFE, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Entanglement")
dynamicTWFE_results$term <- as.numeric(str_extract(dynamicTWFE_results$term, "(?<=timeToTreat::).*?(?=:treatEver)"))
dynamicTWFE_results <- dynamicTWFE_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicTWFE_results$estimate, "type") <- NULL
attr(dynamicTWFE_results$se, "type") <- NULL
dynamicTWFE_results <- dynamicTWFE_results %>% filter(time >= -3 & time <= 6)
dynamicTWFE_results <- rbind(dynamicTWFE_results, c("TWFE", "Entanglement", 0, 0, 0, 0, 0))
dynamicTWFE_results <- dynamicTWFE_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicTWFE_results <- dynamicTWFE_results %>% arrange(time)
rm(dynamicTWFE)

# Callaway and Sant'anna 2021
atts <- att_gt(data = dataDynamic, yname = "entanglementBagInd", tname = "year",  idname = "groupID1", gname = "firstYCS", clustervars = "zip",
               control_group = "notyettreated", 
               bstrap = TRUE, biters = 1000, 
               allow_unbalanced_panel = T, panel = T, base_period = "universal") 
agg_es <- aggte(atts, type = "dynamic")
dynamicCS_results <- tidy(agg_es, conf.int=TRUE) %>% mutate(model = "Callaway and Santanna (2021)", item = "Entanglement")
dynamicCS_results <- dynamicCS_results %>% dplyr::select(model, item, time = event.time, estimate, se = std.error, confLow = point.conf.low, confHigh = point.conf.high)
dynamicCS_results <- dynamicCS_results %>% mutate(time = time + 1)
dynamicCS_results <- dynamicCS_results %>% mutate(estimate = ifelse(time == 0, 0, estimate), se = ifelse(time == 0, 0, se), confLow = ifelse(time == 0, 0, confLow), confHigh = ifelse(time == 0, 0, confHigh))
dynamicCS_results <- dynamicCS_results %>% filter(time >= -3 & time <= 6)
rm(atts, agg_es)

# Chaisemartin and D’Haultfœuille 2020
dynamicDcdh <- did_multiplegt_dyn(df = dataDynamic, outcome = "entanglementBagInd", time = "year", group = "groupID1", treatment = "treatDCDH", cluster = "zip",  effects = 7, placebo = 4)
dynamicDcdh_results_p1 <- data.frame(dynamicDcdh$results$Effects) %>% mutate(time = row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Entanglement") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p1) <- NULL
dynamicDcdh_results_p2 <- data.frame(dynamicDcdh$results$Placebos)%>% mutate(time = -row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Entanglement") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p2) <- NULL
dynamicDcdh_results <- rbind(dynamicDcdh_results_p1, dynamicDcdh_results_p2)
rm(dynamicDcdh, dynamicDcdh_results_p1, dynamicDcdh_results_p2)
dynamicDcdh_results <- rbind(dynamicDcdh_results, c("de Chaisemartin and D'Haultfoeuille (2020)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicDcdh_results <- dynamicDcdh_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicDcdh_results <- dynamicDcdh_results %>% filter(time >= -3 & time <= 6) %>% arrange(time)

# Sun and Abraham 2020 
dynamicSA <- feols(entanglementBagInd ~ sunab(firstYSA, year) | groupID1 + year,  cluster = ~groupID1, data = dataDynamic)
dynamicSA_results <- tidy(dynamicSA, conf.int=TRUE) %>% mutate(model = "Sun and Abraham (2021)", item = "Entanglement")
dynamicSA_results$term <- as.numeric(str_extract(dynamicSA_results$term, "(?<=year::).*"))+1
dynamicSA_results <- dynamicSA_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicSA_results$estimate, "type") <- NULL
attr(dynamicSA_results$se, "type") <- NULL
dynamicSA_results <- dynamicSA_results %>% filter(time >= -3 & time <= 6)
dynamicSA_results <- rbind(dynamicSA_results, c("Sun and Abraham (2021)", "Entanglement", 0, 0, 0, 0, 0))
dynamicSA_results <- dynamicSA_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicSA_results <- dynamicSA_results %>% arrange(time)
rm(dynamicSA)

# combine dynamic results 
dynamic_results <- rbind(dynamicTWFE_results, dynamicCS_results, dynamicDcdh_results, dynamicSA_results)

# control mean 
control_mean <- dataDynamic %>% filter(treatEver == 0) %>% summarise(control_mean = mean(entanglementBagInd))
control_mean <- control_mean$control_mean
dynamic_results <- dynamic_results %>% mutate(estimate = estimate/control_mean*100, se = se/control_mean*100, confLow = confLow/control_mean*100, confHigh = confHigh/control_mean*100)
dynamic_results <- dynamic_results %>% mutate(confLow90 = estimate - 1.645*se, confHigh90 = estimate + 1.645*se)
dynamic_results <- dynamic_results %>% mutate(model = factor(model, levels = c("TWFE", "Callaway and Santanna (2021)", "de Chaisemartin and D'Haultfoeuille (2020)", "Sun and Abraham (2021)")))
dynamic_results <- dynamic_results %>% filter(time < 6)
dynamic_results_entanglement_balanced <- dynamic_results 
rm(dynamic_results)

# Plots -------------------------------------------------------------------

# overall, unbalanced
entanglement <- results_entanglement %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.25, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 3, alpha=0.35, position = position_dodge(width = 0.5)) +
  geom_point(aes(color = model, shape=model), size = 4, position = position_dodge(width = 0.5)) +
  labs(y = "Entangled Animals (Indicator)\n∆ Relative to Control Mean (%)", x = " ", title=paste0("Overall Effects"), subtitle = "Coastal Cleanups\nUnbalanced Panel") +
  scale_color_manual(name = "Estimator", values = paletteGray) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) + 
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) +  
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = c(0), labels = c(paste0("0.1° Cell x Year\n Cleanups = ", comma(count_main), "\n Obs = ", comma(nobs_main)))) +
  scale_y_continuous(breaks = seq(-200, 100, by = 100), limits=c(-225,100)) +
  add_common_theme 
entanglement <- entanglement + guides(color= guide_legend(nrow=3))

# extract legend
legend_paper <- get_legend(entanglement + guides(color = guide_legend(nrow = 3, keywidth = 1,  override.aes = list(shape = c(16, 17, 4, 15, 1),linewidth = 0, alpha = 1))))

# dynamic plot, unbalanced
entanglement_dynamic <- dynamic_results_entanglement %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.5, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 1.5, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 2, position = position_dodge(width = 0.6)) +
  labs(y = "Entangled Animals (Indicator)\n∆ Relative to Control Mean (%)", x = "Years Since Policy", title=paste0("Dynamic Effects"), subtitle=paste0("0.1° Cell x Year Aggregation; Unbalanced Panel")) + 
  scale_color_manual(name = "Estimator", values = paletteGray) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15)) +  
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15)))) +  
  scale_x_continuous(breaks = seq(-3, 5, by = 1), limits=c(-3.5,5.5)) +
  scale_y_continuous(breaks = seq(-200, 200, by = 100), limits=c(-325,225)) +
  add_common_theme

# overall, balanced
entanglement_balanced <- results_entanglement_balanced %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.25, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 3, alpha=0.35, position = position_dodge(width = 0.5)) +
  geom_point(aes(color = model, shape=model), size = 4, position = position_dodge(width = 0.5)) +
  labs(y = "Entangled Animals (Indicator)\n∆ Relative to Control Mean (%)", x = " ", title=paste0("Overall Effects"), subtitle = "Coastal Cleanups\nBalanced Panel") +
  scale_color_manual(name = "Estimator", values = paletteGray) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) + 
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) +  
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = c(0), labels = c(paste0("0.1° Cell x Year\n Cleanups = ", comma(count_main_balanced), "\n Obs = ", comma(nobs_main_balanced)))) +
  scale_y_continuous(breaks = seq(-200, 100, by = 100), limits=c(-225,100)) +
  add_common_theme + 
  theme(legend.direction = "vertical")

# dynamic plot, balanced
entanglement_dynamic_balanced <- dynamic_results_entanglement_balanced %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.5, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 1.5, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 2, position = position_dodge(width = 0.6)) +
  labs(y = "Entangled Animals (Indicator)\n∆ Relative to Control Mean (%)", x = "Years Since Policy", title=paste0("Dynamic Effects"), subtitle=paste0("0.1° Cell x Year Aggregation; Balanced Panel")) + 
  scale_color_manual(name = "Estimator", values = paletteGray) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15)) +  
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15)))) +  
  scale_x_continuous(breaks = seq(-3, 5, by = 1), limits=c(-3.5,5.5)) +
  scale_y_continuous(breaks = seq(-200, 200, by = 100), limits=c(-325,225)) +
  add_common_theme

# get rid of individual legends
entanglement <- entanglement + theme(legend.position = "none")
entanglement_dynamic <- entanglement_dynamic + theme(legend.position = "none")
entanglement_balanced <- entanglement_balanced + theme(legend.position = "none")
entanglement_dynamic_balanced <- entanglement_dynamic_balanced + theme(legend.position = "none")

# arrange plots using ggarrange
arranged_plots <- ggarrange(
  ggarrange(entanglement, NA, entanglement_balanced, ncol = 3, labels = c("A",  "", "C"), widths = c(2, 0.25, 2)),
  NA,
  ggarrange(entanglement_dynamic, NA, entanglement_dynamic_balanced, ncol = 3, labels = c("B", "", "D"), widths = c(2, 0.25, 2)),
  nrow = 3, 
  heights = c(2, 0.25, 2)
)
arranged_plots

# combine arranged plots with the legend
final_plot <-  grid.arrange(
  grobs = list(arranged_plots, legend_paper),
  layout_matrix = rbind(c(1),
                        c(NA),
                        c(2)),
  heights = c(10, 0.5, 1)
)

ggsave("output/Figure5.jpeg", final_plot, width = 11, height = 10, unit = 'in')

