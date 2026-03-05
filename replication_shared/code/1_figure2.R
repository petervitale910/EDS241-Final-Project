#############################################################################################
# Main TWFE analysis of bag bans - FIGURE 2
# last modified: 03/05/25
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
pacman::p_load(dplyr, tidyr, broom, ggplot2, PNWColors, fixest, lfe, did, didimputation, DIDmultiplegt, DIDmultiplegtDYN, scales, stringr, ggpubr, gridExtra)    

## check versions
packageVersion("DIDmultiplegt")
packageVersion("DIDmultiplegtDYN")

# Figure formatting -------------------------------------------------------------------------

# font size and colors 
textSize <- 15
pal <- pnw_palette("Bay", 5)
palette <- gray.colors(5, start = 0.0, end = 0.65, gamma = 2.2, alpha = NULL, rev = FALSE)

# plot theme 
add_common_theme <- theme_bw() + 
        theme(legend.position = "bottom", 
        axis.text.x=element_text(size = textSize-6), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-3), 
        legend.title=element_text(size=textSize-3), 
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
        plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5)) 

# Function -------------------------------------------------------------------------------

did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, covid1, covid2, one_treat, only_treat, grocery_bag) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else if (grocery_bag == 1) {
    data <- data %>% mutate(percPlasticBag = percGroceryBag)
  } else {
  }
  
  # Subsets of data 
  if (covid1 == 1){
    data <- data %>% filter(year != 2020)
  } else if (covid2 == 1){
    data <- data %>% filter(year != 2020 & year != 2021)
  } else if (one_treat == 1){
    data <- data %>% filter((lastPolicyID == firstPolicyID) | (is.na(lastPolicyID) | is.na(firstPolicyID)))
  } else if (only_treat == 1){
    data <- data %>% filter(type != "control")
  }
  
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
                  plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))
  
  # Calculate control mean
  if (only_treat != 1){
    control_mean <- dataMain %>% 
      filter(type == "control") %>% 
      summarise(mean = mean(percPlasticBag, na.rm = TRUE)) %>%
      pull(mean)
  } else {
    control_mean <- dataMain %>% 
      filter(treat == 0) %>% 
      summarise(mean = mean(percPlasticBag, na.rm = TRUE)) %>%
      pull(mean)
  }
  
  
  # TWFE model
  formula_twfe <- as.formula(paste("percPlasticBag ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate / control_mean, 
           std.error = std.error / control_mean) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "percPlasticBag", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect / control_mean, dcdh$se_effect / control_mean)
  results_model <- rbind(results_model, dcdh_results)

  # Callaway and Sant'anna 2021
  if(time_aggregation != "ym"){
    atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = time_var,  
                   idname = group_id_var, gname = "firstCS", clustervars = "zip",
                   control_group = "notyettreated",
                   bstrap = TRUE, biters = 1000,
                   allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
    agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
    cs_results <- c('Callaway and Santanna (2021)', item_description, 
                    (agg_effects$overall.att / control_mean), (agg_effects$overall.se / control_mean))
    results_model <- rbind(results_model, cs_results)
  }
 
  # Sun and Abraham 2020 model
  if (covid1 == 1){
    dataSA <- dataMain %>% filter(firstSA != 2021)
  } else if (covid2 == 1){
    dataSA <- dataMain %>% filter(firstSA != 2021 & firstSA != 2022)
  } else {
    dataSA <- dataMain
  }
  formula_sa <- as.formula(paste("percPlasticBag ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] / control_mean), (sa[2] / control_mean))
  results_model <- rbind(results_model, sa_results)
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
  } else if (time_aggregation == "y" & group_id_var == "zip") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "zip", cluster_var = "zip")
  } 
  borusyak_results <- c('Borusyak et al. (2021)', item_description, 
                        borusyak$estimate / control_mean, borusyak$std.error / control_mean)
  results_model <- rbind(results_model, borusyak_results)
  
  # Process results
  results_model <- results_model %>% mutate(estimate = as.numeric(estimate), se = as.numeric(std.error)) %>% dplyr::select(version = item, model, estimate, se) %>%
    mutate(confLow = estimate - 1.96 * se, 
           confHigh = estimate + 1.96 * se,
           confLow90 = estimate - 1.645 * se, 
           confHigh90 = estimate + 1.645 * se, 
           estimate = estimate * 100, 
           se = se * 100, 
           confLow = confLow * 100, 
           confHigh = confHigh * 100,
           confLow90 = confLow90 * 100, 
           confHigh90 = confHigh90 * 100) %>%
    mutate(model = factor(model, levels = c("TWFE", 
                                            "Callaway and Santanna (2021)", 
                                            "de Chaisemartin and D'Haultfoeuille (2020)", 
                                            "Sun and Abraham (2021)",
                                            "Borusyak et al. (2021)"
                                            )))
  
  return(results_model)
}

counts_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, covid1, covid2, one_treat, only_treat, grocery_bag) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else if (grocery_bag == 1) {
    data <- data %>% mutate(percPlasticBag = percGroceryBag)
  } else {
  }
  
  # Subsets of data 
  if (covid1 == 1){
    data <- data %>% filter(year != 2020)
  } else if (covid2 == 1){
    data <- data %>% filter(year != 2020 & year != 2021)
  } else if (one_treat == 1){
    data <- data %>% filter((lastPolicyID == firstPolicyID) | (is.na(lastPolicyID) | is.na(firstPolicyID)))
  } else if (only_treat == 1){
    data <- data %>% filter(type != "control")
  }
  
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
                  plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))

  cleanups <- sum(dataMain$cleanupCount)
  
  return(cleanups)
}

nobs_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, covid1, covid2, one_treat, only_treat, grocery_bag) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else if (grocery_bag == 1) {
    data <- data %>% mutate(percPlasticBag = percGroceryBag)
  } else {
  }
  
  # Subsets of data 
  if (covid1 == 1){
    data <- data %>% filter(year != 2020)
  } else if (covid2 == 1){
    data <- data %>% filter(year != 2020 & year != 2021)
  } else if (one_treat == 1){
    data <- data %>% filter((lastPolicyID == firstPolicyID) | (is.na(lastPolicyID) | is.na(firstPolicyID)))
  } else if (only_treat == 1){
    data <- data %>% filter(type != "control")
  }
  
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
                  plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))
  
  nobs <- nrow(dataMain)
  
  return(nobs)
}

add_manual_legend_entry <- function(plot, new_label, new_color, new_shape) {
  
  # Function to find color aesthetic in layers
  find_color_aes <- function(layer) {
    aes <- layer$mapping
    if ("color" %in% names(aes)) return(list(name = "color", aes = aes$color))
    if ("colour" %in% names(aes)) return(list(name = "colour", aes = aes$colour))
    return(NULL)
  }
  
  # Find the first layer with a color aesthetic
  color_info <- NULL
  for (layer in plot$layers) {
    color_info <- find_color_aes(layer)
    if (!is.null(color_info)) break
  }
  
  if (is.null(color_info)) {
    stop("No color aesthetic found in the plot")
  }
  
  color_var <- rlang::as_name(color_info$aes)
  color_aes_name <- color_info$name
  
  # Extract the data from the plot
  plot_data <- plot$data
  if (is.null(plot_data)) {
    plot_data <- layer$data
  }
  
  # Get unique values of the color variable
  unique_values <- unique(plot_data[[color_var]])
  unique_values <- unique_values[!is.na(unique_values)]
  
  # Add the new label
  unique_values <- c(unique_values, new_label)
  
  # Generate colors if needed
  n_colors_needed <- length(unique_values)
  existing_scale <- layer_scales(plot)[[color_aes_name]]
  
  if (is.null(existing_scale) || is.null(existing_scale$palette)) {
    # If no existing scale, generate new colors
    all_colors <- hue_pal()(n_colors_needed)
  } else {
    # Try to get existing colors, fill with new ones if not enough
    existing_colors <- existing_scale$palette(n_colors_needed - 1)
    if (length(existing_colors) < n_colors_needed - 1) {
      additional_colors <- hue_pal()(n_colors_needed - 1 - length(existing_colors))
      existing_colors <- c(existing_colors, additional_colors)
    }
    all_colors <- c(existing_colors, new_color)
  }
  
  # Ensure color variable is a factor
  plot_data[[color_var]] <- factor(plot_data[[color_var]], levels = unique_values)
  
  # Update the plot
  plot +
    scale_color_manual(values = setNames(all_colors, unique_values)) +
    guides(color = guide_legend(override.aes = list(
      shape = c(rep(NA, n_colors_needed - 1), new_shape)
    )))
}

# Unbalanced Panel ---------------------------------------------------------------------------

# overall effects --- 

# main results 
results_main <- did_analysis("groupID1", "02_merged_bigcell_year", "0.1° Cell x Year", "y", 0, 0, 0, 0, 0, 0)
count_main <- counts_analysis("groupID1", "02_merged_bigcell_year", "0.1° Cell x Year", "y", 0, 0, 0, 0, 0, 0)
nobs_main <- nobs_analysis("groupID1", "02_merged_bigcell_year", "0.1° Cell x Year", "y", 0, 0, 0, 0, 0, 0)

# zip code results 
results_zip <- did_analysis("zip", "02_merged_zip_year", "Zip Code x Year", "y", 0, 0, 0, 0, 0, 0)
count_zip <- counts_analysis("zip", "02_merged_zip_year", "Zip Code x Year", "y", 0, 0, 0, 0, 0, 0)
nobs_zip <- nobs_analysis("zip", "02_merged_zip_year", "Zip Code x Year", "y", 0, 0, 0, 0, 0, 0)

# merge 
results <- rbind(results_main, results_zip)
results <- results %>% mutate(time = ifelse(version == "0.1° Cell x Year", 0, 
                                            ifelse(version == "Zip Code x Year", 1, 2))) %>% arrange(time)

# dynamic effects --- 

# all
load("data/processed/02_merged_bigcell_year.rda")
data <- data %>% mutate(treat = treatedAny, 
                        firstY = year(firstEffect))

# keep relevant variables 
dataMain <- data %>% filter(type %in% c("charge", "complete", "partial", "control"))
rm(data)

# variables for estimation 
dataMain <- dataMain %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataMain <- dataMain %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataMain <- dataMain %>% mutate(timeToTreat = ifelse(treatEver == 1, year - firstY, 0))
dataMain <- dataMain %>% mutate(firstYSA = ifelse(treatEver == 0, 10000, firstY+1))
dataMain <- dataMain %>% mutate(treatDCDH = ifelse(treat == 1 & year >= firstY, 1, 0))
dataMain <- dataMain %>% mutate(firstYCS = ifelse(is.na(firstY), 0, firstY+1))

# keep final needed variables 
dataMain <- dataMain %>% dplyr::select(type, zip, groupID1, county, year, treat, treatEver, treatDCDH, firstY, firstYCS, firstYSA, timeToTreat, people, adults, children, totalItems, miles, cleanupCount, itemsPP, itemsPPPM, plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, lastPolicyID, firstPolicyID)
dataMain <- dataMain %>% ungroup()
dataMain <- dataMain %>% arrange(groupID1, year)

# TWFE 
dynamicTWFE <- feols(percPlasticBag ~ i(timeToTreat, treatEver, ref = 0) | groupID1 + year,  cluster = ~groupID1, data = dataMain)
dynamicTWFE_results <- tidy(dynamicTWFE, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Grocery Bags,\n% of Items")
dynamicTWFE_results$term <- as.numeric(str_extract(dynamicTWFE_results$term, "(?<=timeToTreat::).*?(?=:treatEver)"))
dynamicTWFE_results <- dynamicTWFE_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicTWFE_results$estimate, "type") <- NULL
attr(dynamicTWFE_results$se, "type") <- NULL
dynamicTWFE_results <- dynamicTWFE_results %>% filter(time >= -3 & time <= 6)
dynamicTWFE_results <- rbind(dynamicTWFE_results, c("TWFE", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicTWFE_results <- dynamicTWFE_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicTWFE_results <- dynamicTWFE_results %>% arrange(time)
rm(dynamicTWFE)

# Callaway and Sant'anna 2021
atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = "year",  idname = "groupID1", gname = "firstYCS", clustervars = "zip",
               control_group = "notyettreated", 
               bstrap = TRUE, biters = 1000, 
               allow_unbalanced_panel = T, panel = T, base_period = "universal") 
agg_es <- aggte(atts, type = "dynamic")
dynamicCS_results <- tidy(agg_es, conf.int=TRUE) %>% mutate(model = "Callaway and Santanna (2021)", item = "Grocery Bags,\n% of Items")
dynamicCS_results <- dynamicCS_results %>% dplyr::select(model, item, time = event.time, estimate, se = std.error, confLow = point.conf.low, confHigh = point.conf.high)
dynamicCS_results <- dynamicCS_results %>% mutate(time = time + 1)
dynamicCS_results <- dynamicCS_results %>% mutate(estimate = ifelse(time == 0, 0, estimate), se = ifelse(time == 0, 0, se), confLow = ifelse(time == 0, 0, confLow), confHigh = ifelse(time == 0, 0, confHigh))
dynamicCS_results <- dynamicCS_results %>% filter(time >= -3 & time <= 6)
rm(atts, agg_es)

# Chaisemartin and D’Haultfœuille 2020
dynamicDcdh <- did_multiplegt_dyn(df = dataMain, outcome = "percPlasticBag", time = "year", group = "groupID1", treatment = "treatDCDH", cluster = "zip",  effects = 7, placebo = 4)
dynamicDcdh_results_p1 <- data.frame(dynamicDcdh$results$Effects) %>% mutate(time = row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Grocery Bags,\n% of Items") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p1) <- NULL
dynamicDcdh_results_p2 <- data.frame(dynamicDcdh$results$Placebos)%>% mutate(time = -row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Grocery Bags,\n% of Items") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p2) <- NULL
dynamicDcdh_results <- rbind(dynamicDcdh_results_p1, dynamicDcdh_results_p2)
rm(dynamicDcdh, dynamicDcdh_results_p1, dynamicDcdh_results_p2)
dynamicDcdh_results <- rbind(dynamicDcdh_results, c("de Chaisemartin and D'Haultfoeuille (2020)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicDcdh_results <- dynamicDcdh_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicDcdh_results <- dynamicDcdh_results %>% filter(time >= -3 & time <= 6) %>% arrange(time)

# Sun and Abraham 2020 
dynamicSA <- feols(percPlasticBag ~ sunab(firstYSA, year) | groupID1 + year,  cluster = ~groupID1, data = dataMain)
dynamicSA_results <- tidy(dynamicSA, conf.int=TRUE) %>% mutate(model = "Sun and Abraham (2021)", item = "Grocery Bags,\n% of Items")
dynamicSA_results$term <- as.numeric(str_extract(dynamicSA_results$term, "(?<=year::).*"))+1
dynamicSA_results <- dynamicSA_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicSA_results$estimate, "type") <- NULL
attr(dynamicSA_results$se, "type") <- NULL
dynamicSA_results <- dynamicSA_results %>% filter(time >= -3 & time <= 6)
dynamicSA_results <- rbind(dynamicSA_results, c("Sun and Abraham (2021)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicSA_results <- dynamicSA_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicSA_results <- dynamicSA_results %>% arrange(time)
rm(dynamicSA)

# combine dynamic results 
dynamic_results <- rbind(dynamicTWFE_results, dynamicCS_results, dynamicDcdh_results, dynamicSA_results)

# control mean 
control_mean <- dataMain %>% filter(treatEver == 0) %>% summarise(control_mean = mean(percPlasticBag))
control_mean <- control_mean$control_mean
dynamic_results <- dynamic_results %>% mutate(estimate = estimate/control_mean*100, se = se/control_mean*100, confLow = confLow/control_mean*100, confHigh = confHigh/control_mean*100)
dynamic_results <- dynamic_results %>% mutate(confLow90 = estimate - 1.645*se, confHigh90 = estimate + 1.645*se)
dynamic_results <- dynamic_results %>% mutate(model = factor(model, levels = c("TWFE", "Callaway and Santanna (2021)", "de Chaisemartin and D'Haultfoeuille (2020)", "Sun and Abraham (2021)")))
dynamic_results <- dynamic_results %>% filter(time < 6)

# cleanups and nobs 
cleanups <- sum(dataMain$cleanupCount)
nobs <- nrow(dataMain)

# Balanced Panel ---------------------------------------------------------------------------

# overall effects 

# main results 
results_main <- did_analysis("groupID1", "02_merged_bigcell_year_balanced", "0.1° Cell x Year", "y", 1, 0, 0, 0, 0, 0)
count_main_balanced <- counts_analysis("groupID1", "02_merged_bigcell_year_balanced", "0.1° Cell x Year", "y", 1, 0, 0, 0, 0, 0)
nobs_main_balanced <- nobs_analysis("groupID1", "02_merged_bigcell_year_balanced", "0.1° Cell x Year", "y", 1, 0, 0, 0, 0, 0)

# zip code results 
results_zip <- did_analysis("zip", "02_merged_zip_year_balanced", "Zip Code x Year", "y", 1, 0, 0, 0, 0, 0)
count_zip_balanced <- counts_analysis("zip", "02_merged_zip_year_balanced", "Zip Code x Year", "y", 1, 0, 0, 0, 0, 0)
nobs_zip_balanced <- nobs_analysis("zip", "02_merged_zip_year_balanced", "Zip Code x Year", "y", 1, 0, 0, 0, 0, 0)

# merge 
results_balanced <- rbind(results_main, results_zip)
results_balanced <- results_balanced %>% mutate(time = ifelse(version == "0.1° Cell x Year", 0, 
                                            ifelse(version == "Zip Code x Year", 1, 2))) %>% arrange(time)
# dynamic effects --- 

# all
load("data/processed/02_merged_bigcell_year_balanced.rda")
data <- dataBalanced %>% mutate(treat = treatedAny, 
                        firstY = year(firstEffect))

# keep relevant variables 
dataMain <- data %>% filter(type %in% c("charge", "complete", "partial", "control"))
rm(data)

# variables for estimation 
dataMain <- dataMain %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataMain <- dataMain %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataMain <- dataMain %>% mutate(timeToTreat = ifelse(treatEver == 1, year - firstY, 0))
dataMain <- dataMain %>% mutate(firstYSA = ifelse(treatEver == 0, 10000, firstY+1))
dataMain <- dataMain %>% mutate(treatDCDH = ifelse(treat == 1 & year >= firstY, 1, 0))
dataMain <- dataMain %>% mutate(firstYCS = ifelse(is.na(firstY), 0, firstY+1))

# keep final needed variables 
dataMain <- dataMain %>% dplyr::select(type, zip, groupID1, county, year, treat, treatEver, treatDCDH, firstY, firstYCS, firstYSA, timeToTreat, people, adults, children, totalItems, miles, cleanupCount, itemsPP, itemsPPPM, plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, lastPolicyID, firstPolicyID)
dataMain <- dataMain %>% ungroup()
dataMain <- dataMain %>% arrange(groupID1, year)

# TWFE 
dynamicTWFE <- feols(percPlasticBag ~ i(timeToTreat, treatEver, ref = 0) | groupID1 + year,  cluster = ~groupID1, data = dataMain)
dynamicTWFE_results <- tidy(dynamicTWFE, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Grocery Bags,\n% of Items")
dynamicTWFE_results$term <- as.numeric(str_extract(dynamicTWFE_results$term, "(?<=timeToTreat::).*?(?=:treatEver)"))
dynamicTWFE_results <- dynamicTWFE_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicTWFE_results$estimate, "type") <- NULL
attr(dynamicTWFE_results$se, "type") <- NULL
dynamicTWFE_results <- dynamicTWFE_results %>% filter(time >= -3 & time <= 6)
dynamicTWFE_results <- rbind(dynamicTWFE_results, c("TWFE", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicTWFE_results <- dynamicTWFE_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicTWFE_results <- dynamicTWFE_results %>% arrange(time)
rm(dynamicTWFE)

# Callaway and Sant'anna 2021
atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = "year",  idname = "groupID1", gname = "firstYCS", clustervars = "zip",
               control_group = "notyettreated", 
               bstrap = TRUE, biters = 1000, 
               allow_unbalanced_panel = T, panel = T, base_period = "universal") 
agg_es <- aggte(atts, type = "dynamic")
dynamicCS_results <- tidy(agg_es, conf.int=TRUE) %>% mutate(model = "Callaway and Santanna (2021)", item = "Grocery Bags,\n% of Items")
dynamicCS_results <- dynamicCS_results %>% dplyr::select(model, item, time = event.time, estimate, se = std.error, confLow = point.conf.low, confHigh = point.conf.high)
dynamicCS_results <- dynamicCS_results %>% mutate(time = time + 1)
dynamicCS_results <- dynamicCS_results %>% mutate(estimate = ifelse(time == 0, 0, estimate), se = ifelse(time == 0, 0, se), confLow = ifelse(time == 0, 0, confLow), confHigh = ifelse(time == 0, 0, confHigh))
dynamicCS_results <- dynamicCS_results %>% filter(time >= -3 & time <= 6)
rm(atts, agg_es)

# Chaisemartin and D’Haultfœuille 2020
dynamicDcdh <- did_multiplegt_dyn(df = dataMain, outcome = "percPlasticBag", time = "year", group = "groupID1", treatment = "treatDCDH", cluster = "zip",  effects = 7, placebo = 4)
dynamicDcdh_results_p1 <- data.frame(dynamicDcdh$results$Effects) %>% mutate(time = row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Grocery Bags,\n% of Items") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p1) <- NULL
dynamicDcdh_results_p2 <- data.frame(dynamicDcdh$results$Placebos)%>% mutate(time = -row_number(),  model = "de Chaisemartin and D'Haultfoeuille (2020)", item = "Grocery Bags,\n% of Items") %>% dplyr::select(model, item, time, estimate = Estimate, se = SE, confLow = LB.CI, confHigh = UB.CI)
row.names(dynamicDcdh_results_p2) <- NULL
dynamicDcdh_results <- rbind(dynamicDcdh_results_p1, dynamicDcdh_results_p2)
rm(dynamicDcdh, dynamicDcdh_results_p1, dynamicDcdh_results_p2)
dynamicDcdh_results <- rbind(dynamicDcdh_results, c("de Chaisemartin and D'Haultfoeuille (2020)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicDcdh_results <- dynamicDcdh_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicDcdh_results <- dynamicDcdh_results %>% filter(time >= -3 & time <= 6) %>% arrange(time)

# Sun and Abraham 2020 
dynamicSA <- feols(percPlasticBag ~ sunab(firstYSA, year) | groupID1 + year,  cluster = ~groupID1, data = dataMain)
dynamicSA_results <- tidy(dynamicSA, conf.int=TRUE) %>% mutate(model = "Sun and Abraham (2021)", item = "Grocery Bags,\n% of Items")
dynamicSA_results$term <- as.numeric(str_extract(dynamicSA_results$term, "(?<=year::).*"))+1
dynamicSA_results <- dynamicSA_results %>% dplyr::select(model, item, time = term, estimate, se = std.error, confLow = conf.low, confHigh = conf.high)
attr(dynamicSA_results$estimate, "type") <- NULL
attr(dynamicSA_results$se, "type") <- NULL
dynamicSA_results <- dynamicSA_results %>% filter(time >= -3 & time <= 6)
dynamicSA_results <- rbind(dynamicSA_results, c("Sun and Abraham (2021)", "Grocery Bags,\n% of Items", 0, 0, 0, 0, 0))
dynamicSA_results <- dynamicSA_results %>% mutate(time = as.numeric(time), estimate = as.numeric(estimate), se = as.numeric(se), confLow = as.numeric(confLow), confHigh = as.numeric(confHigh))
dynamicSA_results <- dynamicSA_results %>% arrange(time)
rm(dynamicSA)

# combine dynamic results 
dynamic_results_balanced <- rbind(dynamicTWFE_results, dynamicCS_results, dynamicDcdh_results, dynamicSA_results)

# control mean 
control_mean <- dataMain %>% filter(treatEver == 0) %>% summarise(control_mean = mean(percPlasticBag))
control_mean <- control_mean$control_mean
dynamic_results_balanced <- dynamic_results_balanced %>% mutate(estimate = estimate/control_mean*100, se = se/control_mean*100, confLow = confLow/control_mean*100, confHigh = confHigh/control_mean*100)
dynamic_results_balanced <- dynamic_results_balanced %>% mutate(confLow90 = estimate - 1.645*se, confHigh90 = estimate + 1.645*se)
dynamic_results_balanced <- dynamic_results_balanced %>% mutate(model = factor(model, levels = c("TWFE",  "Callaway and Santanna (2021)", "de Chaisemartin and D'Haultfoeuille (2020)", "Sun and Abraham (2021)")))
dynamic_results_balanced <- dynamic_results_balanced %>% filter(time < 6)

# cleanups and nobs 
cleanups_balanced <- sum(dataMain$cleanupCount)
nobs_balanced <- nrow(dataMain)

# Plots --------------------------------------------------------------------------------

# overall, unbalanced
a <- results %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.25, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 3, position = position_dodge(width = 0.5), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 4, position = position_dodge(width = 0.5)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = "Aggregation Level", title=paste0("Overall Effects"), subtitle = "Unbalanced Panel") +
  scale_color_manual(name = "Estimator", values = palette) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  # Add this line
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) +  # Add this line
  scale_x_continuous(limits = c(-0.5, 1.5), breaks = c(0, 1), labels = c(paste0("0.1° Cell x Year\n Cleanups = ", comma(count_main), "\n Obs = ", comma(nobs_main)), paste0("Zip Code x Year\n Cleanups = ", comma(count_zip), "\n Obs = ", comma(nobs_zip))))+#, paste0("0.01° Cell x Month\n Cleanups = ", comma(count_month), "\n Obs = ", comma(nobs_month)))) +
  scale_y_continuous(breaks = seq(-100, 50, by = 25), limits=c(-100,50)) +
  add_common_theme
a <- a + guides(color= guide_legend(nrow=3))

# extract legend
legend_paper <- get_legend(a + guides(color = guide_legend(nrow = 3, keywidth = 1,  override.aes = list(shape = c(16, 17, 4, 15, 1),linewidth = 0, alpha = 1))))

# dynamic, unbalanced 
b <- dynamic_results %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.5, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 1.5, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 2, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = "Years Since Policy", title=paste0("Dynamic Effects"), subtitle=paste0("0.1° Cell x Year Aggregation; Unbalanced Panel")) + #\n Cleanups = ", comma(nobs), "; Observations = ", comma(cleanups))) +
  scale_color_manual(name = "Estimator", values = palette) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15)) +  
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15)))) +  
  scale_x_continuous(breaks = seq(-3, 5, by = 1), limits=c(-3.5,5.5)) +
  scale_y_continuous(breaks = seq(-125, 75, by = 25), limits=c(-127.5,85)) +
  add_common_theme

# overall, balanced
c <- results_balanced %>% ggplot(aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.25, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 3, position = position_dodge(width = 0.5), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 4, position = position_dodge(width = 0.5)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = "Aggregation Level", title=paste0("Overall Effects"), subtitle = "Balanced Panel") +
  scale_color_manual(name = "Estimator", values = palette) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  # Add this line
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) +  # Add this line
  scale_x_continuous(limits = c(-0.5, 1.5), breaks = c(0, 1), labels = c(paste0("0.1° Cell x Year\n Cleanups = ", comma(count_main_balanced), "\n Obs = ", comma(nobs_main_balanced)), paste0("Zip Code x Year\n Cleanups = ", comma(count_zip_balanced), "\n Obs = ", comma(nobs_zip_balanced))))+#, comma(nobs_zip_balanced)), "")) +
  scale_y_continuous(breaks = seq(-100, 50, by = 25), limits=c(-100,50)) +
  add_common_theme

# plot dynamic results
d <- dynamic_results_balanced %>% ggplot(aes(x = time, y = estimate)) +
  #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "gray95", alpha = 0.5) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(color = model, ymin = confLow, ymax = confHigh), width = 0.5, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(color = model, ymin = confLow90, ymax = confHigh90), size = 1.5, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(color = model, shape=model), size = 2, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = "Years Since Policy", title=paste0("Dynamic Effects"), subtitle=paste0("0.1° Cell x Year Aggregation; Balanced Panel")) + #\n Cleanups = ", comma(nobs_balanced), "; Observations = ", comma(cleanups_balanced))) +
  scale_color_manual(name = "Estimator", values = palette) + 
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15)) +  
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 4, 15)))) +  
  scale_x_continuous(breaks = seq(-3, 5, by = 1), limits=c(-3.5,5.5)) +
  scale_y_continuous(breaks = seq(-125, 75, by = 25), limits=c(-127.5,85)) +
  add_common_theme

# get rid of individual legends
a <- a + theme(legend.position = "none")
c <- c + theme(legend.position = "none")
b <- b + theme(legend.position = "none")
d <- d + theme(legend.position = "none")

# arrange plots using ggarrange
arranged_plots <- ggarrange(
  ggarrange(a, NA, c, ncol = 3, labels = c("A",  "", "C"), widths = c(2, 0.25, 2)),
  NA,
  ggarrange(b, NA, d, ncol = 3, labels = c("B", "", "D"), widths = c(2, 0.25, 2)),
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

ggsave("output/Figure2.jpeg", final_plot, width = 11, height = 10, unit = 'in')

