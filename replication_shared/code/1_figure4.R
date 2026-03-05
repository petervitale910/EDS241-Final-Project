#############################################################################################
# Heterogeneity - FIGURE 4
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
pacman::p_load(data.table, ggplot2, sf, dplyr, tidyr, broom, stargazer, stringr, scales, usmap, tigris, gridExtra,ggpubr, PNWColors, lfe, fixest, did, didimputation, DIDmultiplegt, DIDmultiplegtDYN, grid, dotwhisker)    

## check versions
packageVersion("DIDmultiplegt")
packageVersion("DIDmultiplegtDYN")

# Figure Formatting --------------------------------------------------------------------------

## size and colors
textSize <- 15
pal_shuksan <- pnw_palette("Shuksan", 12)
pal_bay <- pnw_palette("Bay", 5)
pal_fill <- c(pal_shuksan[c(1:3, 5:7, 10:12)])
pal_type <- pal_fill[c(2, 5, 8)]
pal_geo <- pal_bay[c(1, 2, 5)]

## plot theme 
add_common_theme_coef <-theme(legend.position = "bottom", 
        axis.text.x=element_blank(), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize-2, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-7), 
        legend.title=element_text(size=textSize-6), 
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

add_common_theme_bar <- theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize-4), 
        axis.text.y=element_text(size = textSize-4), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize-4, margin=margin(r=-2.5, t = 20)),
        legend.text=element_text(size=textSize), 
        legend.title=element_text(size=textSize), 
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm"), 
        panel.background = element_rect(fill='grey89', colour='grey89'))

### PART 1: BY POLICY --------------------------------------------------------------------------------

# Function ---

did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, policy_type) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else {
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
  } else {
    stop("Invalid time_aggregation. Choose 'y' or 'ym'.")
  }
  
  # Keep those with same first and last policy type and policy geo 
  dataMain <- data %>% filter((lastPolicyType == firstPolicyType) | (is.na(lastPolicyType) & is.na(firstPolicyType))) 
  
  # Filter based
  if (policy_type == "ban") {
    dataMain <- dataMain %>% filter(type %in% c("complete", "control"))
  } else if (policy_type == "tax") {
    dataMain <- dataMain %>% filter(type %in% c("charge", "control"))
  } else if (policy_type == "partial") {
    dataMain <- dataMain %>% filter(type %in% c("partial", "control"))
  } else if (policy_type == "all") {
    dataMain <- dataMain %>% filter(type %in% c("complete", "partial", "charge", "control"))
  } else {
    stop("Invalid policy_type. Choose 'ban', 'tax', or 'partial'.")
  }
  
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
  control_mean <- dataMain %>% 
    filter(treat == 0) %>% 
    summarise(mean = mean(percPlasticBag, na.rm = TRUE)) %>%
    pull(mean)
  
  # TWFE model
  formula_twfe <- as.formula(paste("percPlasticBag ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate / control_mean, 
           std.error = std.error / control_mean) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Callaway and Sant'anna 2021
  atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = time_var,  
                 idname = group_id_var, gname = "firstCS", clustervars = "zip",
                 control_group = "notyettreated",
                 bstrap = TRUE, biters = 1000,
                 allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
  agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
  cs_results <- c('Callaway and Santanna (2021)', item_description, 
                  (agg_effects$overall.att / control_mean), (agg_effects$overall.se / control_mean))
  results_model <- rbind(results_model, cs_results)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "percPlasticBag", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect / control_mean, dcdh$se_effect / control_mean)
  results_model <- rbind(results_model, dcdh_results)
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("percPlasticBag ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] / control_mean), (sa[2] / control_mean))
  results_model <- rbind(results_model, sa_results)
  
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
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
                                            "Borusyak et al. (2021)")))
  
  return(results_model)
}

# Get Results ---

# main results 
results_main <- did_analysis("groupID1", "02_merged_bigcell_year", "All", "y", 0, "all")

results_ban <- did_analysis("groupID1", "02_merged_bigcell_year", "Ban", "y", 0, "ban")

results_partial <- did_analysis("groupID1", "02_merged_bigcell_year", "Partial Ban", "y", 0, "partial")

results_tax <- did_analysis("groupID1", "02_merged_bigcell_year", "Fee", "y", 0, "tax")

# combine 
results <- rbind(results_main, results_ban, results_partial, results_tax)
results <- results %>% mutate(x = ifelse(version == "All", 0, 
                                         ifelse(version == "Ban", 1, 
                                                ifelse(version == "Partial Ban", 2, 
                                                       ifelse(version == "Fee", 3, NA)))))
results <- results %>% mutate(version = factor(version, levels = c("All", "Ban", "Partial Ban", "Fee")))

# get number of treated cells 
load(paste0("data/processed/02_merged_bigcell_year.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial", "control")) 
data <- data %>% filter((lastPolicyType == firstPolicyType) | (is.na(lastPolicyType) & is.na(firstPolicyType)))
data <- data %>% distinct(firstPolicyType, groupID1, year)
data <- data %>% mutate(typeGeo = ifelse(firstPolicyType == "complete_bag_ban", "Ban", 
                                         ifelse(firstPolicyType == "partial_bag_ban", "Partial Ban", 
                                                ifelse(firstPolicyType == "plastic_bag_charge", "Fee", "All")))) %>% group_by(typeGeo) %>% summarise(sum = n())
data <- data %>% filter(!is.na(typeGeo)) 
sum <- sum(data$sum)
add <- c("All", sum)
data <- rbind(data, add)
data <- data %>% mutate(sum = as.numeric(sum))
data <- data %>% mutate(typeGeo = factor(typeGeo, levels = c("All", "Ban", "Partial Ban", "Fee")))

# Plot ---

# color palette
pal <- pnw_palette("Bay", 5)
pal <- c('black', pal_geo[1], pal_geo[2], pal_geo[3], pal_geo[4])

# plot 
b <- results %>% ggplot(aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(shape = model, ymin = confLow, ymax = confHigh, color=version), width = 0.25, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(shape = model , ymin = confLow90, ymax = confHigh90, color=version), size = 3, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(shape= model, color=version), size = 3.5, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = " ", title= "Policy Type") +
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  
  scale_color_manual(name = "Group", values = pal) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-250, 100, by = 50), limits = c(-290, 125)) +
  scale_x_continuous(breaks = seq(0, 3, 1), labels = c(" ", " ", " ", " "), limits=c(-0.5,3.5)) + 
  add_common_theme_coef
b <- b + guides(shape= guide_legend(nrow=3), color="none")
legend <- get_legend(b)
b <- b + theme(legend.position = "none")

# now bar graph from size 
c <- ggplot(data, aes(x = typeGeo, y = sum, fill = typeGeo)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=pal, guide = "none") +
  theme_bw() + xlab(" ") + ylab("Treated Obs.\n(Cell x Year)") +
  scale_y_continuous(breaks=c(0, 700, 1400, 2100))+ 
  add_common_theme_bar
plot_type <- ggarrange(b, NULL, c, ncol = 1, align = "v", heights = c(2.25, -0.275, 1))

### PART 2: BY GEO --------------------------------------------------------------------------------

# Function ---

did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, geo_type) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else {
  }
  
  # Data preparation based on time_aggregation
  if (time_aggregation == "y") {
    data <- data %>% 
      mutate(treat = treatedAny,
             firstY = year(firstEffect), 
             y = year)
    time_var <- "y"
  } else {
    stop("Invalid time_aggregation. Choose 'y'.")
  }
  
  # Keep those with same first and last policy type and policy geo 
  dataMain <- data %>% filter((lastPolicyGeo == firstPolicyGeo) | (is.na(lastPolicyGeo) & is.na(firstPolicyGeo))) 
  
  # Filter based
  if (geo_type == "state") {
    dataMain <- dataMain %>% filter(firstPolicyGeo == "state" | (is.na(firstPolicyGeo))) %>% filter(type %in% c("complete", "partial", "charge", "control")) 
  } else if (geo_type == "county") {
    dataMain <- dataMain %>% filter(firstPolicyGeo == "county" | (is.na(firstPolicyGeo))) %>% filter(type %in% c("complete", "partial", "charge", "control")) 
  } else if (geo_type == "town") {
    dataMain <- dataMain %>% filter(firstPolicyGeo == "town" | (is.na(firstPolicyGeo))) %>% filter(type %in% c("complete", "partial", "charge", "control")) 
  } else if (geo_type == "all") {
    dataMain <- dataMain %>% filter(type %in% c("complete", "partial", "charge", "control")) 
  } else {
    stop("Invalid policy_type. Choose 'state', 'county', or 'town'.")
  }
  
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
  control_mean <- dataMain %>% 
    filter(treat == 0) %>% 
    summarise(mean = mean(percPlasticBag, na.rm = TRUE)) %>%
    pull(mean)
  
  # TWFE model
  formula_twfe <- as.formula(paste("percPlasticBag ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate / control_mean, 
           std.error = std.error / control_mean) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Callaway and Sant'anna 2021
  atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = time_var,  
                 idname = group_id_var, gname = "firstCS", clustervars = "zip",
                 control_group = "notyettreated",
                 bstrap = TRUE, biters = 1000,
                 allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
  agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
  cs_results <- c('Callaway and Santanna (2021)', item_description, 
                  (agg_effects$overall.att / control_mean), (agg_effects$overall.se / control_mean))
  results_model <- rbind(results_model, cs_results)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "percPlasticBag", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect / control_mean, dcdh$se_effect / control_mean)
  results_model <- rbind(results_model, dcdh_results)
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("percPlasticBag ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] / control_mean), (sa[2] / control_mean))
  results_model <- rbind(results_model, sa_results)
  
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
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
    mutate(model = factor(model, levels = c("TWFE", "Callaway and Santanna (2021)", 
                                            "de Chaisemartin and D'Haultfoeuille (2020)", 
                                            "Sun and Abraham (2021)", 
                                            "Borusyak et al. (2021)")))
  
  return(results_model)
}

# Get Results ---

# main results 
results_main <- did_analysis("groupID1", "02_merged_bigcell_year", "All", "y", 0, "all")

results_state <- did_analysis("groupID1", "02_merged_bigcell_year", "State", "y", 0, "state")

results_county <- did_analysis("groupID1", "02_merged_bigcell_year", "County", "y", 0, "county")

results_town <- did_analysis("groupID1", "02_merged_bigcell_year", "Town", "y", 0, "town")

# combine 
results <- rbind(results_main, results_state, results_county, results_town)
results <- results %>% mutate(x = ifelse(version == "All", 0, 
                                         ifelse(version == "State", 1, 
                                                ifelse(version == "County", 2, 
                                                       ifelse(version == "Town", 3, NA)))))
results <- results %>% mutate(version = factor(version, levels = c("All", "State", "County", "Town")))

# get number of treated cells 
load(paste0("data/processed/02_merged_bigcell_year.rda"))
data <- data %>% filter(type %in% c("complete", "partial", "charge", "control")) 
data <- data %>% filter((lastPolicyGeo == firstPolicyGeo) | (is.na(lastPolicyGeo) & is.na(firstPolicyGeo) ))
data <- data %>% distinct(firstPolicyGeo, groupID1, year)
data <- data %>% mutate(typeGeo = ifelse(firstPolicyGeo == "state", "State", 
                                         ifelse(firstPolicyGeo == "county", "County", 
                                                ifelse(firstPolicyGeo == "town", "Town", "All")))) %>% group_by(typeGeo) %>% summarise(sum = n())
data <- data %>% filter(!is.na(typeGeo)) 
sum <- sum(data$sum)
add <- c("All", sum)
data <- rbind(data, add)
data <- data %>% mutate(sum = as.numeric(sum))
data <- data %>% mutate(typeGeo = factor(typeGeo, levels = c("All", "State", "County", "Town")))

# Plot ---

# color palette
pal <- pnw_palette("Shuksan", 3)
pal <- c("black", pal[1], pal[2], pal[3])

# plot 
b <- results %>% ggplot(aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(shape = model, ymin = confLow, ymax = confHigh, color=version), width = 0.25, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(shape = model, ymin = confLow90, ymax = confHigh90, color=version), size = 3, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(shape=model, color=version), size = 3.5, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = " ", title= "Geographic Scope") +
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  
  scale_color_manual(name = "Group", values = pal) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-250, 100, by = 50), limits = c(-290, 125)) +
  scale_x_continuous(breaks = seq(0, 3, 1), labels = c(" ", " ", " ", " "), limits=c(-0.5,3.5)) + 
  add_common_theme_coef
b <- b + guides(shape= guide_legend(nrow=3), color="none")
legend <- get_legend(b)
b <- b + theme(legend.position = "none")

# now bar graph from size 
c <- ggplot(data, aes(x = typeGeo, y = sum, fill = typeGeo)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=pal, guide = "none") +
  theme_bw() + xlab(" ") + ylab("Treated Obs.\n(Cell x Year)") +
  scale_y_continuous(breaks=c(0, 700, 1400, 2100))+ 
  add_common_theme_bar

plot_geo <- ggarrange(b, NULL, c, ncol = 1, align = "v", heights = c(2.25, -0.275, 1))

### PART 3: BY LOCATION --------------------------------------------------------------------------------

# Function ---
did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else {
  }
  
  # Data preparation based on time_aggregation
  if (time_aggregation == "y") {
    data <- data %>% 
      mutate(treat = treatedAny,
             firstY = year(firstEffect), 
             y = year)
    time_var <- "y"
  } else {
    stop("Invalid time_aggregation. Choose 'y'.")
  }
  
  # Keep those with same first and last policy type and policy geo 
  dataMain <- data %>% filter(type %in% c("complete", "partial", "charge", "control"))  
    
  # Filter based
  dataMain <- dataMain
  
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
  control_mean <- dataMain %>% 
    filter(treat == 0) %>% 
    summarise(mean = mean(percPlasticBag, na.rm = TRUE)) %>%
    pull(mean)
  
  # TWFE model
  formula_twfe <- as.formula(paste("percPlasticBag ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate / control_mean, 
           std.error = std.error / control_mean) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Callaway and Sant'anna 2021
  atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = time_var,  
                 idname = group_id_var, gname = "firstCS", clustervars = "zip",
                 control_group = "notyettreated",
                 bstrap = TRUE, biters = 1000,
                 allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
  agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
  cs_results <- c('Callaway and Santanna (2021)', item_description, 
                  (agg_effects$overall.att / control_mean), (agg_effects$overall.se / control_mean))
  results_model <- rbind(results_model, cs_results)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "percPlasticBag", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect / control_mean, dcdh$se_effect / control_mean)
  results_model <- rbind(results_model, dcdh_results)
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("percPlasticBag ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] / control_mean), (sa[2] / control_mean))
  results_model <- rbind(results_model, sa_results)
  
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
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
    mutate(model = factor(model, levels = c("TWFE", "Callaway and Santanna (2021)", 
                                            "de Chaisemartin and D'Haultfoeuille (2020)", 
                                            "Sun and Abraham (2021)", 
                                            "Borusyak et al. (2021)")))
  
  return(results_model)
}

# Get Results ---

# main results 
results_main <- did_analysis("groupID1", "02_merged_bigcell_year", "All", "y", 0)

results_coast <- did_analysis("groupID1", "02_merged_bigcell_year_coast", "Coast", "y", 0)

results_river <- did_analysis("groupID1", "02_merged_bigcell_year_river", "Rivers", "y", 0)

results_lake <- did_analysis("groupID1", "02_merged_bigcell_year_lake", "Lakes", "y", 0)

# combine 
results <- rbind(results_main, results_coast, results_river, results_lake)
results <- results %>% mutate(x = ifelse(version == "All", 0, 
                                         ifelse(version == "Coast", 1, 
                                                ifelse(version == "Rivers", 2, 
                                                       ifelse(version == "Lakes", 3, NA)))))
results <- results %>% mutate(version = factor(version, levels = c("All", "Coast", "Rivers", "Lakes")))

# get number of treated cell x year combinations for each 
load(paste0("data/processed/02_merged_bigcell_year.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
all <- nrow(data)
load(paste0("data/processed/02_merged_bigcell_year_coast.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
coast <- nrow(data)
load(paste0("data/processed/02_merged_bigcell_year_river.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
river <- nrow(data)
load(paste0("data/processed/02_merged_bigcell_year_river_10km.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
river10km <- nrow(data)
load(paste0("data/processed/02_merged_bigcell_year_lake.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
lake <- nrow(data)
load(paste0("data/processed/02_merged_bigcell_year_lake_10km.rda"))
data <- data %>% filter(type %in% c("charge", "complete", "partial")) 
lake10km <- nrow(data)

data <- data.frame(typeGeo = c("All", "Coast", "Rivers", "Lakes"), sum = c(all, coast, river, lake))
data <- data %>% mutate(typeGeo = factor(typeGeo, levels = c("All", "Coast", "Rivers", "Lakes")))

# modify the data structure
data <- data.frame(
  typeGeo = c("All", "Coast", "Rivers", "Rivers", "Lakes", "Lakes"),
  category = c("Total", "Total", "<10km of Coast", ">10km of Coast", "<10km of Coast", ">10km of Coast"),
  sum = c(all, coast, river*(river10km/river), river*(1-river10km/river), lake*(lake10km/lake), lake*(1-lake10km/lake))
)

# Ensure proper ordering of factors
data$typeGeo <- factor(data$typeGeo, levels = c("All", "Coast", "Rivers", "Lakes"))
data$category <- factor(data$category, levels = c("Total", "<10km of Coast", ">10km of Coast"))

# Plot ---

# color palette
pal <- c("black", "#4059AD", "#00cbdf", "#6cfacd")

# plot 
b <- results %>% ggplot(aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(shape = model, ymin = confLow, ymax = confHigh, color=version), width = 0.25, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(shape = model, ymin = confLow90, ymax = confHigh90, color=version), size = 3, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(shape=model, color=version), size = 3.5, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = " ", title= "Cleanup Location") +
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  
  scale_color_manual(name = "Group", values = pal) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-100, 100, by = 50), limits = c(-135, 125)) +
  scale_x_continuous(breaks = seq(0, 3, 1), labels = c(" ", " ", " ", " "), limits=c(-0.5,3.5)) + 
  add_common_theme_coef
b <- b + guides(shape= guide_legend(nrow=3), color="none")
legend <- get_legend(b)
b <- b + theme(legend.position = "none")

# now bar graph from size 
c <- ggplot(data, aes(x = typeGeo, y = sum, fill = interaction(typeGeo, category))) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(name = "Type", 
                    values = c("All.Total" = "black", 
                               "Coast.Total" = "#4059AD", 
                               "Rivers.<10km of Coast" = "#00cbdf", 
                               "Rivers.>10km of Coast" = "#6ff2ff",
                               "Lakes.<10km of Coast" = "#6cfacd", 
                               "Lakes.>10km of Coast" = "#b5fce6"),
                    labels = c("All", "Coast", "Rivers", "Rivers", "Lakes", "Lakes"),
                    guide = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_bw() + 
  xlab(" ") + 
  ylab("Treated Obs.\n(Cell x Year)") + 
  add_common_theme_bar

plot_loc <- ggarrange(b, NULL, c, ncol = 1, align = "v", heights = c(2.25, -0.275, 1))

### PART 4: BY BASELINE  --------------------------------------------------------------------------------

# Function ---

did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, baseline_group) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else {
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
  } else {
    stop("Invalid time_aggregation. Choose 'y' or 'ym'.")
  }
  
  # Keep those with same first and last policy type and policy geo 
  dataMain <- data %>% filter(type %in% c("charge", "complete", "partial", "control")) 
  
  # Filter based on baseline % of plastic bags 
  dataBaseline <- dataMain %>% filter(type != "control" & treat == 0) %>% group_by(groupID1) %>% summarise(meanPlastic = mean(percPlasticBag)) %>% ungroup()
  dataBaseline <- dataBaseline %>% filter(meanPlastic > 0)
  dataBaseline <- dataBaseline %>% mutate(baseline = ifelse(meanPlastic < quantile(dataBaseline$meanPlastic, 0.50), 0, 
                                                            ifelse(meanPlastic > quantile(dataBaseline$meanPlastic, 0.75), 2, 1)))
  print(quantile(dataBaseline$meanPlastic, 0.50))
  print(quantile(dataBaseline$meanPlastic, 0.75))
  if (baseline_group == 0){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 0])
    control_mean <- dataBaseline %>% filter(baseline == 0) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 1){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 1])
    control_mean <- dataBaseline %>% filter(baseline == 1) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 2){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 2])
    control_mean <- dataBaseline %>% filter(baseline == 2) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 3) {
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline %in% c(0, 1, 2)])
    control_mean <- dataBaseline %>% filter(baseline %in% c(0, 1, 2)) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else {
    stop("Invalid baseline_group. Choose 0, 1, or 2.")
  }
  print(control_mean)
  
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
  
  print(nrow(dataMain))
  
  # Select final needed variables
  dataMain <- dataMain %>%
    dplyr::select(type, zip, !!sym(group_id_var), county, year, !!sym(time_var), treat, treatEver, treatDCDH, 
                  !!sym(paste0("first", toupper(time_var))), firstCS, firstSA, timeToTreat, 
                  people, adults, children, totalItems, miles, cleanupCount, itemsPP, itemsPPPM, 
                  plasticBag, plasticGroceryBag, plasticOtherBag, percPlasticBag, percGroceryBag, percOtherBag, 
                  plasticBagPP, plasticBagPPPM, plasticBagPM, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, 
                  percPlasticFoodCont, lastPolicyID, firstPolicyID) %>%
    arrange(!!sym(group_id_var),  !!sym(time_var))
  
  # TWFE model
  formula_twfe <- as.formula(paste("percPlasticBag ~ treat | factor(", time_var, ") + factor(", group_id_var, ") | 0 | zip"))
  model <- felm(data = dataMain, formula_twfe)
  results_model <- tidy(model, conf.int = TRUE) %>% 
    mutate(model = "TWFE", 
           item = item_description,
           estimate = estimate / control_mean, 
           std.error = std.error / control_mean) %>%
    dplyr::select(model, item, estimate, std.error)
  
  # Callaway and Sant'anna 2021
  atts <- att_gt(data = dataMain, yname = "percPlasticBag", tname = time_var,  
                 idname = group_id_var, gname = "firstCS", clustervars = "zip",
                 control_group = "notyettreated",
                 bstrap = TRUE, biters = 1000,
                 allow_unbalanced_panel = TRUE, panel = TRUE, base_period = "universal")
  agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
  cs_results <- c('Callaway and Santanna (2021)', item_description, 
                  (agg_effects$overall.att / control_mean), (agg_effects$overall.se / control_mean))
  results_model <- rbind(results_model, cs_results)
  
  # Chaisemartin and D'Haultfœuille 2020
  dcdh <- did_multiplegt(df = dataMain, Y = "percPlasticBag", T = time_var, G = group_id_var, 
                         D = "treatDCDH", cluster = "zip", brep = 100, parallel = TRUE)
  dcdh_results <- c("de Chaisemartin and D'Haultfoeuille (2020)", item_description, 
                    dcdh$effect / control_mean, dcdh$se_effect / control_mean)
  results_model <- rbind(results_model, dcdh_results)
  
  # Sun and Abraham 2020 model
  dataSA <- dataMain
  formula_sa <- as.formula(paste("percPlasticBag ~", paste0("sunab(firstSA, ", time_var, ")"), "|", paste0("factor(", group_id_var, ")"), "+", paste0("factor(", time_var, ")")))
  DynamicSA <- feols(formula_sa, cluster = "zip" , data = dataSA)
  sa <- aggregate(DynamicSA, "att")
  sa_results <- c('Sun and Abraham (2021)', item_description, 
                  (sa[1] / control_mean), (sa[2] / control_mean))
  results_model <- rbind(results_model, sa_results)
  
  
  # Borusyak et al. 2021
  if (time_aggregation == "y" & group_id_var == "groupID1") {
    borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstY",  tname = "year", idname = "groupID1", cluster_var = "zip")
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
                                            "Borusyak et al. (2021)")))
  
  return(results_model)
}

count <- function(group_id_var, file_name, item_description, time_aggregation, balanced, baseline_group) {
  
  # Load data
  load(paste0("data/processed/", file_name, ".rda"))
  
  # Rename data based on the 'balanced' parameter
  if (balanced == 1) {
    data <- dataBalanced 
  } else {
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
  } else {
    stop("Invalid time_aggregation. Choose 'y' or 'ym'.")
  }
  
  # Keep those with same first and last policy type and policy geo 
  dataMain <- data %>% filter(type %in% c("charge", "complete", "partial", "control")) 
  
  # Filter based on baseline % of plastic bags 
  dataBaseline <- dataMain %>% filter(type != "control" & treat == 0) %>% group_by(groupID1) %>% summarise(meanPlastic = mean(percPlasticBag)) %>% ungroup()
  dataBaseline <- dataBaseline %>% filter(meanPlastic > 0)
  dataBaseline <- dataBaseline %>% mutate(baseline = ifelse(meanPlastic < quantile(dataBaseline$meanPlastic, 0.50), 0, 
                                                            ifelse(meanPlastic > quantile(dataBaseline$meanPlastic, 0.75), 2, 1)))
  if (baseline_group == 0){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 0])
    control_mean <- dataBaseline %>% filter(baseline == 0) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 1){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 1])
    control_mean <- dataBaseline %>% filter(baseline == 1) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 2){
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline == 2])
    control_mean <- dataBaseline %>% filter(baseline == 2) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else if (baseline_group == 3) {
    dataMain <- dataMain %>% filter(groupID1 %in% dataBaseline$groupID1[dataBaseline$baseline %in% c(0, 1, 2)])
    control_mean <- dataBaseline %>% filter(baseline %in% c(0, 1, 2)) %>% summarise(meanPlastic = mean(meanPlastic)) %>% pull() 
  } else {
    stop("Invalid baseline_group. Choose 0, 1, or 2.")
  }
  
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
  
  count <- nrow(dataMain)
  
  return(count)
}

results_main <- did_analysis("groupID1", "02_merged_bigcell_year", "All", "y", 0, 3)
count_main <- count("groupID1", "02_merged_bigcell_year", "All", "y", 0, 3)

results_low <- did_analysis("groupID1", "02_merged_bigcell_year", "Low", "y", 0, 0)
count_low <- count("groupID1", "02_merged_bigcell_year", "Low", "y", 0, 0)

results_medium <- did_analysis("groupID1", "02_merged_bigcell_year", "Medium", "y", 0, 1)
count_medium <- count("groupID1", "02_merged_bigcell_year", "Medium", "y", 0, 1)

results_high <- did_analysis("groupID1", "02_merged_bigcell_year", "High", "y", 0, 2)
count_high <- count("groupID1", "02_merged_bigcell_year", "High", "y", 0, 2)

# combine 
results <- rbind(results_main, results_low, results_medium, results_high)
results <- results %>% mutate(x = ifelse(version == "All", 0, 
                                         ifelse(version == "Low", 1, 
                                                ifelse(version == "Medium", 2, 
                                                       ifelse(version == "High", 3, NA)))))
results <- results %>% mutate(version = factor(version, levels = c("All", "Low", "Medium", "High")))

# get number of treated cell x year combinations for each 
data <- data.frame(typeGeo = c("All", "Low", "Medium", "High"), sum = c(count_main, count_low, count_medium, count_high))
data <- data %>% mutate(typeGeo = factor(typeGeo, levels = c("All", "Low", "Medium", "High")))

# Plot ---

# color palette

pal <- c("black", "#004D40", "#FFC107", "#D81B60")

# plot 
b <- results %>% ggplot(aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(shape = model, ymin = confLow, ymax = confHigh, color=version), width = 0.25, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(shape = model, ymin = confLow90, ymax = confHigh90, color=version), size = 3, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(shape=model, color=version), size = 3.5, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = " ", title= "Baseline Plastic Bag Share of Items") +
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  
  scale_color_manual(name = "Group", values = pal) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-100, 100, by = 50), limits = c(-135, 125)) +
  scale_x_continuous(breaks = seq(0, 3, 1), labels = c("All", "Low", "Medium", "High"), limits=c(-0.5,3.5)) + 
  add_common_theme_coef
b <- b + guides(shape= guide_legend(nrow=3), color="none")
legend <- get_legend(b)
b <- b + theme(legend.position = "none")

# now bar graph from size 
c <- ggplot(data, aes(x = typeGeo, y = sum, fill = typeGeo)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=pal, guide = "none") +
  theme_bw() + xlab(" ") + ylab("Treated Obs.\n(Cell x Year)") +
  scale_y_continuous(breaks=c(0, 700, 1400, 2100))+ 
  add_common_theme_bar

plot_baseline <- ggarrange(b, NULL, c, ncol = 1, align = "v", heights = c(2.25, -0.275, 1))

### PART 5: FINAL COMBINE --------------------------------------------------------------------------------

# combine arranged plots with the legend
arranged <- ggarrange(plot_type, plot_geo, plot_loc, plot_baseline, ncol = 2, nrow = 2, align = "v",   labels = c("A", "B", "C", "D"))

final_plot <-  grid.arrange(
  grobs = list(arranged, legend),
  heights = c(10, 1.5)
)
final_plot

ggsave("output/Figure4.jpeg", final_plot, width = 11, height = 10, unit = 'in')


