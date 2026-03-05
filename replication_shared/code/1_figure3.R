#############################################################################################
# Spillover counties - FIGURE 3
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
pacman::p_load(data.table, ggplot2, sf, dplyr, tidyr, broom, stringr, scales, usmap, tigris, gridExtra,ggpubr, PNWColors, lfe, fixest, did, didimputation, DIDmultiplegt, DIDmultiplegtDYN, grid, dotwhisker)    

## check versions
packageVersion("DIDmultiplegt")
packageVersion("DIDmultiplegtDYN")

## geometry
sf_use_s2(FALSE)

# Figure formatting -------------------------------------------------------------------------

## remove borders 
no_border_theme <- theme(
  panel.border = element_blank(),
  panel.background = element_rect(fill = "white", color = "white"),
  plot.background = element_rect(fill = "white", color = "white")
)

# figure setup 
textSize <- 15
pal <- pnw_palette("Bay", 5)
palette <- c("black", pal[5], pal[2], pal[4], pal[1])


### PART 1: MAP --------------------------------------------------------------------------------

# Load zip code data ---

# cleanup data 
load("data/processed/00_data_cleanup.rda")
cleanup <- cleanup %>% filter(state == "SC" | state == "NC")
cleanup <- cleanup %>% filter(zip != 97365) %>% filter(zip != 23666) 

# zip code data 
load("data/processed/01_zip_policy.rda")
zipSC <- zip %>% filter(state == "SC" | state == "NC") %>% filter(repealed == 0)
zipSC <- zipSC %>% distinct(zip) %>% mutate(policy = 1)
zipSCR <- zip %>% filter(state == "SC" | state == "NC") %>% filter(repealed == 1)
zipSCR <- zipSCR %>% distinct(zip) %>% mutate(policy = 0)
zipSC <- rbind(zipSC, zipSCR)

# zip code neighbors 
load('data/processed/01_zip_neighbors_policy.rda')
zipNeighbor <- neighborZipFinal %>% distinct(zip, neighborInd, neighborRepealedInd)
rm(neighborZipFinal)

# zip code shapefile (Tiger 2019)
zipSf <- st_read("data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") 
zipSf <- zipSf %>% st_transform(4326)

# rivers shapefile 
riversSf <- st_read("data/shapefiles/rivers/north_american_rivers.shp") 
riversSf <- riversSf %>% st_transform(4326)

# lakes shapefile 
lakesSf <- st_read("data/shapefiles/rivers/ne_10m_lakes_north_america.shp") 
lakesSf <- lakesSf %>% st_transform(4326)

# states 
states <- states(cb = TRUE, class="sf") 
states <- states %>% filter(STUSPS == "NC" | STUSPS == "SC")
states <- states %>% st_transform(4326)

# intersect 
riversC <- st_intersection(riversSf, states)
lakesC <- st_intersection(lakesSf, states)

# Carolinas Example ---

zipSfSC <- zipSf %>% filter(substr(ZCTA5CE10, 1, 2) == "29" | substr(ZCTA5CE10, 1, 2) == "28" | substr(ZCTA5CE10, 1, 2) == "27") 
zipSfSC <- zipSfSC %>% mutate(zip = as.integer(ZCTA5CE10))
zipSfSC <- left_join(zipSfSC, zipSC)
zipSfSC <- left_join(zipSfSC, zipNeighbor)

### PART 2: RESULTS --------------------------------------------------------------------------------

# Function ---

did_analysis <- function(group_id_var, file_name, item_description, time_aggregation, balanced, spillover_level) {
  
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
  
  # Filter needed 
  if (spillover_level == 0){
    dataMain <- data %>% 
      filter(type %in% c("charge", "complete",  "partial", "control"))  
  } else if (spillover_level == 1) {
    dataMain <- data %>% filter(type %in% c("controlSpillover1", "control")) 
    dataMain <- dataMain %>% mutate(firstY = year(neighborFirstEffect)) %>% dplyr::select(-c(treat))
    dataMain <- dataMain %>% rename(treat = treatedSpillover1)
  } else if (spillover_level == 2 ){
    dataMain <- data %>% filter(type %in% c("controlSpillover2", "control"))
    dataMain <- dataMain %>% mutate(firstY = year(neighborFirstEffect)) %>% dplyr::select(-c(treat))
    dataMain <- dataMain %>% rename(treat = treatedSpillover2)
  } else if (spillover_level == 3){
    dataMain <- data %>% filter(type %in% c("controlSpillover3", "control")) 
    dataMain <- dataMain %>% mutate(firstY = year(neighborFirstEffect)) %>% dplyr::select(-c(treat))
    dataMain <- dataMain %>% rename(treat = treatedSpillover3)
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
  if (time_aggregation == "y" & group_id_var == "zip") {
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

# Get Results ---

# main results 
results_main <- did_analysis("zip", "02_merged_zip_year", "Treated", "y", 0, 0)

results_spillover1 <- did_analysis("zip", "02_merged_zip_year", "Neighbors", "y", 0, 1)

results_spillover2 <- did_analysis("zip", "02_merged_zip_year", "Neighbors\nof Neighbors", "y", 0, 2)

results_spillover3 <- did_analysis("zip", "02_merged_zip_year", "Neighbors\nof Neighbors\nof Neighbors", "y", 0, 3)

# combine 
results <- rbind(results_main, results_spillover1, results_spillover2, results_spillover3)
results <- results %>% mutate(x = ifelse(version == "Treated", 0, 
                                         ifelse(version == "Neighbors", 1, 
                                                ifelse(version == "Neighbors\nof Neighbors", 2, 3))))
results <- results %>% mutate(version = factor(version, levels = c("Treated", "Neighbors", "Neighbors\nof Neighbors", "Neighbors\nof Neighbors\nof Neighbors")))

# data for plots 
load(paste0("data/processed/02_merged_zip_year.rda"))
data <- data %>% distinct(type, zip)
data <- data %>% mutate(typeSpillover = ifelse(type %in% c("charge", "complete", "partial"), "Treated", 
                                               ifelse(type == "controlSpillover1", "Neighbors", 
                                                      ifelse(type == "controlSpillover2", "Neighbors\nof Neighbors", 
                                                             ifelse(type == "controlSpillover3", "Neighbors\nof Neighbors\nof Neighbors", NA)))))
data <- data %>% mutate(ind = 1) %>% group_by(typeSpillover) %>% summarise(sum = sum(ind))
data <- data %>% filter(!is.na(typeSpillover))
data <- data %>% mutate(typeSpillover = factor(typeSpillover, levels = c("Treated", "Neighbors", "Neighbors\nof Neighbors", "Neighbors\nof Neighbors\nof Neighbors")))

### PART 2: PLOTS --------------------------------------------------------------------------------

# color palette
pal <- pnw_palette("Sailboat", 7) 

# map for Carolinas 
a <- ggplot() + 
  geom_sf(data = zipSfSC, color=NA, linewidth=0.05, aes(fill = "Group 5")) +  
  geom_sf(data = zipSfSC %>% filter(policy == 1),  linewidth=0.05,color=NA, aes(fill = "Group 1"))  + 
  geom_sf(data = zipSfSC %>% filter(policy == 0),  linewidth=0.05,color=NA, aes(fill = "Repealed"))  + 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 1), linewidth=0.05,color=NA, aes(fill = "Group 2"))+ 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 2), linewidth=0.05,color=NA, aes(fill = "Group 3"))+ 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 3), linewidth=0.05,color=NA, aes(fill = "Group 4"))+ 
  geom_sf(data = zipSfSC %>% filter(neighborRepealedInd == 1 | neighborRepealedInd == 2 | neighborRepealedInd == 3), linewidth=0.05,color=NA, aes(fill = "Repealed"))+ 
  geom_sf(data = riversC, color="blue", linewidth=0.2, alpha=0.6)+
  geom_sf(data = lakesC, color="blue", linewidth=0.1, alpha=0.6)+
  geom_sf(data = states, color="darkgray", linewidth=0.5, fill=NA) + 
  #geom_rect(data = cleanupCell, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill="black", color="black") + 
  geom_point(data = cleanup, aes(x = lon, y = lat), fill="black", color="black", size = 0.05) + 
  xlab(" ") + ylab(" ") + 
  theme_void() +
  scale_fill_manual(values = c("Group 5" = pal[3], "Group 4" = pal[4], "Group 3" = pal[5], "Group 2" = pal[6], "Group 1" = pal[7], "Repealed" = "gray"),  
                    name = "Zip Codes",
                    labels = c("Treated", "Neighbor", "N. of N.", "N. of N. of N.", "Control", "Repealed/N. of R."))+ 
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        legend.key.size = unit(0.5, "lines"), 
        legend.position = c(0.85, 0.175))
a <- a + no_border_theme

# color palette
pal <- pnw_palette("Sailboat", 7) 

# plot 
b <- results %>% ggplot(aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0,colour = "grey60", size=0.2) + 
  geom_errorbar(aes(shape = model, ymin = confLow, ymax = confHigh, color=version), width = 0.25, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(shape = model, ymin = confLow90, ymax = confHigh90, color=version), size = 3, position = position_dodge(width = 0.6), alpha=0.35) +
  geom_point(aes(shape=model, color=version), size = 3.5, position = position_dodge(width = 0.6)) +
  labs(y = "Plastic Bags (Share of Items)\n∆ Relative to Control Mean (%)", x = " ") +
  scale_shape_manual(name = "Estimator", values = c(16, 17, 4, 15, 1)) +  
  scale_color_manual(name = "Group", values = c(pal[7], pal[6], pal[5], pal[4])) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, 17, 4, 15, 1)))) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-100, 100, by = 100)) +
  scale_x_continuous(breaks = seq(0, 3, 1), labels = c("Treated", "Neighbors", "Neighbors\nof Neighbors", "Neighbors\nof Neighbors\nof Neighbors"), limits=c(-0.5,3.5)) +
  theme(legend.position = "bottom", 
        axis.text.x=element_blank(), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize-2, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-6), 
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
b <- b + guides(shape= guide_legend(nrow=3), color="none")
legend <- get_legend(b)
b <- b + theme(legend.position = "none")

# now bar graph from size 
c <- ggplot(data, aes(x = typeSpillover, y = sum, fill = typeSpillover)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=(c(pal[7], pal[6], pal[5], pal[4])), guide = "none") +
  theme_bw() + xlab(" ") + ylab("Zip Codes") +
  scale_y_continuous(breaks=c(0, 1400, 700))+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize-4), 
        axis.text.y=element_text(size = textSize-2), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize-3, margin=margin(r=-2.5, t = 20)),
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
        axis.ticks.length = unit(0.15, "cm"), 
        panel.background = element_rect(fill='grey89', colour='grey89'))

b <- b + no_border_theme
c <- c + no_border_theme
plot <- ggarrange(b, NULL, c, ncol = 1, align = "v", heights = c(2.25, -0.275, 1))

# combine arranged plots with the legend
final_plot <-  grid.arrange(
  grobs = list(plot, legend),
  layout_matrix = rbind(c(1),
                        c(NA),
                        c(2)),
  heights = c(10, 0.1, 1.5)
)
final_plot

# arrange plots using ggarrange
arranged_plots <- ggarrange(a, final_plot, nrow = 1, labels = c("A", "B")) + bgcolor("white") 
arranged_plots <- arranged_plots + no_border_theme

ggsave("output/Figure3.jpeg", arranged_plots, width = 11, height = 5, unit = 'in', bg="white")

