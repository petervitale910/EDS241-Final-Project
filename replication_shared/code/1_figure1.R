#############################################################################################
# Policies Analyzed - FIGURE 1 
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
pacman::p_load(dplyr, tidyr, ggplot2, lubridate, PNWColors, grid, gridExtra, ggpubr, knitr, zoo)    

# Figure formatting -------------------------------------------------------------------------

## text size 
textSize <- 10

## colors 
pal_shuksan <- pnw_palette("Shuksan", 12)
pal_bay <- pnw_palette("Bay", 5)
pal_fill <- c(pal_shuksan[c(1:3, 5:7, 10:12)])
pal_type <- pal_fill[c(2, 5, 8)]
pal_geo <- pal_bay[c(1, 2, 5)]
pal_combined <- c(pal_type, pal_geo)
pal_color_pop <- pnw_palette("Shuksan", 3)
pal_fill_pop <- pnw_palette("Shuksan", 12)[c(1:3, 5:7, 10:12)]

## plot unified theme 
unified_theme <- theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    panel.spacing = unit(-0.005, "cm"),
    strip.text = element_text(size = textSize - 6),
    axis.text = element_text(size = textSize),
    axis.title = element_text(size = textSize),
    legend.text = element_text(size = textSize - 4),
    legend.title = element_text(size = textSize - 2),
    legend.key.size = unit(0.8, "lines"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_line(size = 1),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Function to apply the unified theme with optional overrides
apply_unified_theme <- function(plot, ...) {
  plot + 
    unified_theme + 
    theme(...)
}

# Data ------------------------------------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")
load("data/processed/01_zip_policy_characteristics.rda")
load("data/processed/01_zip_policy_distinct.rda")
load("data/processed/02_merged_bigcell_year.rda")

# zip code population 
zipChars <- merged %>% dplyr::select(zip, popTotal)

# count number of total policies 
numPolicies <- zipPoliciesYear %>% distinct(policyID) %>% nrow()

### PART 1 --------------------------------------------------------------------------------------------

# Part a --- 

# process policies
process_policies <- function(data) {
  data %>%
    mutate(
      firstPolicyID = policyID,
      yearEffective = year(effect),
      geo = policyType,
      type = case_when(
        complete_bag_ban == 1 ~ "complete_bag_ban",
        partial_bag_ban == 1 ~ "partial_bag_ban",
        TRUE ~ "plastic_bag_charge"
      )
    )
}

policies <- process_policies(zipPoliciesYear)

# summarize policies, by type and by geography
summarize_policies <- function(data, group_var, stack_type) {
  data %>%
    group_by(firstPolicyID, yearEffective, {{ group_var }}) %>%
    summarise(zips = n(), .groups = "drop") %>%
    group_by({{ group_var }}, yearEffective) %>%
    summarise(n = n(), zips = sum(zips), .groups = "drop") %>%
    filter(yearEffective >= 2007) %>%
    mutate(stackType = stack_type) %>%
    rename(group = {{ group_var }})
}

policiesType <- summarize_policies(policies, type, "type") %>% mutate(n = ifelse(yearEffective == 2018 & group == "complete_bag_ban", n-1, n))
policiesGeo <- summarize_policies(policies, geo, "geo")

# combine policy summaries
policiesStack <- bind_rows(policiesType, policiesGeo) %>%
  mutate(
    group = factor(group, levels = c("state", "county", "town", "complete_bag_ban", "partial_bag_ban", "plastic_bag_charge")),
    yearChar = as.character(yearEffective)
  )

# add zeros for 2024 - 2028
policiesStack <- policiesStack %>%
  bind_rows(
    expand_grid(
      yearEffective = 2024:2029,
      n = 0,
      zips = 0,
      stackType = "type",
      group = factor("complete_bag_ban", levels = levels(policiesStack$group)),
      yearChar = as.character(2024:2029)
    )
  )

# ensure correct factor levels and order
policiesStack <- policiesStack %>%
  mutate(group = factor(group, levels = c("state", "county", "town", "complete_bag_ban", "partial_bag_ban", "plastic_bag_charge")))

# Part b ---

# initial data cleanup and merge
merged <- left_join(cleanup, merged %>% dplyr::select(-c(state, city, county, lat, lon))) %>%
  mutate_all(~ replace(., is.nan(.), NA)) %>%
  mutate(
    policyType = firstPolicyType,
    policyInEffect = ifelse(policy == 1 & date > firstEffect, 1, 0)
  )

# process policy data
process_policy_data <- function(data) {
  data %>%
    rename(policyGeo = policyType) %>%
    mutate(
      policyType = case_when(
        complete_bag_ban == 1 ~ "ban",
        partial_bag_ban == 1 ~ "partial ban",
        plastic_bag_charge == 1 ~ "tax",
        TRUE ~ ""
      ),
      year = year(effect)
    ) %>%
    dplyr::select(zip, year, policyGeo, policyType, popIRS) %>%
    left_join(zipChars %>% mutate(zip = as.numeric(zip))) %>%
    mutate(pop = ifelse(is.na(popTotal), popIRS, popTotal))
}

# create base policy dataset
policy <- process_policy_data(zipPoliciesYear)
zip_years <- crossing(
  zip = policy %>% distinct(zip) %>% pull(),
  year = 2007:2024
)

# fill missing values in policy data
policy <- left_join(zip_years, policy) %>%
  filter(!is.na(zip)) %>%
  group_by(zip) %>%
  mutate_all(~zoo::na.locf(., na.rm = FALSE)) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# create all geo/type-year combinations for both 
geo_years <- crossing(
  policy = c("state", "county", "town"),
  year = 2008:2024
)

type_years <- crossing(
  policy = c("ban", "partial ban", "tax"),
  year = 2008:2024
)

# geographic summary 
policy_group_geo <- policy %>%
  filter(!is.na(policyGeo)) %>%
  group_by(policyGeo, year) %>%
  summarise(popSum = sum(pop, na.rm = TRUE) / 1000000) %>%  # Scale by millions
  rename(policy = policyGeo) %>%
  ungroup() %>%
  # join with all year combinations and fill missing with 0
  right_join(geo_years) %>%
  mutate(
    popSum = ifelse(is.na(popSum), 0, popSum),
    policy = factor(policy, levels = c("state", "county", "town"))
  )

# policy type summary 
policy_group_type <- policy %>%
  filter(!is.na(policyType)) %>%
  group_by(policyType, year) %>%
  summarise(popSum = sum(pop, na.rm = TRUE) / 1000000) %>%  # Scale by millions
  rename(policy = policyType) %>%
  ungroup() %>%
  # join with all year combinations and fill missing with 0
  right_join(type_years) %>%
  mutate(popSum = ifelse(is.na(popSum), 0, popSum))

# Plots --- 

# create plot A
plot_a <- ggplot(policiesStack) +
  geom_bar(aes(x = stackType, y = n, fill = group),
           position = "stack", stat = "identity", width = 1, alpha = 0.8) +
  facet_grid(~ yearChar, switch = "x") +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = pal_combined,
                    name = "Scope/Type",
                    labels = c("State", "County", "Town", "Bans", "Partial Bans", "Fees")) +
  labs(x = "Treatment Year", y = "Number of New Policies\n", title = "All Policies", 
       subtitle = paste0("n = ", numPolicies)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Add consistent x-axis text formatting

plot_a <- apply_unified_theme(plot_a) +
  theme(legend.position = c(0.9, 0.6), 
        plot.margin = margin(l = 10, t = 5, b = 20))

# create plot B, part i (geographic scope)
plot_b_i <- ggplot() +
  geom_area(data = policy_group_geo, 
            aes(x = year, y = popSum, fill = factor(policy)),
            alpha = 0.6, color = NA, size = 0.5) +
  scale_color_manual(values = pal_type) +
  scale_fill_manual(values = pal_type,
                    name = "Scope",
                    labels = c("State", "County", "Town")) +
  labs(x = NULL,  # Remove x-axis label for middle plot
       y = NULL, 
       subtitle = "US Population Subject to Policies\nby Geographic Scope") +  # Remove individual y-axis label
  theme_bw() +
  ylim(0, 120) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4), 
                     limits = c(2007.55, 2024.2))
plot_b_i <- apply_unified_theme(plot_b_i)

# create plot B, part ii (policy type)
plot_b_ii <- ggplot() +
  geom_area(data = policy_group_type, 
            aes(x = year, y = popSum, fill = factor(policy)),
            alpha = 0.6, color = NA, size = 0.5) +
  scale_color_manual(values = pal_geo) +
  scale_fill_manual(values = pal_geo,
                    name = "Type",
                    labels = c("Bans", "Partial Bans", "Fees")) +
  labs(x = "Year",  # Keep x-axis label only for bottom plot
       y = NULL, 
       subtitle = "\nby Policy Type") +  # Remove individual y-axis label
  theme_bw() +
  ylim(0, 120) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4), 
                     limits = c(2007.55, 2024.2))
plot_b_ii <- apply_unified_theme(plot_b_ii)

# create a common y-axis label
y_label <- textGrob("Cumulative Population Subject to\nPlastic Bag Policy (Millions)", 
                    rot = 90, gp = gpar(fontsize = textSize))

# combine plots with shared y-axis label
plot_b <- ggarrange(plot_b_i, plot_b_ii, 
                    ncol = 1, 
                    nrow = 2, 
                    align = "v", 
                    heights = c(1, 1),
                    common.legend = FALSE)

# add the common y-axis label
plot_b <- grid.arrange(arrangeGrob(plot_b, left = y_label))

# final combination of all plots
plot1 <- ggarrange(plot_a, plot_b, 
                   ncol = 1, 
                   nrow = 2, 
                   align = "v", 
                   labels = c("A", "B"), 
                   heights = c(2.5, 3.5)) + 
  bgcolor("white")

### PART 2 --------------------------------------------------------------------------------------------

# Part a ---

# Data preprocessing
df_all <- data %>%
  filter(type %in% c("charge", "complete", "partial", "control")) %>%
  filter(!is.na(percPlasticBag), state != "CA", !(state == "NY" & type == "control"))

df_filtered <- df_all %>%
  filter(type != "control") %>%
  mutate(yearEffective = year(firstEffect))

# count number of total policies 
numPoliciesAnalyzed <- df_filtered %>% distinct(firstPolicyID) %>% nrow()

# function to process policy data
process_policy_data <- function(df, group_col, stack_type) {
  df %>%
    group_by(firstPolicyID, yearEffective, !!sym(group_col)) %>%
    summarise(zips = n(), .groups = "drop") %>%
    group_by(!!sym(group_col), yearEffective) %>%
    summarise(n = n(), zips = sum(zips), .groups = "drop") %>%
    filter(yearEffective >= 2007) %>%
    mutate(
      stackType = stack_type,
      yearChar = as.character(yearEffective)
    )
}

# process policy data by type and geography
df_policies_type <- process_policy_data(df_filtered, "firstPolicyType", "type") %>%
  rename(group = firstPolicyType)

df_policies_geo <- process_policy_data(df_filtered, "firstPolicyGeo", "geo") %>%
  rename(group = firstPolicyGeo)

# combine and prepare data for plotting
df_policies_stack <- bind_rows(df_policies_type, df_policies_geo) %>%
  mutate(group = factor(group, levels = c("state", "county", "town", "complete_bag_ban", "partial_bag_ban", "plastic_bag_charge")))

# add zeros for 2016
df_policies_stack <- bind_rows(
  df_policies_stack,
  expand.grid(
    group = levels(df_policies_stack$group),
    yearEffective = 2016,
    n = 0,
    zips = 0,
    stackType = "type",
    yearChar = "2016"
  )
)

# add zeros for 2024 - 2028
df_policies_stack <- df_policies_stack %>%
  bind_rows(
    expand_grid(
      yearEffective = 2024,
      n = 0,
      zips = 0,
      stackType = "type",
      group = factor("complete_bag_ban", levels = levels(policiesStack$group)),
      yearChar = as.character(2024)
    )
  )

# Part b ---

# constant policies with basic information
df_policies <- data %>%
  group_by(zip) %>%
  summarise(
    first_effect = first(firstEffect),
    policy_type = first(firstPolicyType),
    policy_geo = first(firstPolicyGeo)
  ) %>%
  mutate(year = year(first_effect))

# merge population data
df_policies <- df_policies %>%
  left_join(zipChars %>% mutate(zip = as.numeric(zip))) %>%
  mutate(population = popTotal)

# create zip code x year combinations starting from 2016
df_zip_years <- crossing(
  zip = df_policies %>% distinct(zip) %>% unlist(),
  year = seq(2016, 2024, by = 1)  
)

# merge policies with zip_years and fill missing values
df_policies <- df_zip_years %>%
  left_join(df_policies) %>%
  filter(!is.na(zip)) %>%
  group_by(zip) %>%
  mutate(across(everything(), ~zoo::na.locf(., na.rm = FALSE))) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# calculate population sums by geo with 0's for missing values
policy_group_geo <- df_policies %>%
  group_by(policy_geo, year) %>%
  summarise(popSum = sum(population / 1e6, na.rm = TRUE)) %>%
  rename(policy = policy_geo) %>%
  ungroup() %>%
  complete(policy = c("state", "county", "town"),
           year = 2016:2024,
           fill = list(popSum = 0)) %>%  
  mutate(policy = factor(policy, levels = c("state", "county", "town")))

# Calculate population sums by type with 0's for missing values
policy_group_type <- df_policies %>%
  group_by(policy_type, year) %>%
  summarise(popSum = sum(population / 1e6, na.rm = TRUE)) %>%
  rename(policy = policy_type) %>%
  ungroup() %>%
  complete(policy = unique(df_policies$policy_type),
           year = 2016:2024,
           fill = list(popSum = 0))  

# Plots --- 

# create plot A
plot_a <- ggplot(df_policies_stack) +
  geom_bar(aes(x = stackType, y = n, fill = group),
           position = "stack", stat = "identity", width = 1, alpha = 0.8) +
  facet_grid(~ yearChar, switch = "x") +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = pal_combined,
                    name = "Scope/Type",
                    labels = c("State", "County", "Town", "Bans", "Partial Bans", "Fees")) +
  labs(x = "Treatment Year", y = "Number of New Policies\n", title = "Policies in Analysis (2017-2023)", 
       subtitle = paste0("n = ", numPoliciesAnalyzed)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Add consistent x-axis text formatting

plot_a <- apply_unified_theme(plot_a) +
  theme(legend.position = "none", 
        plot.margin = margin(l = 10, t = 5, b = 20))

# create plot B, part i (geographic scope)
plot_b_i <- ggplot() +
  geom_area(data = policy_group_geo, 
            aes(x = year, y = popSum, fill = factor(policy)),
            alpha = 0.6, color = NA, size = 0.5) +
  scale_color_manual(values = pal_type) +
  scale_fill_manual(values = pal_type,
                    name = "Scope",
                    labels = c("State", "County", "Town")) +
  labs(x = NULL, 
       y = NULL, 
       subtitle = "Population Analyzed\nby Geographic Scope") +  # Remove individual y-axis label
  theme_bw() +
  ylim(0, 15.5) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), 
                     limits = c(2015.9, 2024.15))
plot_b_i <- apply_unified_theme(plot_b_i) + theme(legend.position = "none")

# create plot B, part ii (policy type)
plot_b_ii <- ggplot() +
  geom_area(data = policy_group_type, 
            aes(x = year, y = popSum, fill = factor(policy)),
            alpha = 0.6, color = NA, size = 0.5) +
  scale_color_manual(values = pal_geo) +
  scale_fill_manual(values = pal_geo,
                    name = "Type",
                    labels = c("Bans", "Partial Bans", "Fees")) +
  labs(x = "Year",  # Keep x-axis label only for bottom plot
       y = NULL, 
       subtitle = "\nby Policy Type") +  # Remove individual y-axis label
  theme_bw() +
  ylim(0, 15.5) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), 
                     limits = c(2015.9, 2024.15))
plot_b_ii <- apply_unified_theme(plot_b_ii) + theme(legend.position = "none")

# create a common y-axis label
y_label <- textGrob("Cumulative Population Subject to\nPlastic Bag Policy (Millions)", 
                    rot = 90, gp = gpar(fontsize = textSize))

# combine plots with shared y-axis label
plot_b <- ggarrange(plot_b_i, plot_b_ii, 
                    ncol = 1, 
                    nrow = 2, 
                    align = "v", 
                    heights = c(1, 1),
                    common.legend = FALSE)

# add the common y-axis label
plot_b <- grid.arrange(arrangeGrob(plot_b, left = y_label))

# final combination of all plots
plot2 <- ggarrange(plot_a, plot_b, 
                   ncol = 1, 
                   nrow = 2, 
                   align = "v", 
                   labels = c("C", "D"), 
                   heights = c(2.5, 3.5)) + 
  bgcolor("white")

### PART 3 ------------------------------

no_border_theme <- theme(
  panel.border = element_blank(),
  panel.background = element_rect(fill = "white", color = "white"),
  plot.background = element_rect(fill = "white", color = "white")
)
plot1 <- plot1 + no_border_theme
plot2 <- plot2 + no_border_theme

ggarrange(plot1, plot2, ncol = 2 , nrow = 1, labels = c(" ", " "), widths = c(0.55, 0.45)) + bgcolor("white")

ggsave("output/Figure1.jpeg", width = 11, height = 9, unit= 'in')
