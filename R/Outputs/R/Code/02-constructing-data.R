# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# load libraries
library(tidyverse)
library(labelled)
library(haven)

# Preliminary - Load Data ----
# Load household-level data (HH)
hh_data <- read_dta("Data/Intermediate/TZA_CCT_HH.dta")

# Load HH-member data
mem_data <- read_dta("Data/Intermediate/TZA_CCT_HH_mem.dta")

# Load secondary data
secondary_data <- read_dta("Data/Intermediate/TZA_amenity_tidy.dta")

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres.
# 2. Household consumption (food and nonfood) in USD.
# 3. Any HH member sick.
# 4. Any HH member can read or write.
# 5. Average sick days.
# 6. Total treatment cost in USD.
# 7. Total medical facilities.

# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres. - 2.47
# 2. USD conversion factor.
acre_conv <- 2.47
usd <- 0.0037

# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.

hh_data <- hh_data %>%
    mutate(area_acre = case_when(
        ar_unit == 2 ~ ar_farm,
        ar_unit == 3 ~ ar_farm * acre_conv,
    TRUE ~ ar_farm,
    )) %>%
    mutate(acre_acre = replace_na(area_acre, 0)) %>%
    set_variable_labels(area_acre = "Area farmed in acres")


# consuption in USD
hh_data <- hh_data %>%
    mutate(across(c(food_cons, nonfood_cons),
                  ~.x*usd,
                  .names = "{.col}_usd"))


# Exercise 3: Handle outliers ----
# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Winsorize selected variables in the dataset
win_vars <- c("area_acre", "food_cons_usd", "nonfood_cons_usd")

# Apply the custom winsor_function to each variable in win_vars
for (var in win_vars) {
    hh_data <- winsor_function(hh_data, var)
}

# Update the labels to reflect that winsorization was applied
hh_data <- hh_data %>%
    mutate(across(ends_with("_w"), 
                  ~ labelled(.x, label = paste0(attr(.x, "label"), 
                                                " (Winsorized 0.05)"))))

# Exercise 4.1: Create indicators at HH level ----

# Collapse HH-member data to HH level
hh_mem_collapsed <- mem_data %>%
    group_by(hhid) %>%
    summarise(
        sick = max(sick, na.rm = TRUE),  # Any member was sick
        read = max(read, na.rm = TRUE),  # Any member can read/write
        # If all values of days_sick are NA, return NA; otherwise, calculate mean
        days_sick = if_else(all(is.na(days_sick)), NA_real_, mean(days_sick, na.rm = TRUE)),
        # If all values of treat_cost are NA, return NA; otherwise, calculate sum in USD
        treat_cost_usd = if_else(all(is.na(treat_cost)), NA_real_, sum(treat_cost, na.rm = TRUE) * usd)
    ) %>%
    ungroup() %>%
    # Replace missing treat_cost_usd with the average of non-missing values
    mutate(treat_cost_usd = if_else(is.na(treat_cost_usd), 
                                    mean(treat_cost_usd, na.rm = TRUE), 
                                    treat_cost_usd)) %>%
    # Apply labels to the variables
    set_variable_labels(
        read = "Any member can read/write",
        sick = "Any member was sick in the last 4 weeks",
        days_sick = "Average sick days",
        treat_cost_usd = "Total cost of treatment (USD)"
    )

# Exercise 4.2: Data construction: Secondary data ----

# Calculate the total number of medical facilities
secondary_data <- secondary_data %>%
    mutate(n_medical = rowSums(select(., n_clinic, n_hospital), 
                               na.rm = TRUE)) %>% 
    rename(district = adm2_en)

# Apply label to the new column
var_label(secondary_data$n_medical) <- "No. of medical facilities"

# Exercise 5: Merge HH and HH-member data ----

# Merge HH and HH-member datasets
final_hh_data <- hh_data %>%
    left_join(hh_mem_collapsed, by = "hhid")

# Load treatment status and merge
treat_status <- read_dta("Data/Raw/treat_status.dta")

final_hh_data <- final_hh_data %>%
    left_join(treat_status, by = "vid") 

# Exercise 6: Save final dataset ----

# final_hh_data <- final_hh_data %>%
#     select(vid, hhid, read, sick, days_sick, treat_cost_usd, treatment)

# Save the final merged data for analysis
write_dta(final_hh_data, ("Data/Final/TZA_CCT_analysis.dta"))

# Save the final secondary data for analysis
write_dta(secondary_data, ("Data/Final/TZA_amenity_analysis.dta"))