
# 01. Data processing

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

### Loading data ----

# Load the dataset
#data_path <- ""

data     <- read_dta("Data/Raw/TZA_CCT_baseline.dta")

# We reviewed the duplicates in the data and we found two duplicates in the hhid 
# variable, so we can safely removed them. 

### Remove duplicates based on hhid

data_clean <- data %>%
    distinct(hhid, .keep_all = TRUE) 

### Household (HH) level data ----

#### Tidying data for HH level
data_tidy_hh <- data_clean %>%
    select(vid, hhid, enid, floor:n_elder, food_cons:submissionday)

#### Data cleaning for Household (HH) level
data_clean_hh <- data_tidy_hh %>%
    # Convert submissionday to date
    mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    mutate(duration = as.numeric(duration)) %>%
    # Convert ar_farm_unit to factor (categorical data)
    mutate(ar_unit = as.factor(ar_farm_unit)) %>%
    mutate(ar_unit = na_if(ar_unit, "")) %>%
    # Clean the crop_other variable (capitalize first letter)
    mutate(crop_other = str_to_title(crop_other)) %>%
    # Replace values in the crop variable based on crop_other using regex for new crops
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut") ~ 40,
        str_detect(crop_other, "Sesame") ~ 41,
        TRUE ~ crop
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(where(is.numeric), ~ replace(., . == -88, NA))) %>%
    # Add variable labels
    set_variable_labels(
        duration = "Duration of the interview (minutes)",
        submissiondate = "Submission date",
        ar_unit = "Farm area unit?"
    )


# Save the tidy household data
write_dta(data_clean_hh, file.path("Data/Intermediate/TZA_CCT_HH.dta"))

### Household member (HH-member) level data ----

#### Tidying data for HH-member level
data_tidy_mem <- data_clean %>%
    select(vid, hhid, enid, starts_with("gender"), starts_with("age"), 
           starts_with("read"), starts_with("clinic_visit"), starts_with("sick"), 
           starts_with("days_sick"), starts_with("treat_fin"), 
           starts_with("treat_cost"), starts_with("ill_impact"), 
           starts_with("days_impact")) %>%
    pivot_longer(cols = -c(vid, hhid, enid),  # Keep IDs static
                 names_to = c(".value", "member"),
                 names_pattern = "(.*)_(\\d+)")  # Capture the variable and the suffix

### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    filter(!is.na(gender)) %>%
    set_variable_labels(
        member = "HH member ID",
        age = "Age",
        clinic_visit = "In the past 12 months, how many times has the member attended the clinic?",
        days_sick = "No. of days in the last 4 weeks the member suffered from the heath problem?",
        treat_cost = "How much did the treatment cost?",
        days_impact = "No. of days member was unable to perform daily activities due to illness?"
    )

# Save the tidy household-member data
write_dta(data_clean_mem, file.path("Data/Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path("Data/Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = amenity, values_from = n, names_prefix = "n_")

# Save the final tidy secondary data
write_dta(secondary_data, file.path("Data/Intermediate/TZA_amenity_tidy.dta"))