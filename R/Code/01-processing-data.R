# Reproducible Research Fundamentals 
# 01. Data processing

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

### Loading data ----

# Load the dataset
data_path <- "C:/Users/wb645047/OneDrive - WBG/Training/Data"
data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

### Remove duplicates based on hhid
data_clean <- data %>% 
    distinct(hhid, .keep_all = TRUE)

### Household (HH) level data ----

#### Tidying data for HH level
# This will leave only the first occurence among unique IDs
data_clean <- data %>%
    distinct(hhid, .keep_all = TRUE)


# Tidy data for HHs
data_tidy_hh <- data_clean %>%
    select(vid,
           hhid,
           enid,
           floor:n_elder,
           food_cons:last_col())


#Exercise 3: Tidying the HH member dataframe
library(tidyr)
data_tidy_hh_member <- data_clean %>%
    select(vid, hhid, enid,
           starts_with("gender"),
           starts_with("age"),
           starts_with("read"),
           starts_with("clinic_visit"),
           starts_with("sick"),
           starts_with("days_sick"),
           starts_with("treat_fin"),
           starts_with("treat_cost"),
           starts_with("ill_impact"),
           starts_with("days_impact")) %>%
    pivot_longer(cols = -c(vid, hhid, enid),
                 names_to = c(".value", "member"),
                 names_pattern = "(.*)(\\d+)")


# Ex 4
library(stringr)
library(labelled)
data_clean_hh <- data_tidy_hh %>%
    mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(duration = as.numeric(duration)) %>%
    mutate(ar_unit = as.factor(ar_farm_unit)) %>%
    mutate(ar_unit = na_if(ar_unit, "")) %>%
    mutate(crop_other = str_to_title(crop_other)) %>%
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut") ~ 40,
        str_detect(crop_other, "Sesame") ~ 41,
        TRUE ~ crop)) %>%
    mutate(across(where(is.numeric), ~ replace(., . == -88, NA))) %>%
    set_variable_labels(
        duration = "Duration of the interview (minutes)",
        submissiondate = "Submission date",
        ar_unit = "Farm area unit?")


# Ex 4b
data_clean_hh_member <- data_tidy_hh_member %>%
    filter(!is.na(gender_)) %>%
    set_variable_labels(
        member = "HH member ID",
        age_ = "Age",
        clinic_visit_ = "In the past 12 months, how many times has the member attended the clinic?",
        days_sick_ = "No. of days in the last 4 weeks the member suffered from the health problem?",
        treat_cost_ = "How much did the treatment cost?",
        days_impact_ = "No. of days member was unable to perform daily activities due to illness?")



# load CSV data
secondary_data <- read.csv("Data/Raw/TZA_amenity.csv")


# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = amenity,
                values_from = n,
                names_prefix = "n_")


# Save the household data
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Save the tidy household-member data
write_dta(data_clean_hh_member, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))






