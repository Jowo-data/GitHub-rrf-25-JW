# Reproducible Research Fundamentals 
# 03. Data Analysis

#Libraries -----
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data -----
#household level data
data_path <- "C:/Users/wb645047/Downloads/GitHub-rrf-25-JW/R/Data"
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
    mutate(district = as_factor(district))

# Exercise 1 and 2: Create graph of area by district -----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district))

# Create the bar plot
ggplot(hh_data_plot, 
       aes(x = district, y = area_acre_w,
           fill = treatment)) +
    geom_bar(stat = "summary",
             fun = "mean",
             position = position_dodge(width = 0.9),
             width = 0.7) +
    geom_text(stat = "summary",
              fun = "mean",
              aes(label = round(after_stat(y), 2)),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3.5) +
    labs(title = "Area cultivated by treatment assignment across districts",
         y = "Average area cultivated (acre)",
         x = "District",
         fill = "Group") +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    theme_minimal()

# Save the plot
ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)



#### HH summary table of my choice

# Create summary statistics table
summary_table <- hh_data_plot %>%
    group_by(district) %>%
    summarise(
        # Household size
        hh_size_mean = mean(hh_size, na.rm = TRUE),
        hh_size_sd = sd(hh_size, na.rm = TRUE),
        
        # Area cultivated (acres)
        area_acre_mean = mean(area_acre_w, na.rm = TRUE),
        area_acre_sd = sd(area_acre_w, na.rm = TRUE),
        
        # Food consumption (USD)
        food_cons_mean = mean(food_cons_usd_w, na.rm = TRUE),
        food_cons_sd = sd(food_cons_usd_w, na.rm = TRUE),
        
        # Non-food consumption (USD)
        nonfood_cons_mean = mean(nonfood_cons_usd_w, na.rm = TRUE),
        nonfood_cons_sd = sd(nonfood_cons_usd_w, na.rm = TRUE),
        
        # Number of children under 5
        n_child_5_mean = mean(n_child_5, na.rm = TRUE),
        n_child_5_sd = sd(n_child_5, na.rm = TRUE),
        
        # Sample size
        n = n()
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))

# View the table
print(summary_table)

# Optional: Create a more readable format
summary_table_long <- summary_table %>%
    pivot_longer(cols = -c(district, n),
                 names_to = c("variable", "statistic"),
                 names_pattern = "(.*)_(mean|sd)") %>%
    pivot_wider(names_from = statistic,
                values_from = value) %>%
    mutate(variable = recode(variable,
                             "hh_size" = "Household size",
                             "area_acre" = "Area cultivated (acres)",
                             "food_cons" = "Food consumption (USD)",
                             "nonfood_cons" = "Non-food consumption (USD)",
                             "n_child_5" = "Children under 5"))

print(summary_table_long)

# Export long format table
write.csv(summary_table_long, 
          file = file.path("Outputs", "summary_statistics_long.csv"),
          row.names = FALSE)


# Exercise 4: Summary statistics ----

# Create summary statistics by district and export to CSV
# Load required package
library(modelsummary)

# Create balance table comparing treatment and control groups
balance_table <- datasummary(
    hh_size + area_acre + food_cons_usd + nonfood_cons_usd + n_child_5 ~ 
        treatment * (Mean + SD), 
    data = hh_data_plot,
    title = "Balance Table: Treatment vs Control",
    output = file.path("Outputs", "balance_table.csv"))



# Exercise 6: Regressions ----

# Exercise 6: Regressions ----
# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data_plot)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, 
             data = hh_data_plot)

# Model 3: Add FE by district
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood + factor(district), 
             data = hh_data_plot)

# Create regression table using stargazer
library(stargazer)

stargazer(
    model1, model2, model3,
    title = "Food Consumption Effects",
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Standard errors in parentheses",
    out = file.path("Outputs", "regression_table.csv")
)


# non stargazer output:

# Load required package
library(openxlsx)

# Create a workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "Regression Results")

# Extract regression results into a data frame
library(broom)

# Tidy the models
results1 <- tidy(model1) %>% mutate(Model = "Model 1")
results2 <- tidy(model2) %>% mutate(Model = "Model 2")
results3 <- tidy(model3) %>% mutate(Model = "Model 3")

# Combine results
all_results <- bind_rows(results1, results2, results3) %>%
    select(Model, term, estimate, std.error, statistic, p.value)

# Write to Excel
writeData(wb, "Regression Results", all_results)

# Save the workbook
saveWorkbook(wb, file.path("Outputs", "regression_results.xlsx"), overwrite = TRUE)



# Exercise 7: Combining two plots ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))
