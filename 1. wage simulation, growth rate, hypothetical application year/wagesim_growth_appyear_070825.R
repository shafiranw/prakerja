rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(tidyverse)

PathIn <- file.path("Documents")
setwd(PathIn)

# Load Data
ifls <- read.csv("~/Documents/033125_ifls_match.csv")
sakernas <- read.csv("~/Documents/033125_sakernas_match.csv")
sakernas_full <- read.csv("~/Documents/sakernas_full_1014.csv")

# WAGE SIMULATION -------------------------------------------------------------#
# (1) IFLS
# Filter IFLS dataset to only people with positive wages
ifls_filtered <- ifls %>%
  filter(age>17 & age<68)

ifls_filtered <- ifls_filtered %>%
  mutate(educ = ifelse(educ>16, 16,educ))

ifls_filtered <- ifls_filtered %>%
  filter(wage>0)

mean(ifls_filtered$ln_wage)
     
# Fit the IFLS regression model
model_ifls <- lm(wage ~ age_before + age2_before + male + married + educ + village + disability,
                 data = ifls_filtered)
summary(model_ifls)

# Extract coefficientsd from the model
betas_wage <- coef(model_ifls)
print(betas_wage)

# (2) Sakernas
# Filter Sakernas dataset to only people in program
sakernas_filtered <- sakernas %>%
  filter(!is.na(age_before))

# Define same independent variables for Sakernas
x_sakernas <- sakernas_filtered %>%
  select(age_before, age2_before, male, married, educ, village, disability) %>%
  mutate(constant = 1)
x_sakernas <- x_sakernas %>%
  select(constant, everything())

# Simulate log wage for Sakernas using the betas from the IFLS regression
sakernas_filtered$wage_before <- as.vector(as.matrix(x_sakernas) %*% betas_wage)

# (3) Adjust for inflation
# Define year before
sakernas_filtered <- sakernas_filtered %>%
  mutate(year_before = case_when(
    pk_accept_year == 1 ~ 2019,
    pk_accept_year == 2 ~ 2020,
    pk_accept_year == 3 ~ 2021,
    pk_accept_year == 4 ~ 2022,
    TRUE ~ NA_real_  # If pk_accept_year is missing or invalid
  ))

# Define CPIs
sakernas_filtered <- sakernas_filtered %>%
  mutate(cpi = case_when(
    year_before == 2019 ~ 151.2,
    year_before == 2020 ~ 154.1,
    year_before == 2021 ~ 156.5,
    year_before == 2022 ~ 163.1,
    TRUE ~ NA_real_  # If year_before is missing or invalid
  ))

# Calculate inflation factor
sakernas_filtered <- sakernas_filtered %>%
  mutate(inflation_factor = cpi / 124.4)  # CPI 2014

# Adjust wage before for inflation
sakernas_filtered <- sakernas_filtered %>%
  mutate(wage_before = wage_before + log(inflation_factor))

# Revert back to wage
sakernas_filtered <- sakernas_filtered %>%
  mutate(ln_wage_before = log(wage_before))

sakernas_filtered <- sakernas_filtered %>%
  select(-c("year_before", "cpi", "inflation_factor"))

# (4) Merge back into Sakernas
sakernas <- left_join(sakernas, sakernas_filtered %>% select(id, ln_wage_before, wage_before), by = "id")

# GROWTH RATE -----------------------------------------------------------------#
# (1) IFLS
# Filter on for people with growth rates > 0
ifls_g <- ifls %>%
  filter(gi > 0 & gi < 1)

model_ifls_g <- lm(gi ~ ln_wage + age + age2 + male + married + educ + village + disability,
                 data = ifls_g)

betas_g <- coef(model_ifls_g)

# (2) Sakernas
# Apply coefficients to Sakernas
# Define same independent variables for Sakernas
x_sakernas_g <- sakernas %>%
  select(ln_wage, age, age2, male, married, educ, village, disability) %>%
  mutate(constant = 1)

x_sakernas_g <- x_sakernas_g %>%
  select(constant, everything())

# Simulate log wage for Sakernas using the betas from the IFLS regression
sakernas$gi <- as.vector(as.matrix(x_sakernas_g) %*% betas_g)

# HYPOTHETICAL APPLICATION YEAR -----------------------------------------------#
# Count distribution of application year (should be the same as acceptance year)
apply_year_dist <- sakernas %>%
  filter(pk_accept==1) %>%
  count(pk_accept_year) %>%
  mutate(prop = n / sum(n))

# Assign random years (based on distribution) to obs with no application years
## Number of obs with no application years
no_app_year_indices <- which(is.na(sakernas$pk_apply) | sakernas$pk_apply == 0 | sakernas$pk_accept == 0)

## Function
set.seed(100)
random_years <- sample(apply_year_dist$pk_accept_year, 
                       size = length(no_app_year_indices), 
                       prob = apply_year_dist$prop, 
                       replace = TRUE)

## Apply to obs with no application years
sakernas$pk_apply_year <- sakernas$pk_accept_year

sakernas$pk_apply_year[no_app_year_indices] <- random_years

# COMBINE BACK WITH BIG DATASET -----------------------------------------------#
sakernas_full <- sakernas_full %>%
  rename(id = X)
intersect(names(sakernas), names(sakernas_full))

sakernas_combined <- sakernas_full %>%
  left_join(sakernas, by = c("id", "age", "male", "married", "educ", "village",
                             "employ", "wage", "ln_wage", "pk_know", "pk_apply",
                             "pk_reason", "pk_accept", "pk_accept_year", "pk_work",
                             "pk_complete", "pk_inline", "pk_skill", "pk_inc_daily",
                             "pk_inc_capital", "pk_inc_debt", "pk_inc_transport",
                             "pk_inc_internet", "pk_inc_train", "pk_inc_other"))
# Save
write.csv(sakernas_combined, "sakernas_070825.csv", row.names = FALSE)
