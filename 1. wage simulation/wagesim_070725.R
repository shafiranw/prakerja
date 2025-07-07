rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(tidyverse)
library(purrr)
library(broom)

PathIn <- file.path("Documents")
setwd(PathIn)

# Load Data
ifls <- read.csv("~/Documents/033125_ifls_match.csv")
sakernas <- read.csv("~/Documents/033125_sakernas_match.csv")


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

# Save
write.csv(sakernas, "sakernas_wagesim_070725.csv", row.names = FALSE)
