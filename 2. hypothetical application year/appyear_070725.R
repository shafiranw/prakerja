rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(tidyverse)

PathIn <- file.path("Documents")
setwd(PathIn)

# Load Data
sakernas <- read.csv("~/Documents/sakernas_wagesim_070725.csv")

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

# Save
write.csv(sakernas, "sakernas_app_year_070725.csv", row.names = FALSE)
