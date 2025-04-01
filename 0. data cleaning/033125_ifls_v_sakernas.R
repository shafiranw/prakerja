# In this file, I'm matching the variables of IFLS and Sakernas to later simulate the income of Sakernas observations

rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(haven)
library(readxl)
library(stringr)
library(expss)
library(vtable)
library(huxtable)
library(ggplot2)
library(readr)
library(vtable)
library(cdlTools)
library(usmap)
library(ipumsr)
library(tidyverse)
library(ggalt)

PathIn <- file.path("Documents", "PhD", "Dissertation", "code_data", "data")
setwd(PathIn)

# Load Dataset ----------------------------------------------------------------#
ifls <- read.csv("~/Documents/PhD/Dissertation/code_data/data/IFLS/033125_ifls.csv")
sakernas <- read.csv("~/Documents/PhD/Dissertation/code_data/data/SAKERNAS/SAKERNAS_full/sakernas_full_1014.csv")

# Clean Up --------------------------------------------------------------------#
# Only keep important variables in Sakernas
## IFLS
ifls <- subset(ifls, select = c(id, age, age2, male, married, educ, hhsize, rural, disability, 
                                employ, wage, ln_wage))

## Sakernas
sakernas <- subset(sakernas, select = c(village, hh_number, male, age, married, educ, disabled, 
                                        employ, wage, ln_wage, pk_know, pk_apply, pk_reason, pk_accept, 
                                        pk_accept_year, pk_work, pk_complete, pk_inline, pk_skill, pk_inc_daily, 
                                        pk_inc_capital, pk_inc_debt, pk_inc_transport, pk_inc_internet, 
                                        pk_inc_train, pk_inc_other))
sakernas$age2 <- (sakernas$age)^2

# Add ID column
sakernas$id <- seq.int(nrow(sakernas))

# Age before prakerja
ifls$age_before <- ifls$age
ifls$age2_before <- ifls$age2

sakernas$age_before <- sakernas$age - (2024 - (sakernas$pk_accept_year + 2019) + 1)
sakernas$age2_before <- (sakernas$age_before)^2

# Rearrange
sakernas <- sakernas[, c("id", "age", "age2", "male", "married", "educ", "hh_number", "village", "disabled", 
                         "employ", "wage", "ln_wage", "pk_know", "pk_apply", "pk_reason", "pk_accept", 
                         "pk_accept_year", "pk_work", "pk_complete", "pk_inline", "pk_skill", "pk_inc_daily", 
                         "pk_inc_capital", "pk_inc_debt", "pk_inc_transport", "pk_inc_internet", "pk_inc_train",
                         "pk_inc_other", "age_before", "age2_before")]

# Rename variables to match
names(ifls) <- c("id", "age", "age2", "male", "married", "educ", "hhsize", "village", "disability",
                 "employ", "wage", "ln_wage", "age_before", "age2_before")
names(sakernas) <- c("id", "age", "age2", "male", "married", "educ", "hhsize", "village", "disability",
                     "employ", "wage", "ln_wage", "pk_know", "pk_apply", "pk_reason", "pk_accept", 
                     "pk_accept_year", "pk_work", "pk_complete", "pk_inline", "pk_skill", "pk_inc_daily", 
                     "pk_inc_capital", "pk_inc_debt", "pk_inc_transport", "pk_inc_internet", "pk_inc_train",
                     "pk_inc_other", "age_before", "age2_before")

# Save Dataset ----------------------------------------------------------------#
write.csv(ifls,"033125_ifls_match.csv", row.names=FALSE)
write.csv(sakernas,"033125_sakernas_match.csv", row.names=FALSE)
