# In this file I am evaluating employment of IFLS observations

rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(haven)
library(readxl)
library(stringr)
library(expss)
library(ggplot2)
library(readr)
library(tidyverse)

PathIn <- file.path("Documents", "PhD", "Dissertation", "code_data", "data")
setwd(PathIn)

# b3a_tk1 ------------------------------------------------------------------------------------#
b3a_tk1 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3a_tk1.dta")

# TK01a: During the past week, did you do any of these activities? Work for pay
b3a_tk1 <- mutate(b3a_tk1, tk01a = ifelse(tk01a=="1:Yes",1,0))
summary(b3a_tk1$tk01a) #66%

# TK01: What was your primary activity during the past week? Working/trying to work/helping to earn income
b3a_tk1 <- mutate(b3a_tk1, tk01 = ifelse(tk01=="1:Working/trying to get work/helping to earn income",1,0))
summary(b3a_tk1$tk01) #58%

# TK02: Did you work/try to work/help to earn income for pay for at least 1 hour during the past week? Yes
b3a_tk1 <- mutate(b3a_tk1, tk02 = ifelse(tk02=="1:Yes",1,0))
summary(b3a_tk1$tk02) #21%

# TK03: Do you have a job/business, but werer temporarily not working during the past week? Yes
b3a_tk1 <- mutate(b3a_tk1, tk03 = ifelse(tk03=="1:Yes",1,0))
summary(b3a_tk1$tk03) #9%

# TK04: Did you work at a family-owned (farm or non-farm) business during the past week? Yes
b3a_tk1 <- mutate(b3a_tk1, tk04 = ifelse(tk04=="1:Yes",1,0))
summary(b3a_tk1$tk04) #3%

# b3a_tk2 ------------------------------------------------------------------------------------#
b3a_tk2 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3a_tk2.dta")
b3a_tk2$id <- paste0(b3a_tk2$hhid14, b3a_tk2$pid14)

# TK25A1 + TK25B1: Approximately what was your salary/wage during the last month?
b3a_tk2$wage <- ifelse(is.na(b3a_tk2$tk25a1) & is.na(b3a_tk2$tk25b1), NA, rowSums(b3a_tk2[, c("tk25a1", "tk25b1")], na.rm = TRUE))

# TK25A2 + TK25B2: Approximately what was your salary/wage during the last year?
b3a_tk2$wage_yr <- ifelse(is.na(b3a_tk2$tk25a2) & is.na(b3a_tk2$tk25b2), NA, rowSums(b3a_tk2[, c("tk25a2", "tk25b2")], na.rm = TRUE))

# How many people have NA/0 wage and non-NA wage_yr vice versa
b3a_tk2 %>%
  summarise(
    wage_NA_or_0_wageyr_present = sum((is.na(wage) | wage == 0) & !is.na(wage_yr), na.rm = TRUE)
    ) # 804 people
b3a_tk2 <- b3a_tk2 %>%
  mutate(missing_wage = if_else((is.na(wage) | wage == 0) & !is.na(wage_yr), 1, 0))

## Fix wages for people with missing wages
b3a_tk2 <- mutate(b3a_tk2, wage = ifelse(missing_wage==1,wage_yr/12,wage))

# Add log wages
b3a_tk2$ln_wage <- log(b3a_tk2$wage)

# TK19Ab: Sectors
b3a_tk2$sec_agr <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "1:Agriculture", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_min <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "2:Mining and quarrying", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_man <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "3:Manufacturing", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_ele <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "4:Electricity", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_con <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "5:Construction", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_who <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "6:Wholesale", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_tra <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "7:Transportation", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_fin <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "8:Finance", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_soc <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "9:Social services", 1, 
                          ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))
b3a_tk2$sec_other <- ifelse(!is.na(b3a_tk2$tkp19aa) & b3a_tk2$tkp19aa == "10:Activities that cannot be classified", 1, 
                            ifelse(!is.na(b3a_tk2$tkp19aa), 0, NA))

b3a_tk2 <- subset(b3a_tk2, select = c(id, wage, ln_wage, sec_agr, sec_min, sec_man, sec_ele, sec_con, sec_who, sec_tra,
                                      sec_fin, sec_soc, sec_other))
