

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
library(vtable)

PathIn <- file.path("Documents", "PhD", "Dissertation", "code_data", "data", "IFLS")
setwd(PathIn)

# Load Data --------------------------------------------------------------------#
## Age, Marstat, Sex
b3a_cov <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3a_cov.dta")
b3a_cov$id <- paste0(b3a_cov$hhid14, b3a_cov$pid14)
b3a_cov <- subset(b3a_cov, select = c(id, hhid14, age, marstat, sex))
b3a_cov <- mutate(b3a_cov, marstat = ifelse(marstat=="2:Married",1,0))
b3a_cov <- mutate(b3a_cov, sex = ifelse(sex=="1:Male",1,0))
b3a_cov$age2 <- (b3a_cov$age)^2
names(b3a_cov) <- c("id", "hhid14", "age", "married", "male", "age2")

## Education
b3a_dl1 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3a_dl1.dta")
b3a_dl1$id <- paste0(b3a_dl1$hhid14, b3a_dl1$pid14)
b3a_dl1 <- subset(b3a_dl1, select = c(id, dl06))
b3a_dl1 <- mutate(b3a_dl1, dl06 = ifelse(dl06=="2:Elementary school",6,
                                         ifelse(dl06=="11:Adult education A",6,
                                                ifelse(dl06=="72:Islamic elementary school (Madrasah I)",6,
                                                       ifelse(dl06=="3:Junior high general",9,
                                                              ifelse(dl06=="4:Junior high vocational",9,
                                                                     ifelse(dl06=="12:Adult education B",9,
                                                                            ifelse(dl06=="73:Islamic junior high school (Madrasah)",9,
                                                                                   ifelse(dl06=="5:Senior high general",12,
                                                                                          ifelse(dl06=="6:Senior high vocational",12,
                                                                                                 ifelse(dl06=="15:Adult education C",12,
                                                                                                        ifelse(dl06=="74:Islamic senior high school (Madrasah)",12,
                                                                                                               ifelse(dl06=="13:Open university",16,
                                                                                                                      ifelse(dl06=="60:College (D1, D2, D3)",16,
                                                                                                                             ifelse(dl06=="61:University S1",16,
                                                                                                                                    ifelse(dl06=="62:University S2",18,
                                                                                                                                           ifelse(dl06=="63:University S3",23,0)))))))))))))))))
names(b3a_dl1) <- c("id", "educ")

## Employment & Earnings
b3a_employ <- read.csv("~/Documents/PhD/Dissertation/code_data/data/IFLS/033125_ifls_employ.csv")

## Disability
b3b_cd2 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3b_cd2.dta")
b3b_cd2$id <- paste0(b3b_cd2$hhid14, b3b_cd2$pid14)
b3b_cd2 <- subset(b3b_cd2, select = c(id, cd01type,cd01))
b3b_cd2$id <- as.numeric(b3b_cd2$id)
b3b_cd2 <- mutate(b3b_cd2, cd01 = ifelse(cd01=="1:Yes",1,0))

b3b_cd2_new <- b3b_cd2 %>%
  mutate(cd01type = as.character(cd01type)) %>% 
  mutate(cd01type = case_when(
    cd01type == "A" ~ "dis_phys",
    cd01type == "B" ~ "dis_cog",
    cd01type == "C" ~ "dis_vis",
    cd01type == "D" ~ "dis_hear",
    cd01type == "E" ~ "dis_verb",
    cd01type == "F" ~ "dis_men",
    cd01type == "I" ~ "dis_autism",
    TRUE ~ cd01type  # Keep unchanged if not in the list
  ))
b3b_cd2_new <- b3b_cd2_new %>% 
  pivot_wider(names_from = cd01type, values_from = cd01, values_fill = list(cd01 = 0))

b3b_cd2_new <- b3b_cd2_new %>%
  mutate(disability = ifelse(rowSums(select(., 2:8)) > 0, 1, 0))

## Rural/Urban
htrack <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/htrack.dta")
htrack <- subset(htrack, select = c(hhid14, sc05_93))
htrack <- mutate(htrack, sc05_93 = ifelse(sc05_93=="2.rural",1,
                                                   ifelse(sc05_93=="1.urban",0,NA)))
names(htrack) <- c("hhid14", "rural")

# Assets (decide later if want to include)
b3a_hr1 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/b3a_hr1.dta")
b3a_hr1$id <- paste0(b3a_hr1$hhid14, b3a_hr1$pid14)
b3a_hr1 <- b3a_hr1 %>%
  filter(hr1type == 'A')
b3a_hr1 <- subset(b3a_hr1, select = c(id, hr02_a, hr02_b, hr02_c, hr02_d1, hr02_d2,
                                      hr02_d3, hr02_e, hr02_f, hr02_g, hr02_h, hr02_j,
                                      hr02_k1, hr02_k2))
b3a_hr1 <- b3a_hr1 %>% replace(is.na(.), 0)
b3a_hr1$asset <- sum(b3a_hr1$hr02_a, b3a_hr1$hr02_b, b3a_hr1$hr02_c, b3a_hr1$hr02_d1, 
                     b3a_hr1$hr02_d2, b3a_hr1$hr02_d3, b3a_hr1$hr02_e, b3a_hr1$hr02_f,
                     b3a_hr1$hr02_g, b3a_hr1$hr02_h, b3a_hr1$hr02_j, b3a_hr1$hr02_k1,
                     b3a_hr1$hr02_k2)
b3a_hr1$asset <- rowSums( b3a_hr1[,2:14] )
b3a_hr1$ln_asset <- log(b3a_hr1$asset)
b3a_hr1 <- subset(b3a_hr1, select = c(id, asset, ln_asset))

## Household size
bk_ar0 <- read.dta("~/Documents/PhD/Dissertation/code_data/data/IFLS/hh14_all_dta/bk_ar0.dta")
bk_ar0 <- subset(bk_ar0, select = c(hhid14, ar03, ar03a, ar04, ar05, ar06))
bk_ar0 <- mutate(bk_ar0, ar03 = ifelse(ar03=="1:Yes",1,
                                       ifelse(ar03=="2:Yes and has been added to the list of HHM",1,0)))
bk_ar0 <- mutate(bk_ar0, ar03a = ifelse(ar03a=="1:Yes",1,
                                       ifelse(ar03a=="2:Yes and has been added to the list of HHM",1,0)))
bk_ar0 <- mutate(bk_ar0, ar04 = ifelse(ar04=="1:Yes",1,
                                       ifelse(ar04=="2:Yes and has been added to the list of HHM",1,0)))
bk_ar0 <- mutate(bk_ar0, ar05 = ifelse(ar05=="1:Yes",1,
                                       ifelse(ar05=="2:Yes and has been added to the list of HHM",1,0)))
bk_ar0 <- mutate(bk_ar0, ar06 = ifelse(ar06=="1:Yes",1,
                                       ifelse(ar06=="2:Yes and has been added to the list of HHM",1,0)))
bk_ar0$hhsize <- (rowSums( bk_ar0[,2:6] ))+2
bk_ar0 <- subset(bk_ar0, select = c(hhid14, hhsize))

# Merge ------------------------------------------------------------------------#
# b3a_cov has most number of observations (first data set is main dataset)
b3a_cov_dl1 <- merge(b3a_cov, b3a_dl1, by=c("id"), all.x = TRUE)
b3a <- merge(b3a_cov_dl1, b3a_employ, by=c("id"), all.x = TRUE)

b3a_b3b <- merge(b3a, b3b_cd2_new, by=c("id"), all.x = TRUE)

b3_htrack <- merge(b3a_b3b, htrack, by=c("hhid14"), all.x = TRUE)

ifls <- merge(b3_htrack, bk_ar0, by=c("hhid14"), all.x = TRUE)

# Final Clean Up ---------------------------------------------------------------#

ifls <- ifls[, c("id", "hhid14", "age", "age2", "male", "married", "educ", "hhsize", "rural", "disability", 
                 "dis_phys", "dis_cog", "dis_vis", "dis_hear", "dis_verb", "dis_men", "dis_autism",
                 "employ", "wage", "ln_wage", "sec_agr", "sec_min", "sec_man", "sec_ele", "sec_con", 
                 "sec_who", "sec_tra", "sec_fin", "sec_soc", "sec_other")]
sumtable(ifls,
         summ=c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'))

ifls_poswage <- subset(ifls, wage > 0)
sumtable(ifls_poswage,
         summ=c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'))
# Linear Regression ---------------------------------------------------------------#
## Characteristics on employment
ols1 <- lm(employ ~ male + age + age2 + educ + married + disability + rural + hhsize, data = ifls)
summary(ols1)

## Employment by disability types
ols4 <- lm (employ ~ dis_phys + dis_cog + dis_vis + dis_hear + dis_verb + dis_men + dis_autism, data=ifls)
summary(ols4)

## Conditional on being employed, characteristics on wage
ifls_poswage <- subset(ifls, wage > 0)
ols2 <- lm(ln_wage ~ male + age + age2 + educ + married + disability + rural, data = ifls_poswage)
summary(ols2)

## Conditional on being employed, wages in sectors
ols3 <- lm(ln_wage ~ sec_agr + sec_min + sec_man + sec_ele + sec_con + sec_who + sec_tra +
           sec_fin + sec_soc + sec_other, data = ifls_poswage)
summary(ols3)

# Save Dataset ---------------------------------------------------------------#
write.csv(ifls,"033125_ifls.csv", row.names=FALSE)
 