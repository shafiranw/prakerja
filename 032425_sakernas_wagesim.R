# In this file, I'm cleaning up the simulated wages and comparing them to wages before

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

# Load & Celaning Up Dataset
sakernas_sim <- read.csv("~/Documents/PhD/Dissertation/code_data/data/sakernas_wagesim_032425.csv")

hist(sakernas_sim$ln_wage_before)

sakernas_sim$wage_diff <- sakernas_sim$wage - sakernas_sim$wage_before
hist(sakernas_sim$wage_diff)

sumtable(sakernas_sim,
         summ=c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'))
