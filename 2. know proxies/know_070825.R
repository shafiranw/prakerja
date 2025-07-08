rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(readxl)

# Load Dataset
internet <- read_excel("~/Documents/internet.xlsx")
news <- read_excel("~/Documents/news.xls")
tv <- read_excel("~/Documents/tv.xls")
radio <- read_excel("~/Documents/radio.xls")
sakernas <- read.csv("~/Documents/sakernas_070825.csv")

# Merge with Internet, News, TV, and Radio
know_proxy <- internet %>%
  left_join(news, by = c("province_name" = "province_name"))
know_proxy <- know_proxy %>%
  left_join(tv, by = c("province_name" = "province_name"))
know_proxy <- know_proxy %>%
  left_join(radio, by = c("province_name" = "province_name"))

# Create province code data
province_lookup <- data.frame(
  province = c(11,12,13,14,15,16,17,18,19,21,
               31,32,33,34,35,36,
               51,52,53,
               61,62,63,64,65,
               71,72,73,74,75,76,
               81,82,
               91,92),
  province_name = c(
    "Aceh",
    "Sumatera Utara",
    "Sumatera Barat",
    "Riau",
    "Jambi",
    "Sumatera Selatan",
    "Bengkulu",
    "Lampung",
    "Kepulauan Bangka Belitung",
    "Kepulauan Riau",
    "DKI Jakarta",
    "Jawa Barat",
    "Jawa Tengah",
    "DI Yogyakarta",
    "Jawa Timur",
    "Banten",
    "Bali",
    "Nusa Tenggara Barat",
    "Nusa Tenggara Timur",
    "Kalimantan Barat",
    "Kalimantan Tengah",
    "Kalimantan Selatan",
    "Kalimantan Timur",
    "Kalimantan Utara",
    "Sulawesi Utara",
    "Sulawesi Tengah",
    "Sulawesi Selatan",
    "Sulawesi Tenggara",
    "Gorontalo",
    "Sulawesi Barat",
    "Maluku",
    "Maluku Utara",
    "Papua",
    "Papua Barat"
  )
)

# Merge Province Name with main dataset
know_proxy <- know_proxy %>%
  left_join(province_lookup, by = c("province_name" = "province_name"))
know_proxy <- know_proxy[, !(names(know_proxy) %in% c("province_name"))]

# Merge with SAKERNAS
data <- sakernas %>%
  left_join(know_proxy, by = c("province" = "province"))
data <- data[, !(names(data) %in% c("X"))]

summary(data)

# Regression -------------------------------------------------------------#
model1 <- lm(pk_know ~ internet_all + news_all + tv_all + radio_all, data = data)
summary(model1)

# Only people in villages
data_village <- data[(data$village == 1),]

model2 <- lm(pk_know ~ internet_village + news_village + tv_village + radio_village, data = data_village)
summary(model2)

# Save dataset -------------------------------------------------------------------------#
write.csv(data, "sakernas_full_070825.csv", row.names = FALSE)
