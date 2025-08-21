library(zoo)
library(tidyverse)
library(here)
library(readxl)
library(imputeTS)

calidad_aire_2024_CE_tbl <- read_excel(here("data", "BD 2024.xlsx"), sheet = 'CE')

calidad_aire_2025_CE_tbl <- read_excel(here("data", "BD 2025.xlsx"), sheet = 'CE')

calidad_aire_2025_CE_tbl_Corregido <- calidad_aire_2025_CE_tbl
calidad_aire_2025_CE_tbl_Corregido <- calidad_aire_2025_CE_tbl_Corregido[-1, ]

for (i in 2:16) {
  calidad_aire_2025_CE_tbl_Corregido[[i]] <- as.numeric(as.character(calidad_aire_2025_CE_tbl_Corregido[[i]]))
}
names(calidad_aire_2025_CE_tbl_Corregido) <- names(calidad_aire_2024_CE_tbl)

dataset_CE <- rbind(calidad_aire_2024_CE_tbl, calidad_aire_2025_CE_tbl_Corregido)

dataset_CE <- dataset_CE[-1, ]

ts_CE <- ts(dataset_CE, start = c(2024, 1), frequency = 24)

dataset_CE_interpolado <- na_interpolation(ts_CE)
cor(dataset_CE_interpolado[, -1], use = "pairwise.complete.obs")
names_CE <- colnames(dataset_CE_interpolado)

for (i in 1:length(names_CE)) {
  
  qqnorm(dataset_CE_interpolado[, i], main = paste("Q-Q Plot of", names_CE[i]))
  
  qqline(dataset_CE_interpolado[, i], col = "red")
}

dataset_CE_arima <- na_kalman(ts_CE[1:5 ,], model = "auto.arima")
#cor(dataset_CE_arima[, -1], use = "pairwise.complete.obs")
