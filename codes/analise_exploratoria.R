
library(tidyverse)
library(sf)
library(readxl)

cadunico <- read_excel("data/CECAD - agosto 2020 - SEPLAN_Original.xlsx")
resultados_familias <- read_csv2("data/resultados_familias.csv")

resultados_individuos <- cadunico %>%
  left_join(resultados, by = "Código familiar")

table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`)
round(prop.table(table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`),margin=2),2)
round(prop.table(table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`),margin=1),2)
