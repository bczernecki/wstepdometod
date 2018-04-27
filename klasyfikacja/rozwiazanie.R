library(readxl)
library(dplyr)
library(tidyr)
dane <- read_excel("dane/daneimgw.xls")
dane <- select(dane, rok, miesiac, WARSZAWA)

dane <- spread(dane, miesiac, WARSZAWA)

dane$year <- rowMeans(dane[,-1])
head(dane)
