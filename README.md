# Wstęp do metod badań geograficznych

1. Instalacja pakietu
--------------------
W celu zainstalowania pakietu najszybciej wykorzystać pakiet `devtools` oraz pakiety na których bazuje `wstepdometod`:

``` r
# dla swiezej instalacji R:
install.packages("devtools")
install.packages("ggplot2")
install.packages("reshape")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("RColorBrewer")
library(devtools)
install_github("bczernecki/wstepdometod")
```


2. Skrypty robocze do wizualizacji i przetwarzania danych
--------------------


![Klasyfikacja kwantylowa temperatur miesięcznych w Polsce - źródło danych meteorologicznych IMGW-PIB](dywan_kwantylowa.svg)


Wykresy pudełkowe dla percentyli: 0.05, 0.25, 0.50, 0.75, 0.95 oraz 1- i 2.5-odchylenia standardowego
![Wykres pudełkowy rozkładu miesięcznych temperatur powietrza w Polsce](boxplot.svg)

Rozkłady anomalii
![Rozkład prawdopodobieństwa miesięcznych anomalii temperatury powietrza](polygon.svg)

Wartości rzeczywiste z zaznaczonymi wartościami średnimi w wieloleciu
![Rozkład prawdopodobieństwa miesięcznych temperatur powietrza w Polsce](polygon_surowe.svg)