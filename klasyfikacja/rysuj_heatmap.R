kolory <-  colorRampPalette(colors = rev(c("red4","red","orange","white","lightblue", "blue","blueviolet")))(11)
etykiety <- rev(c("ekstremalnie ciepły", "anomalnie ciepły", "bardzo ciepły", "ciepły", "lekko ciepły",
                  "normalny", "lekko chłodny" , "chłodny", "bardzo chłodny", "anomalnie chłodny", "ekstremalnie chłodny"))

library(dplyr)
library(tidyr)

wynik <- readRDS("dane/przykladowywynik.rds") # wczytujemy jakis przykladowy plik
macierz <- wynik[,c(1:2,6)]  %>% spread(., miesiac, kwantyl) 
macierz <- apply(macierz, 2, as.numeric)
head(macierz) # ktory wyglada tak...

###############################################
# rozpoczecie operacji zwiazanej z rysowaniem:


par(mar=c(0.5,3,2,1), fig=c(0.05, 0.70, 0.06, 0.95)) # marginesy: + wymiary okna dla calej rysowanej figury:
image(x = 1:13, y=1:nrow(macierz), t(macierz[nrow(macierz):1,-1]), xlab = "", xaxt='n', yaxt='n', col = kolory, ylab='')
axis(2, labels = macierz[seq(from=1, to=nrow(macierz), by=3),1], at=(nrow(macierz):1)[seq(from=1, to=nrow(macierz), by=3)], cex.axis=0.95)
axis(3, labels = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","ROK"), at=1:13, tick = F, padj = 1)
abline(v=1.5:13, lwd=0.8)
abline(v=12.5:13.5, lwd=2.5, lty=2, col="black") # oddzielamy serie roczna
abline(h=1.5:nrow(macierz), lwd=0.5)
box(lwd=2)

# i opcjonalne dorysowanie legendy:
par(mar=c(0.5,3,2,1), fig=c(0.65, 0.95, 0.06, 0.45), new=T)
image(t(matrix(1:11)), col=kolory, xaxt='n', yaxt='n', main="Legenda:" , cex.main=1)
text(x = rep(0,11), y=0:10/10, labels=etykiety, cex=0.8)
box()
abline(h=0.5:10/10, lwd=0.5)
