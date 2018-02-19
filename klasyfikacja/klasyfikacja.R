library(ggplot2)
library(reshape)
library(plyr)
library(tidyverse)
library(RColorBrewer)

dane <- read.table("klasyfikacja/polska.csv", dec=",",header=T) 
colnames(dane) <- c("rok", month.name, "DJF","MAM", "JJA", "SON", "YEAR")


anomal <- function(x, rok) {
  ind <- which(rok >= 1951 & rok <= 2010) # deklaracja referencji
  round(x-mean(x[ind], na.rm=T),2)
}

klasa_kwantyl <- function(x, rok){
  ind <- which(rok >= 1951 & rok <= 2010) # deklaracja referencji
  przedzialy <- quantile(x[ind], c(0,5,10,20,30,40,60,70,80,90,95,100)/100)
  przedzialy[c(1,11)] <- c(-99,99)
  cut(x, breaks=przedzialy, labels=1:11)
} 

klasa_lorenc <- function(x, rok){
  ind <- which(rok >= 1951 & rok <= 2010) # deklaracja referencji
  przedzialy <- (sd(x[ind], na.rm=T)*c(-99,-2.5,-2.0,-1.5,-1.0,-0.5, 0.5, 1.0, 1.5, 2.0, 2.5,99))+mean(x[ind], na.rm=T)
  cut(x, breaks = przedzialy, labels = 1:11)
}


dane55 <- dplyr::select(dane, -DJF, -MAM, -JJA, -SON) %>% gather(., miesiac, temp, -rok) 
dane55$miesiac <- factor(dane55$miesiac, levels = c(month.name,"YEAR"))

wynik <- ddply(dane55, "miesiac", transform, 
            anomalia=anomal(x=temp, rok=rok),  
            lorenc=klasa_lorenc(x=temp, rok=rok),
            kwantyl=klasa_kwantyl(x=temp, rok=rok))



kolory <-  colorRampPalette(colors = rev(c("red4","red","orange","white","lightblue", "blue","blueviolet")))(11)
etykiety <- rev(c("ekstremalnie ciepły", "anomalnie ciepły", "bardzo ciepły", "ciepły", "lekko ciepły",
            "normalny", "lekko chłodny" , "chłodny", "bardzo chłodny", "anomalnie chłodny", "ekstremalnie chłodny"))
#etykiety <- 1:11

wynik$rok <- as.factor(wynik$rok*-1)

rysuj_dywan <- function(data="lorenc", tytul="Klasyfikacja kwantylowa - POLSKA"){
  ggplot(wynik, aes_string(y="rok", x="miesiac", fill=data)) + 
    geom_tile(colour="black",  width=0.95, height=.95) + theme_minimal() +
    geom_text(size=3, colour="gray", aes(label=format(round(wynik$anomalia,1),nsmall=1)))+
    scale_fill_manual(name="miesiąc:",  labels = etykiety, values = kolory)+
#                       guide=guide_colourbar(ticks=T, nbin=11, barheight=1, label=T, barwidth=12))+
    labs(x="", y="", fill="") +
    scale_y_discrete(labels=2013:1951,expand=c(0,0))+
    scale_x_discrete(labels=paste(c(month.abb, "YEAR"), round(colMeans(dane[,c(c(2:13,18))]),1),sep="\n"))+
    geom_vline(xintercept = 12.5, col='black', size=0.3, lty=2)+
    ggtitle(tytul)+
    theme(#legend.position=c(.50, -.07),
        legend.direction="vertical",
        legend.text=element_text(colour="black"),
        #plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=10, family="Helvetica",hjust=1),
        axis.text.x=element_text(size=9),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, family="Helvetica",size=11),
        text=element_text(family="Helvetica", size=11.5, colour = "gray20"))
}

rysuj_dywan(data = "kwantyl", tytul = "Klasyfikacja kwantylowa - POLSKA")
ggsave("dywan_kwantylowa.svg", width = 9, height = 11)

rysuj_dywan(data = "lorenc", tytul = "Klasyfikacja wg Lorenc (w oparciu o odch. stand.) - POLSKA")
ggsave("dywan_lorenc.svg", width = 9, height = 11)
