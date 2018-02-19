library(ggplot2)
library(reshape)
library(tidyverse)

dane <- read.table("klasyfikacja/polska.csv", dec=",",header=T) 
colnames(dane) <- c("rok", month.name, "DJF","MAM", "JJA", "SON", "YEAR")
dane2 <- as.data.frame(round(t(as.matrix(t(dane))-colMeans(dane, na.rm=T)),2))
dane2[,1] <- dane[,1]

dane3 <- select(dane2, -DJF:-SON) %>% gather(., miesiac, temp, -rok) 
dane3$rok <- as.factor(dane3$rok*-1)

dane3$miesiac <- as.factor(dane3$miesiac)
dane3$miesiac <- factor(dane3$miesiac, levels = c(month.name,"YEAR"))

library(plyr)
anomal <- function(x) x-mean(x)

klasa <- function(x){
  percentile <- ecdf(x)
  round(percentile(x),2)
} 

klasa_lorenc <- function(x){
cut(x, 
    breaks = (sd(x)*c(-99,-2.5,-2.0,-1.5,-1.0,-0.5, 0.5, 1.0, 1.5, 2.0, 2.5,99)),
    labels = 1:11)
}
    

R2 <- ddply(dane3, "miesiac", transform, 
            anomalia=anomal(temp), percentyl=klasa(temp), lorenc=klasa_lorenc(temp))
R2$percentylklasa <- cut(R2$percentyl, breaks=c(0,5,10,20,30,40,60,70,80,90,95,100)/100 ,labels=1:11)
head(R2)
#R2$percentylklasa <- as.numeric(as.character(R2$percentylklasa))

ggplot(dane3, aes(y=rok, x=miesiac, fill=temp)) + 
  geom_tile(colour="black",  width=0.95, height=.95) + theme_minimal() +
  geom_text(size=3, colour="gray", aes(label=round(dane3$temp,1)))+
  scale_fill_gradientn(name = "anomalia \ntemperatury (*C): ",
                      colours=colorRampPalette(colors = rev(c("red4","red","orange","white","lightblue", "blue","blueviolet")))(11),
                       limits=c(-10, 10),
                       breaks=seq(-10, 10, by=1), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=seq(-10, 10, by=1),
                       guide=guide_colourbar(ticks=T, nbin=11, barheight=1, label=T, barwidth=12))+
  labs(x="", y="", fill="") +
  scale_y_discrete(labels=2013:1951,expand=c(0,0))+
  scale_x_discrete(labels=paste(c(month.abb, "YEAR"), round(colMeans(dane[,c(c(2:13,18))]),1),sep="\n"))+
  geom_vline(xintercept = 12.5, col='black', size=0.3, lty=2)+
  ggtitle("Klasyfikacja - POLSKA")+
  theme(legend.position=c(.50, -.07),
        legend.direction="horizontal",
        legend.text=element_text(colour="black"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=10, family="Helvetica",hjust=1),
        axis.text.x=element_text(size=9),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, family="Helvetica",size=11),
        text=element_text(family="Helvetica", size=11.5, colour = "gray20"))


library(RColorBrewer)
kolory <-  colorRampPalette(colors = rev(c("red4","red","orange","white","lightblue", "blue","blueviolet")))(11)
etykiety <- rev(c("ekstremalnie ciepły", "anomalnie ciepły", "bardzo ciepły", "ciepły", "lekko ciepły",
            " normalny ", "lekko chłodny" , "chłodny", "bardzo chłodny", "anomalnie chłodny", "ekstremalnie chłodny"))
#etykiety <- 1:11


ggplot(R2, aes(y=rok, x=miesiac, fill=percentylklasa)) + 
  geom_tile(colour="black",  width=0.95, height=.95) + theme_minimal() +
  geom_text(size=3, colour="gray", aes(label=round(R2$temp,1)))+
  scale_fill_manual(name="miesiąc:",  labels = etykiety, values = kolory)+
#                       guide=guide_colourbar(ticks=T, nbin=11, barheight=1, label=T, barwidth=12))+
  labs(x="", y="", fill="") +
  scale_y_discrete(labels=2013:1951,expand=c(0,0))+
  scale_x_discrete(labels=paste(c(month.abb, "YEAR"), round(colMeans(dane[,c(c(2:13,18))]),1),sep="\n"))+
  geom_vline(xintercept = 12.5, col='black', size=0.3, lty=2)+
  ggtitle("Klasyfikacja kwantylowa - POLSKA")+
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


