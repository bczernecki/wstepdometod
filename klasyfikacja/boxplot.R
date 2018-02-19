# boxplot
library(ggplot2)
library(reshape)
library(tidyverse)
library(RColorBrewer)
setwd("~/github/wstepdometod/")

dane <- read.table("klasyfikacja/polska.csv", dec=",",header=T) 
colnames(dane) <- c("rok", month.name, "DJF","MAM", "JJA", "SON", "YEAR")
dane2 <- as.data.frame(round(t(as.matrix(t(dane))-colMeans(dane, na.rm=T)),2))
dane2[,1] <- dane[,1]
dane3 <- select(dane2, -DJF:-SON) %>% gather(., miesiac, temp, -rok) 
dane3$rok <- as.factor(dane3$rok*-1)
dane3$miesiac <- as.factor(dane3$miesiac)
dane3$miesiac <- factor(dane3$miesiac, levels = c(month.name,"YEAR"))


f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


ggplot(dane3, aes(miesiac, temp))+ #geom_boxplot()+
  stat_summary(fun.data = f, geom="errorbar", width = 0.3) + 
  stat_summary(fun.data = f, geom="boxplot", position=position_dodge(width=5.5))+
  scale_y_continuous(name = "anomalia temperatury", breaks=seq(-5,5,1))+
  geom_hline(yintercept = 0, col='red', size=2)
ggsave("dywan_kwantylowa.svg")

dane3 <- dplyr::filter(dane3, miesiac!="YEAR")


kolory <-  colorRampPalette(colors = (c("blueviolet", "blue", "lightblue", "yellow", "orange","red","red4","lightblue", "blue","blueviolet")))(12)

ggplot(dane3, aes(temp, fill=miesiac))+ geom_density(show.legend = F)+
  facet_wrap(~miesiac, nrow=3)+
  geom_abline(intercept = 0, col="white" ,lty=2)+
  scale_fill_manual(name="miesiąc:",  labels = month.abb, values = kolory)+
  scale_x_continuous(limits=c(-10,10))+
  theme(legend.key = NULL)

ggsave("polygon.svg")
