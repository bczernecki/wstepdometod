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


f_perc <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


f_sd <- function(x) {
  r <- sd(x)*c(-2.5,-1,0,1,2.5)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


p1 <- ggplot(dane3, aes(miesiac, temp))+ #geom_boxplot()+
  stat_summary(fun.data = f_perc, geom="errorbar", width = 0.3) + 
  stat_summary(fun.data = f_perc, geom="boxplot", position=position_dodge(width=5.5))+
  scale_y_continuous(name = "anomalia temperatury", breaks=seq(-9,9,1), limits = c(-9,9))+
  geom_hline(yintercept = 0, col='red', size=2)+ggtitle("Rozstrzał - klasyfikacja kwanthylowa")

p2 <- ggplot(dane3, aes(miesiac, temp))+ #geom_boxplot()+
  stat_summary(fun.data = f_sd, geom="errorbar", width = 0.3) + 
  stat_summary(fun.data = f_sd, geom="boxplot", position=position_dodge(width=5.5))+
  scale_y_continuous(name = "anomalia temperatury", breaks=seq(-9,9,1), limits = c(-9,9))+
  geom_hline(yintercept = 0, col='red', size=2)+ggtitle("Rozstrzał - klasyfikacja oparta o odch. stand.")

library("ggpubr")
ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave("boxplot.svg", width = 14, height = 6)

dane3 <- dplyr::filter(dane3, miesiac!="YEAR")


kolory <-  colorRampPalette(colors = (c("blueviolet", "blue", "lightblue", "yellow", "orange","red","red4","lightblue", "blue","blueviolet")))(12)

ggplot(dane3, aes(temp, fill=miesiac))+ geom_density(show.legend = F)+
  facet_wrap(~miesiac, nrow=3)+
  geom_abline(intercept = 0, col="white" ,lty=2)+
  scale_fill_manual(name="miesiąc:",  labels = month.abb, values = kolory)+
  scale_x_continuous(limits=c(-10,10))+
  theme(legend.key = NULL)

ggsave("polygon.svg")


# sprawdzmy jeszcze zaleznosci na wartosciach surowych (nie-anomaliach)
dane4 <- select(dane, -DJF:-SON) %>% gather(., miesiac, temp, -rok) %>% 
  mutate(miesiac=factor(miesiac, levels=month.name)) %>% filter(miesiac!="YEAR")

srednie <-  dane4 %>% dplyr::group_by(miesiac) %>% dplyr::summarise(srednie=mean(temp))
srednie

p <- ggplot(dane4, aes(temp, fill=miesiac))+ geom_density(show.legend = F)+
  facet_wrap(~miesiac, nrow=3,scales = "free_x")+
  #geom_vline(intercept = 0, col="white" ,lty=2)+
  theme(legend.key = NULL)
p+geom_text(data=srednie, aes(x=srednie+2, y=0.3, label=round(srednie,1)))+
  geom_vline(data=srednie, aes(xintercept = srednie))

ggsave("polygon_surowe.svg")
