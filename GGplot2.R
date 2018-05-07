library(forecast)
library(ggplot2)
library(forecastHybrid)

getwd()
.libPaths("C:\Users\Anton\Documents\R\win-library\3.5")


list.of.packages <- c("forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

install.packages("forecast", dependencies = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(forecast)

install.packages("forecast", dependencies=...)



Pib <- ts(Produto, start = 1997, frequency = 4)
Camb<- ts(Cambio, start = 1930, frequency = 12)

Camb2 <- window(Camb, start = 1998,end = c(2017,12))
Pib2 <- window(Pib, start = 1998,end = c(2017,4))
Camb2
Pib2

plot(Pib2,Camb2)

library(dplyr)
library(ggplot2)
library(zoo)

f <- ggplot(Pib2, Camb2, aes(Pib2, Camb2))
g
ggplot() + 
  
  geom_line(data = Pib2, aes(x = time(Pib2), y = Pib2,color = "blue") ) +
  geom_line(data = Camb2, aes(x = time(Camb2), y = Camb2,color = "red") )+
  xlab('Ano')+
  ylab("Taxas")+
  labs(title="Câmbio X Crescimento",x="", y = "Taxas")+
  scale_color_discrete(name = "", labels = c("Crescimento do Pib(%)", "Câmbio nominal"))+
  theme_minimal()
  
  

dualplot(x1 = time(Pib), y1 = Pib,
         x2 = time(Camb), y2 = Camb, 
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", main = "Price per share of two key firms")

ggplot_dual_axis(Pib, Camb, "x")
library(plm)



R> grun.fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")


ggplot(Pib2,aes(x,y))+geom_line(aes(color="First line"))+
  geom_line(data=Camb2,aes(color="Second line"))+
  labs(color="Legend text")
