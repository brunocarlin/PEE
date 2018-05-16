#Pacotes
devtools::install_github("dgrtwo/gganimate")
library(pwt9)
library(plm)
library(varhandle)
library(ggplot2)
library(tibble)
library(ggplot2)
library(WDI)
library(dplyr)
library(tidyr)
library(data.table)
library(wbstats)
library(mosaic)
library(gganimate)

library(gapminder)
library(ggplot2)
library(magick)

#Dados e variaveis

K1 <-  pwt9.0
K2 <- subset(K1, select=c(year,country,isocode,pl_gdpo,rgdpe,rgdpo,pop,hc,csh_i))

#Real Exchange
K2 <- mutate(K2, RER = 1/pl_gdpo)

#Pib per capita
K2 <- mutate(K2, RGDPCH = rgdpo/pop)

#n
K2 <- ddply( 
  K2, .(country), transform,
  # This assumes that the data is sorted
  POP2 = c( NA, pop[-length(pop)] ) 
)
K2 <- ddply( 
  K2, .(country), transform,
  # This assumes that the data is sorted
  n =  log(pop/POP2)
)
#summary(K2$n)


#Crescimento pib

K2 <- ddply( 
  K2, .(country), transform,
  # This assumes that the data is sorted
  PIB2 = c( NA, RGDPCH[-length(RGDPCH)] ) 
)
K2 <- ddply( 
  K2, .(country), transform,
  # This assumes that the data is sorted
  GROWNTH =  log(RGDPCH/PIB2)
)
#summary(K2$GROWNTH)

#Classificao
library(quantmod)
K2 <- mutate(K2, Status = derivedFactor(
  "Developing" = RGDPCH  < 6000,
  "Developed" = RGDPCH  > 6000,
  .default = NA
))


#HC

K2 <- ddply( 
  K2, .(country), transform,
  # This assumes that the data is sorted
  hum2 = c( NA, hc[-length(hc)] ) 
)
K2 <- mutate(K2, Human = log(hc/hum2))
summary(K2$Human)


#Destruicao
K2 <- mutate(K2, Destruction = log(n + 0.05 +0.05))

#Para poder cruzar
names(K2)[names(K2) == 'isocode'] <- 'iso3c'








wb_countries <- as.data.frame(wbcountries())
wb_countries <- subset(wb_countries, region != "Aggregates")


K3 <- merge(K2,wb_countries, by = "iso3c")

K3 <- mutate(K3, RER = 1/pl_gdpo)
K3 <- mutate(K3, RGDPCH = rgdpo/pop)
#mtcars %>% mutate(diff_qsec = qsec - lag(qsec))

K3 <- mutate(K3, GROWTH = log(rgdpo/lag(rgdpo)))

library(quantmod)
K3 <- mutate(K3, Status = derivedFactor(
  "Developing" = RGDPCH  < 6000,
  "Developed" = RGDPCH  > 6000,
  .default = NA
))

names(K3)[names(K3) == 'country.y'] <- 'country'

pdata <- pdata.frame(K3, index = c("country", "year"))




Variable <- plm(log(RER) ~ log(RGDPCH),data = pdata, model = "within",effect = "time")

summary(Variable)

pdata$RERHAT <- fixef(Variable)

pdata <- mutate(pdata,  LOGUNDERVALUE = log(RER)- log(RERHAT))


#DataSets

pdataAll <- subset(pdata, country != "Iraq" & country != "Laos" & country != "Korea, Rep.")

names(pdataAll)
keeps <- c("RGDPCH", "GROWTH","year","region","income","LOGUNDERVALUE","country","Status")
pdataAll <- pdataAll[keeps]


pdataOld <- subset(pdataAll, unfactor(year) < 2004)

pdataAllDeveloping <- subset(pdataAll, RGDPCH < 6000)

pdataAllDeveloped <- subset(pdataAll, RGDPCH > 6000)

pdataOldDeveloping <- subset(pdataOld, RGDPCH < 6000)

pdataOldDeveloped <- subset(pdataOld, RGDPCH > 6000)

pdataAllDeveloping1950 <- subset(pdataAllDeveloping, unfactor(year) > 1950 &  unfactor(year) < 1979)

pdataAllDeveloping1979 <- subset(pdataAllDeveloping, unfactor(year) < 1980 &  unfactor(year) < 2004)

pdataAllDeveloping2004 <- subset(pdataAllDeveloping, unfactor(year) > 2004 &  unfactor(year) < 2014)



#Visualizacoes 

#Histogramas


require(gridExtra)


theme_set(theme_bw())

HistogramAll <-ggplot(data=pdataAll, 
                      aes(pdataAll$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2014", y="")

HistogramOld <-ggplot(data=pdataOld, 
                      aes(pdataOld$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2004", y="")

HistogramDeveloping <-ggplot(data=pdataAllDeveloping, 
                             aes(pdataAllDeveloping$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1) ) + labs(x="1950-2014 em Desenvolvimento", y="")

HistogramDeveloped <-ggplot(data=pdataAllDeveloped, 
                            aes(pdataAllDeveloped$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2014 Desenvolvido", y="")

Histogram1950 <-ggplot(data=pdataAllDeveloping1950, 
                       aes(pdataAllDeveloping1950$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-1979 em Desenvolvimento", y="")

Histogram2004 <-ggplot(data=pdataAllDeveloping2004, 
                       aes(pdataAllDeveloping2004$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1) ) + labs(x="2000-2014 em Desenvolvimento", y="")

grid.arrange(HistogramAll, HistogramOld,HistogramDeveloping,HistogramDeveloped,Histogram2004,Histogram1950, ncol=2)

grid.arrange(HistogramAll,HistogramDeveloping ,Histogram2004,HistogramOld,HistogramDeveloped,Histogram1950, ncol=3, top = "Histogramas Undervalue")

grid.arrange(HistogramAll, HistogramOld,HistogramDeveloping,HistogramDeveloped, ncol=2)

grid.arrange(Histogram2004,Histogram1950)

#Scatter Plots


ScatterAll <-ggplot(pdataAll, 
         aes(x = LOGUNDERVALUE , y = GROWTH)) +  xlim(c(-3, 3)) +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(shape = income, color = region))

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="Undervalue",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão Undervalue x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fontes: Penn World Table e World Bank")

ScatterFacetRegion <-  ScatterAll + facet_grid(region ~ .)

ScatterFacetRegionDev <- ScatterFacetRegion %+% drop_na(pdataAll) + facet_grid(region ~ Status,drop = TRUE)

ScatterFacetYear <-  ScatterAll %+%  pdataAllDeveloping2004 + facet_grid( year ~. )  + xlim(c(-1.3, 1.3))

print(ScatterAll)

print(ScatterFacetRegion)

print(ScatterFacetRegionDev)

print(ScatterFacetYear)


#Animate to win

img <- image_graph(width = 1920, height = 1080, res= 96)
substet <- subset(pdataAll, unfactor(year) > 2004)
datalist <- split(substet, substet$year,drop = TRUE)
out <- lapply(datalist, function(data){
  
  ScatterAll <-ggplot(data, 
                      aes(x = LOGUNDERVALUE , y = GROWTH)) +  xlim(c(-3, 3)) +  ylim(c(-0.2, 0.2))
  ScatterAll <- ScatterAll + geom_point(aes(shape = income, color = region))
  
  ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0) 
  
  ScatterAll <- ScatterAll + labs(x="Undervalue",  y="Crescimento do PIB per Capita(%)",subtitle="Dados de 1950-2014", fill="Close", caption="Sources: Penn World Table e World Bank")
  
  ScatterAll <- ScatterAll+ ggtitle(data$year) #+ geom_text(aes(label = country), color = "gray20", 
       #data = subset(data, country %in% pointsToLabel),check_overlap = TRUE) 
  print(ScatterAll)
})
dev.off()
animationAll5 <- image_animate(img, fps = 1)
image_write(animationAll5, "animation20074.git")

#Animate to realy win
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")


img <- image_graph(width = 1920, height = 1080, res= 150)
substet <- subset(pdataAll, unfactor(year) > 1980) 
datalist <- split(substet, substet$year,drop = TRUE)
out <- lapply(datalist, function(data){
  
  ScatterAll <-ggplot(data, 
                      aes(x = LOGUNDERVALUE , y = GROWTH)) +  xlim(c(-1.5, 1.5)) +  ylim(c(-0.2, 0.2))
  ScatterAll <- ScatterAll + geom_point(aes(shape = income, color = region))
  
  ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0) 
  
  ScatterAll <- ScatterAll + labs(x="Undervalue",  y="Crescimento do PIB per Capita(%)",subtitle="Dados de 1950-2014", fill="Close", caption="Sources: Penn World Table e World Bank")
  
  ScatterFacet <- ScatterAll + facet_grid( . ~ Status,drop = TRUE)
  ScatterFacet <- ScatterFacet+ ggtitle(data$year) + geom_text(aes(label = country), color = "gray20", 
  data = subset(data, country %in% pointsToLabel),check_overlap = TRUE) 
  print(ScatterFacet)
})
dev.off()
animationAll5 <- image_animate(img, fps = 2)
image_write(animationAll5, "animationFacetTT.git")


#Parte Econometrica

#Copiando Paineis Antigos

form <- GROWTH ~ LOGUNDERVALUE + log(lag(RGDPCH))

PanelOld <- plm(form,data = pdataOld, model = "within",effect = "twoways")
summary(PanelOld)

PanelOldDeveloping <- plm(form,data = pdataAllDeveloping, model = "within",effect = "twoways")
summary(PanelOldDeveloping)

PanelOldDeveloped <- plm(form,data = pdataOldDeveloped, model = "within",effect = "twoways")
summary(PanelOldDeveloped)

PanelDeveloping1950 <- plm(form,data = pdataAllDeveloping1950, model = "within",effect = "twoways")
summary(PanelDeveloping1950)


PanelAllDeveloping1979 <- plm(form,data = pdataAllDeveloping1979, model = "within",effect = "twoways")
summary(PanelAllDeveloping1979)

PanelAllDeveloping2004 <- plm(form,data = pdataAllDeveloping2004, model = "within",effect = "twoways")
summary(PanelAllDeveloping2004)



#Fixed Effect Models

library(sandwich)
library(lmtest)

PanelAll <- plm(form,data = pdataAllDeveloping2004, model = "within",effect = "twoways")
summary(PanelAll)





#Testes cross-sectional dependence/contemporaneous correlation   The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across
#entities are not correlatedCross-sectional dependence can lead to bias in

pcdtest(PanelAll, test = c("lm"))
pcdtest(PanelAll, test = c("cd"))

#Testing for serial correlation The null is that there is not serial correlation.
pbgtest(PanelAll)

#Testing for heteroskedasticity  The null hypothesis for the Breusch-Pagan test is homoskedasticity
bptest(PanelAll)


#Erro Robusto
#"arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
coeftest(PanelAll,vcovHC(PanelAll, method = "arellano",type="HC3"))

# "Nosso Modelo" Nonparametric robust covariance matrix estimators a la Driscoll and Kraay for panel models with cross-sectional and serial correlation.
coeftest(PanelAll,vcovSCC(PanelAll,type = "HC3"))


#ExtraDivisao Developed

PanelDeveloping <- plm(form,data = pdataAllDeveloping, model = "within",effect = "twoways")
summary(PanelDeveloping)
coeftest(PanelDeveloping,vcovSCC(PanelDeveloping,type = "HC3"))

PanelAjustado <- plm(form,data = pdataAllDeveloping2004, model = "within",effect = "twoways")
summary(PanelAjustado)
coeftest(PanelAjustado,vcovSCC(PanelAjustado,type = "HC3"))
plmtest(PanelAjustado, effect = "twoways")













