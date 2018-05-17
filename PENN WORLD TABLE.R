#####Pacotes#####
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
library(plyr)
library(gapminder)
library(ggplot2)
library(magick)

#Dados e variaveis

K1 <-  pwt9.0
K2 <- subset(K1, select=c(year,country,isocode,pl_gdpo,rgdpe,rgdpo,pop,hc,csh_i,emp,ck,avh
))

#Real Exchange
K2 <- mutate(K2, RER = 1/pl_gdpo)

#Pib per capita
K2 <- mutate(K2, RGDPCH = rgdpo/pop)
K2 <- mutate(K2, LOGRGDPCH = log(RGDPCH))
K2 <- mutate(K2, LOGRER = log(RER))

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
  GROWTH =  log(RGDPCH/PIB2)
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

#Horas totais
K2 <- mutate(K2, Hours = emp*avh)

#Trabalho
K2 <- mutate(K2, Trabalho = emp/pop)

#Destruicao
K2 <- mutate(K2, Destruction = log(n + 0.05 +0.05))

#Para poder cruzar
names(K2)[names(K2) == 'isocode'] <- 'iso3c'


#Creating our undervalue
pdata <- pdata.frame(K2, index = c("country", "year"))
Variable <- plm(log(RER) ~ LOGRGDPCH,data = pdata, model = "within",effect = "time")
summary(Variable)
pdata$RERHAT <- fixef(Variable)
pdata <- mutate(pdata,  LOGUNDERVALUE = log(RER/RERHAT))


#Merging with the world bank data
wb_countries <- as.data.frame(wbcountries())
wb_countries <- subset(wb_countries, region != "Aggregates")


K3 <- merge(pdata,wb_countries, by = "iso3c")
summary(K3$LOGUNDERVALUE)


#mtcars %>% mutate(diff_qsec = qsec - lag(qsec))

library(quantmod)
K3 <- mutate(K3, Status = derivedFactor(
  "Developing" = RGDPCH  < 6000,
  "Developed" = RGDPCH  > 6000,
  .default = NA
))

names(K3)[names(K3) == 'country.y'] <- 'country'

#DataSets

pdataAll <- subset(K3, country != "Iraq" & country != "Laos" & country != "Korea, Rep.")

names(pdataAll)
keeps <- c("RGDPCH", "GROWTH","year","region","income","LOGUNDERVALUE","country","Status","n","Destruction","Human","pop","hc","csh_i","emp","ck","avh","Hours","Trabalho"
)
pdataAll <- pdataAll[keeps]

summary(pdataAll$GROWTH)

#pdataOld <- subset(pdataAll, unfactor(year) < 2004)

#pdataAllDeveloping <- subset(pdataAll, RGDPCH < 6000)

#pdataAllDeveloped <- subset(pdataAll, RGDPCH > 6000)

#pdataOldDeveloping <- subset(pdataOld, RGDPCH < 6000)

#pdataOldDeveloped <- subset(pdataOld, RGDPCH > 6000)

#pdataAllDeveloping1950 <- subset(pdataAllDeveloping, unfactor(year) > 1950 &  unfactor(year) < 1979)

#pdataAllDeveloping1979 <- subset(pdataAllDeveloping, unfactor(year) < 1980 &  unfactor(year) < 2004)

#pdataAllDeveloping2004 <- subset(pdataAllDeveloping, unfactor(year) > 2004 &  unfactor(year) < 2014)


#######Visualizacoes##### 

#Histogramas


require(gridExtra)


theme_set(theme_bw())

HistogramAll <-ggplot(data=pdataAll, 
                      aes(pdataAll$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2014", y="")

#HistogramOld <-ggplot(data=pdataOld, 
                      #aes(pdataOld$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2004", y="")

#HistogramDeveloping <-ggplot(data=pdataAllDeveloping, 
                            # aes(pdataAllDeveloping$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1) ) + labs(x="1950-2014 em Desenvolvimento", y="")

#HistogramDeveloped <-ggplot(data=pdataAllDeveloped, 
                            #aes(pdataAllDeveloped$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-2014 Desenvolvido", y="")

#Histogram1950 <-ggplot(data=pdataAllDeveloping1950, 
                       #aes(pdataAllDeveloping1950$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1)) + labs(x="1950-1979 em Desenvolvimento", y="")

#Histogram2004 <-ggplot(data=pdataAllDeveloping2004, 
                       #aes(pdataAllDeveloping2004$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1) ) + labs(x="2000-2014 em Desenvolvimento", y="")

#grid.arrange(HistogramAll, HistogramOld,HistogramDeveloping,HistogramDeveloped,Histogram2004,Histogram1950, ncol=2)

#grid.arrange(HistogramAll,HistogramDeveloping ,Histogram2004,HistogramOld,HistogramDeveloped,Histogram1950, ncol=3, top = "Histogramas Undervalue")

#grid.arrange(HistogramAll, HistogramOld,HistogramDeveloping,HistogramDeveloped, ncol=2)

#grid.arrange(Histogram2004,Histogram1950)

#Scatter Plots


ScatterAll <-ggplot(pdataAll, 
         aes(x = LOGUNDERVALUE , y = GROWTH)) +  xlim(c(-3, 3)) +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="ln Subvalorização",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão Subvalorização x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")

#ScatterFacetRegion <-  ScatterAll + facet_grid(region ~ .)

#ScatterFacetRegionDev <- ScatterFacetRegion %+% drop_na(pdataAll) + facet_grid(region ~ Status,drop = TRUE)

#ScatterFacetYear <-  ScatterAll %+%  pdataAllDeveloping2004 + facet_grid( year ~. )  + xlim(c(-1.3, 1.3))

print(ScatterAll)

#print(ScatterFacetRegion)

#print(ScatterFacetRegionDev)

#print(ScatterFacetYear)


ScatterAll <-ggplot(pdataAll, 
                    aes(x = log(hc) , y = GROWTH)) +  xlim(c(0, 1.5)) +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="ln Capital Humano",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão Capital Humano x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")


ScatterAll <-ggplot(pdataAll, 
                    aes(x = log(ck)  , y = GROWTH))  +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="ln Capital Físico",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão Capital Físico x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")



ScatterAll <-ggplot(pdataAll, 
                    aes(x = log(pop*1000000)  , y = GROWTH))  +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="ln População",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão População x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")



ScatterAll <-ggplot(pdataAll, 
                    aes(x = log(emp*1000000)  , y = GROWTH))  +  ylim(c(-0.2, 0.2))
ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")

ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)

ScatterAll <- ScatterAll + labs(x="ln População Economicamente Ativa",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão População Economicamente Ativa x PIB", 
                                subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")


coeftest(PanelAll,vcovSCC(PanelAll))






#Animate to win

img <- image_graph(width = 1400, height = 670, res= 96)
substet <- subset(pdataAll, unfactor(year) > 2004)
datalist <- split(substet, substet$year,drop = TRUE)
out <- lapply(datalist, function(data){
  
  ScatterAll <-ggplot(data, 
                      aes(x = LOGUNDERVALUE , y = GROWTH)) +  xlim(c(-3, 3)) +  ylim(c(-0.2, 0.2))
  ScatterAll <- ScatterAll + geom_point(aes(color = log(RGDPCH))) + scale_color_gradient(low="blue",high="green")
  
  ScatterAll <- ScatterAll + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)
  
  ScatterAll <- ScatterAll + labs(x="ln Subvalorização",  y="Crescimento do PIB per Capita(%)",title = "Gráfico de Dispersão Capital Humano x PIB", 
                                  subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")
  
  
  
  ScatterAll <- ScatterAll+ ggtitle(data$year) #+ geom_text(aes(label = country), color = "gray20", 
       #data = subset(data, country %in% pointsToLabel),check_overlap = TRUE) 
  print(ScatterAll)
})
dev.off()
animationAll5 <- image_animate(img, fps = 1)
image_write(animationAll5, "animation2007774.git")

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
  
  ScatterAll <- ScatterAll + labs(x="Undervalue",  y="Crescimento do PIB per Capita(%)",subtitle="Dados de 1950-2014", fill="Close", caption="Fonte: Penn World Table")
  
  ScatterFacet <- ScatterAll + ggtitle(data$year) 
  print(ScatterFacet)
})
dev.off()
animationAll5 <- image_animate(img, fps = 2)
image_write(animationAll5, "animationFacetTT.git")
##########

#Parte Econometrica

#Copiando Paineis Antigos
library(plm)
library(sandwich)
library(lmtest)

pdataAll <- pdataAll[
  with(pdataAll, order(country, year)),
  ]
summary(pdataAll)
pdataAll <- pdata.frame(pdataAll, index = c("country","year"))

form <- GROWTH ~ LOGUNDERVALUE + log(lag(RGDPCH))+ log(hc) + log(ck) + log(pop) + log(emp)


#Teste Hausman
phtest(form,data = pdataAll)

PanelTestDummies <- plm(form,data = pdataAll, model = "within", index =  ) 
summary(PanelTestDummies)

#Lagrange Multiplier Test - two-ways effects (Honda) for unbalanced panels
plmtest(PanelTestDummies, effect = "twoways")


PanelAll <- plm(form ,data = pdataAll, model = "within",effect = "twoways")
summary(PanelAll)
#Testes cross-sectional dependence/contemporaneous correlation   The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across
#entities are not correlatedCross-sectional dependence can lead to bias in

pcdtest(PanelAll, test = c("lm"))
pcdtest(PanelAll, test = c("cd"))

#Testing for serial correlation The null is that there is not serial correlation.
pbgtest(PanelAll)

#Testing for heteroskedasticity  The null hypothesis for the Breusch-Pagan test is homoskedasticity
bptest(PanelAll)

#Nosso Modelo" Nonparametric robust covariance matrix estimators a la Driscoll and Kraay for panel models with cross-sectional and serial correlation.
coeftest(PanelAll,vcovSCC(PanelAll))




#Erro Robusto
#"arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
#coeftest(PanelAll,vcovHC(PanelAll, method = "arellano",type="HC3"))

# "Nosso Modelo" Nonparametric robust covariance matrix estimators a la Driscoll and Kraay for panel models with cross-sectional and serial correlation.
#coeftest(PanelAll,vcovSCC(PanelAll))


#ExtraDivisao Developed












