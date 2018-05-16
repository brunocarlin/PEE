library(ggplot2)
library(WDI)
library(dplyr)
library(tidyr)
library(data.table)
library(wbstats)
ok <- WDI_data
new_wdi_cache <- WDIcache() 

wb_countries <- as.data.frame(wbcountries())
wb_countries <- subset(wb_countries, region != "Aggregates")
names(wb_countries)

wb_countries <- wbcountries() 
WDIsearch("extra")
WDIsearch('exchange')
WDIsearch('gdp.*capita.*constant')
WDIsearch(
  string = "gdp",
  field = "name",
  short = TRUE,
  cache = NULL
)

#Real effective exchange rate index (2010 = 100),GDP per capita (constant 2010 US$),6 tipos cambio, GDP per capita growth (annual %)
#
K <-
  WDI(
    country = "all",
    indicator = c("NY.GDP.PCAP.KD", "PX.REX.REER", "NY.GDP.PCAP.KD.ZG","FP.CPI.TOTL.ZG"),
    start = 2000,
    end = 2014,
    extra = TRUE,
    cache = NULL
  )
K1 <- drop_na(K)

names(K1)[names(K1) == 'NY.GDP.PCAP.KD'] <- 'GDP per Capita'
names(K1)[names(K1) == 'PX.REX.REER'] <- 'REER'
names(K1)[names(K1) == 'NY.GDP.PCAP.KD.ZG'] <- 'GDP growth per Capita'
#names(K1)[names(K1) == 'FR.INR.RINR'] <- 'Real Interest Rate'
names(K1)[names(K1) == 'FP.CPI.TOTL.ZG'] <- 'Inflation'





#k5 <- mutate(k4, logpib = log(NY.GDP.PCAP.KD))  mydata$agecat[age > 75] <- "Elder"



pl <-
  ggplot(subset(k6, year == 2016 &
                  region != "Aggregates"),
         aes(x = IQ.CPA.TRAN.XQ , y = logpib))
p2 <-
  pl + geom_point(aes(shape = income, color = region), stroke = 3)
p3 <-
  p2 + scale_shape(solid = FALSE) + geom_smooth(method = "lm", level = 0)
print(p3)


PL1 <- ggplot(data=K3, aes(K3$LOGUNDERVALUE, ..density..)) + geom_histogram(breaks=seq(-1.9, 1.9, by=0.1))

  
print(pl2)

#datar <- subset(k6, year == 2016 &  income != "Aggregates")

#Testes
pbgtest

plmtest

mean(K6$LOGUNDERVALUE)


hist(K6$LOGUNDERVALUE,
     probability=TRUE)




K2 <- subset(K1, select=c(year,country,isocode,csh_m,csh_x,pop,emp,hc,csh_r))
names(K2)[names(K2) == 'isocode'] <- 'iso3c'

