library(pwt9)


k <-  pwt9.0

#K2 <-  c(pwt9.0$rgdpe,pwt9.0$pop)

#K2 <- k[c(1,5:10)]

#k[ , c("x","y")]

K2 <- subset(k, select=c(year,country,isocode,csh_m,csh_x,pop,emp,hc,csh_r))
names(K2)[names(K2) == 'isocode'] <- 'iso3c'


K3 <- merge(K1,K2,by = c("year","iso3c"))


K4 <- drop_na(K3)      
