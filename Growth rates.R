df <- data.frame(Category=c(rep("A",6),rep("B",6)),
                 Year=rep(2010:2015,2),Value=1:12)


library(plyr)
ddply(df,"Category",transform,
      Growth=c(NA,exp(diff(log(Value)))-1))

grow<- ddply(K3,"country",transform,
      Growth=c(NA,log(diff(pop))))
      
      
      
      d <- data.frame( 
        User = rep( LETTERS[1:3], each=10 ),
        Date = seq.Date( Sys.Date(), length=30, by="day" ),
        Value = rep(1:10, 3)
      )
      library(plyr)
      d <- ddply( 
        d, .(User), transform,
        # This assumes that the data is sorted
        Value = c( NA, Value[-length(Value)] ) 
      )
      
      K3 <- K3[
        with(K3, order(country, year)),
        ]
      K3 <- ddply( 
        K3, .(country), transform,
        # This assumes that the data is sorted
        POP2 = c( NA, pop[-length(pop)] ) 
      )
      K3 <- ddply( 
        K3, .(country), transform,
        # This assumes that the data is sorted
        POP3 =  log(pop/POP2)
      )
      summary(K3$POP3)
      
      
      K3<- K3[
        with(K3, order(country, year)),
        ]
      K3 <- ddply( 
        K3, .(country), transform,
        # This assumes that the data is sorted
        pibb = c( NA, RGDPCH[-length(RGDPCH)] ) 
      )
      K3 <- ddply( 
        K3, .(country), transform,
        # This assumes that the data is sorted
        GROWTH2 =  log(RGDPCH/pibb)
      )
      
      summary(K3$GROWTH2)
      