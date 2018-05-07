olddata_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
                           1   M     7.9  12.3  10.7
                           2   F     6.3  10.6  11.1
                           3   F     9.5  13.1  13.8
                           4   M    11.5  13.4  12.9
                           ')
# Make sure the subject column is a factor
olddata_wide$subject <- factor(olddata_wide$subject)


olddata_wide


library(tidyr)
names(olddata_wide[3:5])
olddata_wide[]
data_long2 <- gather(olddata_wide, condition, measurement,names(olddata_wide[3:5]),factor_key = TRUE)

                    