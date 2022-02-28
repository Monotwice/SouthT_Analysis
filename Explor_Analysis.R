####################
####### EXPLORATORY ANALYSIS

#plots

library(ggplot2)
library(dplyr)

ggplot(data = dfE, aes(x = date, y = rain_mean_0))+
  geom_line()+
  ylab("Rainfall (mm)") + xlab("Datetime")+
  ggtitle("catchA Rainfall 2000 - 2021")+
  geom_smooth(se = FALSE)


mean(dfC$rain_mean_0)
mean(dfS$rain_mean_0)
mean(dfW$rain_mean_0)
mean(dfE$rain_mean_0)
sd(dfE$rain_mean_0)


C.rain <- dfC %>% 
  select(date, rain_mean_0) 
colnames(C.rain) <- c("date", "Center Mean Precipitation ")

W.rain <- dfW %>% 
  select(date, rain_mean_0)  
  colnames(W.rain) <- c("date", "West Mean Precipitation ")

E.rain <- dfE %>% 
  select(date, rain_mean_0)
  colnames(E.rain) <- c("date", "East Mean Precipitation ")

S.rain <- dfS %>% 
  select(date, rain_mean_0)
  colnames(S.rain) <- c("date", "South Mean Precipitation ")

precip <- merge(C.rain, W.rain, by = "date")
precip <- merge(precip, E.rain, by = "date")
precip <- merge(precip, S.rain, by = "date")

