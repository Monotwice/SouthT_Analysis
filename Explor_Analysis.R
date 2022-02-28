####################
####### EXPLORATORY ANALYSIS

#plots

library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(gridExtra)

dfC$Month <- as.Date(cut(dfC$date,
                         breaks = "month"))
dfC$Week <- as.Date(cut(dfC$date,
                        breaks = "week",
                        start.on.monday = FALSE)) # changes weekly break point to Sunday

c = ggplot(data = dfC,
       aes(Month, rain_mean_0)) +
  stat_summary(fun  = mean, # adds up all observations for the month
               geom = "line")+
  ylab("Mean Precipitation (mm)") + 
  xlab("Date")+
  ggtitle("Center Rainfall 2000 - 2021")+
  stat_smooth(se = FALSE)

#c = ggplotly(c)

dfS$Month <- as.Date(cut(dfS$date,
                         breaks = "month"))
dfS$Week <- as.Date(cut(dfS$date,
                        breaks = "week",
                        start.on.monday = FALSE)) # changes weekly break point to Sunday

s = ggplot(data = dfS,
           aes(Month, rain_mean_0)) +
  stat_summary(fun  = mean, # adds up all observations for the month
               geom = "line")+
  ylab("Mean Precipitation (mm)") + 
  xlab("Date")+
  ggtitle("South Rainfall 2000 - 2021")+
  stat_smooth(se = FALSE)


dfE$Month <- as.Date(cut(dfE$date,
                         breaks = "month"))
dfE$Week <- as.Date(cut(dfE$date,
                        breaks = "week",
                        start.on.monday = FALSE)) # changes weekly break point to Sunday

e = ggplot(data = dfE,
           aes(Month, rain_mean_0)) +
  stat_summary(fun  = mean, # adds up all observations for the month
               geom = "line")+
  ylab("Mean Precipitation (mm)") + 
  xlab("Date")+
  ggtitle("East Monthly mean Rainfall 2000 - 2021")+
  stat_smooth(se = FALSE)

dfW$Month <- as.Date(cut(dfW$date,
                         breaks = "month"))
dfW$Week <- as.Date(cut(dfW$date,
                        breaks = "week",
                        start.on.monday = FALSE)) 

w = ggplot(data = dfW,
           aes(Month, rain_mean_0)) +
  stat_summary(fun  = mean, # adds up all observations for the month
               geom = "line")+
  ylab("Mean Precipitation (mm)") + 
  xlab("Date")+
  ggtitle("West Monthly mean Rainfall 2000 - 2021")+
  stat_smooth(se = FALSE)

#s = ggplotly(s)

grid.arrange(c,s,e,w, ncol = 1)
 
##################################################################
#################################################################
#################################################################

library(lubridate)
library(forecast)
bymonth <- aggregate(rain_mean_0~year(date),
                     data=dfC,FUN=mean)

out = dfC %>% group_by(date) %>% summarise_each(funs = list(mean = mean, median = median))



##################################################################
###################################################################
#Seasonality
# Works with temperature data 
# doesnt work for daily precip data

ts_rain_c <- ts(dfC$rain_mean_0, frequency=365.25, start = c(2000, 1,1))
plot.ts(ts_rain_c)

#tslsItest <- ts(df$mnth_pr, frequency=364, start=c(year(minday), as.numeric(format(minday, "%d"))))
#lsI_ts <- window(tslsI, start = c(2000,1))

decomp <- stl(ts_rain_c, s.window= 7)
plot(decomp)


Tt <- trendcycle(decomp)
St <- seasonal(decomp)
Rt <- remainder(decomp)

#Trend Strength Calculation
Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
#Seasonal Strength Calculation
Fs <- max(0,1 - (var(Rt)/var(St + Rt)))

data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)

########################################################################

ggplot(data = dfC, aes(x = date, y = rain_mean_0))+
  geom_line()+
  ylab("Rainfall (mm)") + xlab("Datetime")+
  ggtitle("catchA Rainfall 2000 - 2021")+
  stat_smooth(se = FALSE)


mean(dfC$rain_mean_0)
mean(dfS$rain_mean_0)
mean(dfW$rain_mean_0)
mean(dfE$rain_mean_0)
sd(dfE$rain_mean_0)


C.rain <- dfC %>% 
  select(date, rain_mean_0) 
colnames(C.rain) <- c("date", "Center_Mean_Precipitation")

W.rain <- dfW %>% 
  select(date, rain_mean_0)  
  colnames(W.rain) <- c("date", "West_Mean_Precipitation")

E.rain <- dfE %>% 
  select(date, rain_mean_0)
  colnames(E.rain) <- c("date", "East Mean Precipitation")

S.rain <- dfS %>% 
  select(date, rain_mean_0)
  colnames(S.rain) <- c("date", "South Mean Precipitation")

precip <- merge(C.rain, W.rain, by = "date")
precip <- merge(precip, E.rain, by = "date")
precip <- merge(precip, S.rain, by = "date")

precip <- data.frame(lapply(precip,    # Using Base R functions
                     function(x) if(is.numeric(x)) round(x, 1) else x))


z = ggplot(data = precip,aes(x = date, y = Center_Mean_Precipitation )) +
  geom_bar(stat = "identity")
  ylab("Rainfall (mm)") + xlab("Datetime")+
  ggtitle("catchA Rainfall 2000 - 2021")

ggplotly(z)




