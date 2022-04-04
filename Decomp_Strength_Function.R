##### Function stl decomposition


library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(gridExtra)
library(tsutils)
library(lubridate)
library(forecast)
library(Kendall)

library(trend)


decomp_landslide_freq <- function(aoi = NULL) {
  
  aoi <- aoi %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date))
  
  
  aoi[is.na(aoi)] <- 0
  
  ls_0 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(frequency)) %>%
    as.data.frame()
  
  ls_5 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cnls_5d)) %>%
    as.data.frame()
  
  ls_25 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cnls_25d)) %>%
    as.data.frame()
  
  
  ls_0$day <- 1
  ls_5$day <- 1
  ls_25$day <- 1
  
  ls_0$date = as.Date(with(ls_0,paste(year,month, day, sep="-")), "%Y-%m-%d")
  ls_5$date = as.Date(with(ls_5,paste(year,month, day, sep="-")), "%Y-%m-%d")
  ls_25$date = as.Date(with(ls_25,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  ls_0 <- ls_0[order(as.Date(ls_0$date, format="%Y-%m-%d")),]
  ls_5 <- ls_5[order(as.Date(ls_5$date, format="%Y-%m-%d")),]
  ls_25 <- ls_25[order(as.Date(ls_25$date, format="%Y-%m-%d")),]
  
  ts_ls_0<- ts(ls_0$ls, frequency = 12, start= c(2000, 1))
  ts_ls_5<- ts(ls_5$ls, frequency = 12, start= c(2000, 1))
  ts_ls_25<- ts(ls_25$ls, frequency = 12, start= c(2000, 1))
  
  decomp_0 <- stl(ts_ls_0, s.window = 21, t.window = 12)
  decomp_5 <- stl(ts_ls_5, s.window = 21, t.window = 12)
  decomp_25 <- stl(ts_ls_25, s.window = 21, t.window = 12)
  
  
  Tt <- trendcycle(decomp_0)
  St <- seasonal(decomp_0)
  Rt <- remainder(decomp_0)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_ls_0 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_5)
  St <- seasonal(decomp_5)
  Rt <- remainder(decomp_5)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_ls_5 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_25)
  St <- seasonal(decomp_25)
  Rt <- remainder(decomp_25)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_ls_25 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Strength_aoi <- rbind(strength_ls_0, strength_ls_5, strength_ls_25)
  
  ##### Kendal
  SK_0 <- SeasonalMannKendall(ts_ls_0)
  SK_5 <- SeasonalMannKendall(ts_ls_5)
  SK_25 <- SeasonalMannKendall(ts_ls_25)
  
  Kendal <- rbind(SK_0, SK_5, SK_25)
  res <- cbind(Strength_aoi, Kendal)
  
  return(res)
  
}

decomp_rain <- function(aoi = NULL) {
  
  
  
  aoi <- aoi %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date))
  
  
  aoi[is.na(aoi)] <- 0
  
  p1 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(rain_mean_0)) %>%
    as.data.frame()
  
  p2 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cmeanR_5d)) %>%
    as.data.frame()
  
  p3 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cmeanR_5d)) %>%
    as.data.frame()
  
  
  p1$day <- 1
  p2$day <- 1
  p3$day <- 1
  
  p1$date = as.Date(with(p1,paste(year,month, day, sep="-")), "%Y-%m-%d")
  p2$date = as.Date(with(p2,paste(year,month, day, sep="-")), "%Y-%m-%d")
  p3$date = as.Date(with(p3,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  p1 <- p1[order(as.Date(p1$date, format="%Y-%m-%d")),]
  p2 <- p2[order(as.Date(p2$date, format="%Y-%m-%d")),]
  p3 <- p3[order(as.Date(p3$date, format="%Y-%m-%d")),]
  
  ts_p1<- ts(p1$ls, frequency = 12, start= c(2000, 1))
  ts_p2<- ts(p2$ls, frequency = 12, start= c(2000, 1))
  ts_p3<- ts(p3$ls, frequency = 12, start= c(2000, 1))
  
  decomp_0 <- stl(ts_p1, s.window = 21, t.window = 12)
  decomp_5 <- stl(ts_p2, s.window = 21, t.window = 12)
  decomp_25 <- stl(ts_p3, s.window = 21, t.window = 12)
  
  
  Tt <- trendcycle(decomp_0)
  St <- seasonal(decomp_0)
  Rt <- remainder(decomp_0)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p1 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_5)
  St <- seasonal(decomp_5)
  Rt <- remainder(decomp_5)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p2 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_25)
  St <- seasonal(decomp_25)
  Rt <- remainder(decomp_25)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p3 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Strength_aoi <- rbind(strength_p1, strength_p2, strength_p3)
  
  ##### Kendal
  SK_rain_0 <- SeasonalMannKendall(ts_p1)
  SK_rain_5d <- SeasonalMannKendall(ts_p2)
  SK_rain_25d <- SeasonalMannKendall(ts_p3)
  
  Kendal <- rbind(SK_rain_0, SK_rain_5d, SK_rain_25d)
  res <- cbind(Strength_aoi, Kendal)
  
  return(res)
  
}

decomp_temp <- function(aoi = NULL) {
  
  
  
  aoi <- aoi %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date))
  
  
  aoi[is.na(aoi)] <- 0
  
  p1 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(temp_mean_0)) %>%
    as.data.frame()
  
  p2 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cmeant_5d)) %>%
    as.data.frame()
  
  p3 <- aoi %>%
    group_by(month, year) %>%
    summarise(ls = sum(cmeanT_25d)) %>%
    as.data.frame()
  
  
  p1$day <- 1
  p2$day <- 1
  p3$day <- 1
  
  p1$date = as.Date(with(p1,paste(year,month, day, sep="-")), "%Y-%m-%d")
  p2$date = as.Date(with(p2,paste(year,month, day, sep="-")), "%Y-%m-%d")
  p3$date = as.Date(with(p3,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  p1 <- p1[order(as.Date(p1$date, format="%Y-%m-%d")),]
  p2 <- p2[order(as.Date(p2$date, format="%Y-%m-%d")),]
  p3 <- p3[order(as.Date(p3$date, format="%Y-%m-%d")),]
  
  ts_p1<- ts(p1$ls, frequency = 12, start= c(2000, 1))
  ts_p2<- ts(p2$ls, frequency = 12, start= c(2000, 1))
  ts_p3<- ts(p3$ls, frequency = 12, start= c(2000, 1))
  
  decomp_0 <- stl(ts_p1, s.window = 21, t.window = 12)
  decomp_5 <- stl(ts_p2, s.window = 21, t.window = 12)
  decomp_25 <- stl(ts_p3, s.window = 21, t.window = 12)
  
  
  Tt <- trendcycle(decomp_0)
  St <- seasonal(decomp_0)
  Rt <- remainder(decomp_0)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p1 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_5)
  St <- seasonal(decomp_5)
  Rt <- remainder(decomp_5)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p2 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Tt <- trendcycle(decomp_25)
  St <- seasonal(decomp_25)
  Rt <- remainder(decomp_25)
  
  Ft <- max(0,1 - (var(Rt)/var(Tt + Rt)))
  #Seasonal Strength Calculation
  Fs <- max(0,1 - (var(Rt)/var(St + Rt)))
  
  strength_p3 <- data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
  
  Strength_aoi <- rbind(strength_p1, strength_p2, strength_p3)
  
  ##### Kendal
  SK_temp_0 <- SeasonalMannKendall(ts_p1)
  SK_temp_5d <- SeasonalMannKendall(ts_p2)
  SK_temp_25d <- SeasonalMannKendall(ts_p3)
  
  Kendal <- rbind(SK_temp_0, SK_temp_5d, SK_temp_25d)
  res <- cbind(Strength_aoi, Kendal)
  
  return(res)
  
}




Southtyrol <- decomp_landslide_freq(dfT)
Southtyrol

southtyrolrain <- decomp_rain(dfT)
southtyrolrain

southtyroltemp <- decomp_temp(dfT)
southtyroltemp

############# Visual Exploration





                             