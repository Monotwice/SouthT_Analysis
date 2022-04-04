####################
####### EXPLORATORY ANALYSIS

#plots

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

####################################
dfT <- dfT %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(DoY = yday(date))

dfC <- dfC %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(DoY = yday(date))

dfS <- dfS %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(DoY = yday(date))

dfW <- dfW %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(DoY = yday(date))

dfE <- dfE %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(DoY = yday(date))

#############################
### Explor Plots

explor_plots <- function(aoi, area, path.rain, path.freq, path.temp){
  
  ######## Precipitation
  sum.rain <- aoi %>%
    group_by(month, year) %>%
    summarise(sum.precip = sum(rain_mean_0))
  
  plot.sum <- sum.rain %>% 
    mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
    ggplot(aes(x = month2, y = sum.precip)) +
    geom_bar(stat = "identity", fill = "darkorchid4") +
    facet_wrap(~ year, ncol = 7) +
    labs(title = paste0(area, "- Cumulative Monthly Precipitation of entire Area"),
         subtitle = "Data plotted by year",
         y = "Cumulative Precipitation (mm)",
         x = "Month") + theme_bw(base_size = 10)+
          scale_x_date(date_labels = "%b")
  ggsave(plot.sum, file = paste0(path.rain, area, ".png"), width = 10, height = 5)
  
  ######## Frequency
  sum.freq <- aoi %>%
    group_by(month, year) %>%
    summarise(freq.sum = sum(frequency))
  
  plot.freq <- sum.freq %>% 
    mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
    ggplot(aes(x = month2, y = freq.sum)) +
    geom_bar(stat = "identity", fill = "darkorchid4") +
    facet_wrap(~ year, ncol = 7) +
    labs(title = paste0(area,"- Total Monthly Frequency of Landslides of entire Area"),
         subtitle = "Data plotted by year",
         y = "Frequency",
         x = "Month") + theme_bw(base_size = 10)+
    scale_x_date(date_labels = "%b")
  ggsave(plot.freq, file = paste0(path.freq, area, ".png"))
  
  mean.temp <- aoi %>%
    group_by(month, year) %>%
    summarise(mean_precip = mean(temp_mean_0))
  
  plot.temp <- mean.temp %>% 
    mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
    ggplot(aes(x = month2, y = mean_precip)) +
    geom_bar(stat = "identity", fill = "darkorchid4") +
    facet_wrap(~ year, ncol = 7) +
    labs(title = paste0(area,"- Total Monthly Mean Temperature of entire area "),
         subtitle = "Data plotted by year",
         y = "Mean Temperature (°C)",
         x = "Month") + theme_bw(base_size = 10)+
    scale_x_date(date_labels = "%b")
  ggsave(plot.temp, file = paste0(path.temp, area, ".png"))
  
}


dir.rain <- "figures/Precipitation/"
dir.freq <- "figures/Landslide_frequency/"
dir.temp <- "figures/Temperature/"

explor_plots(dfC, "Center South Tyrol", dir.rain, dir.freq, dir.temp)

explor_plots(dfS, "South South Tyrol", dir.rain, dir.freq, dir.temp)
explor_plots(dfW, "West South Tyrol", dir.rain, dir.freq, dir.temp)
explor_plots(dfE, "East South Tyrol", dir.rain, dir.freq, dir.temp)
explor_plots(dfT, "South Tyrol", dir.rain, dir.freq, dir.temp)


#############################
### Prepetory Boxplots

prep.cond.box <- function(aoi, area, path.box){
  
  rain0 <<- aoi %>%
    group_by(month, year) %>%
    summarise(rain0.sum = sum(rain_mean_0)) %>%
    as.data.frame()
  
  rain5 <<- aoi %>%
    group_by(month, year) %>%
    summarise(rain5.sum = sum(cmeanR_5d)) %>%
    as.data.frame()
  
  rain25 <<- aoi %>%
    group_by(month, year) %>%
    summarise(rain25.sum = sum(cmeanR_25d)) %>%
    as.data.frame()
  
  rain0$day <- 1
  rain5$day <- 1
  rain25$day <- 1
  
  rain0$date = as.Date(with(rain0,paste(year,month, day, sep="-")), "%Y-%m-%d")
  rain5$date = as.Date(with(rain5,paste(year,month, day, sep="-")), "%Y-%m-%d")
  rain25$date = as.Date(with(rain25,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  rain0 <- rain0[order(as.Date(rain0$date, format="%Y-%m-%d")),]
  rain5 <- rain5[order(as.Date(rain5$date, format="%Y-%m-%d")),]
  rain25 <- rain25[order(as.Date(rain25$date, format="%Y-%m-%d")),]
  
  
  ts_rain0<- ts(rain0$rain0.sum, frequency = 12, start= c(2000, 1))
  ts_rain5<- ts(rain5$rain5.sum, frequency = 12, start= c(2000, 1))
  ts_rain25<<- ts(rain25$rain25.sum, frequency = 12, start= c(2000, 1))
  
  png(paste0(path.box,"rain0 -", area, ".png"))
  seasplot(ts_rain0, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Cumulative Precipitation"), 
           ylab= "Cumulative Precipitation (mm)",
            xlab = "Month of the Year")
  dev.off()
  
  png(paste0(path.box,"rain5 -", area, ".png"))
  seasplot(ts_rain5, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Cumulative Precipitation of five Days"), 
           ylab= "Cumulative Precipitation (mm)",
           xlab = "Month of the Year")
  dev.off()
  
  png(paste0(path.box,"rain25 - ", area, ".png"))
  seasplot(ts_rain25, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Cumulative Precipitation of 25 Days"), 
           ylab= "Cumulative Precipitation (mm)",
           xlab = "Month of the Year")
  dev.off()
  
  ######################################################################
  
  temp0 <<- aoi %>%
    group_by(month, year) %>%
    summarise(temp0.sum = mean(temp_mean_0)) %>%
    as.data.frame()
  
  temp5 <<- aoi %>%
    group_by(month, year) %>%
    summarise(temp5.sum = mean(cmeant_5d)) %>%
    as.data.frame()%>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  temp25 <<- aoi %>%
    group_by(month, year) %>%
    summarise(temp25.sum = mean(cmeanT_25d)) %>%
    as.data.frame()%>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  temp0$day <- 1
  temp5$day <- 1
  temp25$day <- 1
  
  temp0$date = as.Date(with(temp0,paste(year,month, day, sep="-")), "%Y-%m-%d")
  temp5$date = as.Date(with(temp5,paste(year,month, day, sep="-")), "%Y-%m-%d")
  temp25$date = as.Date(with(temp25,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  temp0 <- temp0[order(as.Date(temp0$date, format="%Y-%m-%d")),]
  temp5 <- temp5[order(as.Date(temp5$date, format="%Y-%m-%d")),]
  temp25 <- temp25[order(as.Date(temp25$date, format="%Y-%m-%d")),]
  
  
  ts_temp0<- ts(temp0$temp0.sum, frequency = 12, start= c(2000, 1))
  ts_temp5<- ts(temp5$temp5.sum, frequency = 12, start= c(2000, 1))
  ts_temp25<<- ts(temp25$temp25.sum, frequency = 12, start= c(2000, 1))
  
  png(paste0(path.box,"temp0 -", area, ".png"))
  seasplot(ts_temp0, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Mean Temperature"), 
           ylab= "Mean Temperature (°C)",
           xlab = "Month of the Year")
  dev.off()
  
  png(paste0(path.box,"temp5 -", area, ".png"))
  seasplot(ts_temp5, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Mean cumulative Temperature of five Days"), 
           ylab= "Mean Temperature (°C)",
           xlab = "Month of the Year")
  dev.off()
  
  png(paste0(path.box,"temp25 - ", area, ".png"))
  seasplot(ts_temp25, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Mean cumulative Temperature of 25 Days"), 
           ylab= "Mean Temperature (°C)",
           xlab = "Month of the Year")
  dev.off()
}

dir.box <- "figures/Boxplots/"

prep.cond.box(dfC, "Center South Tyrol", dir.box)
prep.cond.box(dfS, "South South Tyrol", dir.box)























