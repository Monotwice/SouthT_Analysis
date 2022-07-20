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
    labs(title = paste0(area, " - Monthly Precipitation"),
         subtitle = "Data plotted by year",
         y = "Precipitation (mm)",
         x = "Month") + theme_bw(base_size = 10)+
          scale_x_date(date_labels = "%b")+
    ylim(0,300)
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
    labs(title = paste0(area," - Monthly Frequency of Landslides"),
         subtitle = "Data plotted by year",
         y = "Frequency",
         x = "Month") + theme_bw(base_size = 10)+
    scale_x_date(date_labels = "%b")
  ggsave(plot.freq, file = paste0(path.freq, area, ".png"), width = 10, height = 5)
  
  mean.temp <- aoi %>%
    group_by(month, year) %>%
    summarise(mean_precip = mean(temp_mean_0))
  
  plot.temp <- mean.temp %>% 
    mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
    ggplot(aes(x = month2, y = mean_precip)) +
    geom_bar(stat = "identity", fill = "darkorchid4") +
    facet_wrap(~ year, ncol = 7) +
    labs(title = paste0(area," - Monthly Mean Temperature"),
         subtitle = "Data plotted by year",
         y = "Mean Temperature (°C)",
         x = "Month") + theme_bw(base_size = 10)+
    scale_x_date(date_labels = "%b")+
    ylim(-5, 20)
  ggsave(plot.temp, file = paste0(path.temp, area, ".png"), width = 10, height = 5)
  
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
  
  ls <- aoi %>%
    group_by(month, year) %>%
    summarise(ls.sum = sum(frequency)) %>%
    as.data.frame()
  
  
  rain0$day <- 1
  ls$day <- 1
 
  
  rain0$date = as.Date(with(rain0,paste(year,month, day, sep="-")), "%Y-%m-%d")
  ls$date = as.Date(with(ls,paste(year,month, day, sep="-")), "%Y-%m-%d")
 
  
  rain0 <- rain0[order(as.Date(rain0$date, format="%Y-%m-%d")),]
  ls <- ls[order(as.Date(ls$date, format="%Y-%m-%d")),]
 
  
  
  ts_rain0<- ts(rain0$rain0.sum, frequency = 12, start= c(2000, 1))
  ts_ls<- ts(ls$ls.sum, frequency = 12, start= c(2000, 1))

  
  png(paste0(path.box,"rain0 -", area, ".png"))
  seasplot(ts_rain0, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Monthly Precipitation"), 
           ylab= "Cumulative Precipitation (mm)",
            xlab = "Month of the Year")
  dev.off()
  
  png(paste0(path.box,"landslides -", area, ".png"))
  seasplot(ts_ls, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Frequency of Landslides "), 
           ylab= "Frequency",
           xlab = "Month of the Year")
  dev.off()
  
  
  ######################################################################
  
  temp0 <<- aoi %>%
    group_by(month, year) %>%
    summarise(temp0.sum = mean(temp_mean_0)) %>%
    as.data.frame()
  
  temp0$day <- 1
  
  temp0$date = as.Date(with(temp0,paste(year,month, day, sep="-")), "%Y-%m-%d")
  
  temp0 <- temp0[order(as.Date(temp0$date, format="%Y-%m-%d")),]
 
  ts_temp0<- ts(temp0$temp0.sum, frequency = 12, start= c(2000, 1))
  
  
  png(paste0(path.box,"temp0 -", area, ".png"))
  seasplot(ts_temp0, outplot = 2, trend = FALSE, 
           main =  paste0(area, " - Mean Temperature"), 
           ylab= "Mean Temperature (°C)",
           xlab = "Month of the Year")
  dev.off()
  
  #########################################################################
  
  
  ls$ls.sum <- ls$ls.sum + 1
  ls$ls.sum <- log(ls$ls.sum)
  
  rain0$rain0.sum <- rain0$rain0.sum +1 
  rain0$rain0.sum <- log(rain0$rain0.sum)
  
  
  png(paste0(path.box,"scatter -", area, ".png"))
  plot(rain0$rain0.sum, ls$ls.sum)
  dev.off()
  
  
}

dir.box <- "figures/Boxplots/"

prep.cond.box(dfC, "Center South Tyrol", dir.box)
prep.cond.box(dfS, "South South Tyrol", dir.box)
prep.cond.box(dfE, "East South Tyrol", dir.box)
prep.cond.box(dfW, "West South Tyrol", dir.box)
prep.cond.box(dfT, "South Tyrol", dir.box)


###########################






















